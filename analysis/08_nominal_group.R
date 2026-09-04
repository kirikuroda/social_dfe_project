# =============================================================================
# Nominal-group control analysis (Stan/cmdstanr)
# -----------------------------------------------------------------------------
# Builds 33 NOMINAL groups of 5 from the SOLO condition and fits group_full.stan
# to them. Solo participants never saw social information, so a nominal group is
# a counterfactual: its members are stitched together after the fact, and each
# member's social evidence is what they WOULD have seen had those four people
# been their group-mates. Any social effect recovered here is therefore an
# artefact of the reconstruction (shared choice problems, shared response
# tendencies), and the posterior of the social parameters
# (beta_social_strength, beta_social_intercept, beta_confirm,
# eta_intercept / eta_strength / eta_social_strength, theta) serves as a
# baseline against which fit_group_full (real, interacting groups) is read.
#
# Design mirrors the real group condition as closely as possible:
#   * 33 groups x 5 members = 165 member slots, drawn from the 164 solo
#     participants WITH REPLACEMENT (see `allow_within_group_duplicates`).
#     Each slot is a distinct pseudo-subject in Stan (subj_id 1..165), so a solo
#     participant drawn twice contributes two identical trial sets that are free
#     to load on different group-level effects, exactly as a bootstrap resample.
#   * Members are aligned by option_id, not by trial_number: every solo
#     participant saw all 36 non-attention-check options, but in an
#     individually randomised order, whereas real group-mates faced them
#     synchronously. Aligning on option_id keeps social information about the
#     SAME choice problem; the focal's own trial_num is left on their own
#     (solo) trial order.
#   * Social information follows the same strictly-fewer-samples rule as
#     02_modeling.R: a group-mate who stopped at m samples is revealed to the
#     focal at the focal's sample step m, so the fewest-sample member of each
#     nominal group x option has no social information and members who tie see
#     nothing of each other.
#
# The standardization constants (k, t_offset, beta_scale, social_scale,
# outcome_mean / outcome_sd) are NOT recomputed: they are read from
# output/fit/group_scales.rds, written by 02_modeling.R and pooled over the solo
# and group conditions. Reusing them is what makes the nominal-group posterior
# directly comparable, coefficient by coefficient, with fit_solo and
# fit_group_full. Run 02_modeling.R first.
#
# Three models are fitted to the same nominal-group data set, so that LOO can
# be compared among them (unlike a comparison with the real group condition,
# which is a different data set):
#   * solo.stan                 no social information anywhere (asocial null)
#   * group_asocial_choice.stan social information in the sampling / stopping
#                               stages, but the consequential choice is purely
#                               asocial (pi_copy = 0)
#   * group_full.stan           social information in all stages
# Each uses the same sampler settings as its counterpart in 02_modeling.R, so
# the nominal and real posteriors differ only in the data they were fitted to.
#
# Cache: delete the corresponding output/fit/fit_nominal_*.rds to re-run a fit.
# =============================================================================

library(tidyverse)
library(here)
library(cmdstanr)
library(magrittr)
library(loo)
library(tidybayes)
load(here("data/data.rda"))


# Settings ---------------------------------------------------------------------

seed_nominal     <- 1   # RNG seed for the membership draw (and for Stan)
n_nominal_groups <- 33  # matches the 33 real groups
group_size       <- 5   # matches the intended group size

# TRUE  = one plain with-replacement draw of 165 slots from the 164 solo
#         participants, so a participant can occupy two slots in the SAME
#         nominal group. Two slots held by the same participant always stop at
#         the same sample count, and the strictly-fewer rule means neither ever
#         becomes the other's social information; they simply share the trials.
# FALSE = 5 distinct participants per group (with replacement ACROSS groups).
allow_within_group_duplicates <- TRUE


# Pooled standardization constants from 02_modeling.R --------------------------

scales_path <- here("output/fit/group_scales.rds")
if (!file.exists(scales_path)) {
  stop("output/fit/group_scales.rds not found; run analysis/02_modeling.R first.")
}
scales <- readRDS(scales_path)


# Nominal group membership -----------------------------------------------------

solo_subjects <- df_trials |>
  filter(condition == "solo") |>
  distinct(subject_id) |>
  arrange(subject_id) |>
  pull(subject_id)

n_slots <- n_nominal_groups * group_size

set.seed(seed_nominal)
sampled_subjects <- if (allow_within_group_duplicates) {
  sample(solo_subjects, n_slots, replace = TRUE)
} else {
  # With replacement across groups, without replacement within a group.
  map(seq_len(n_nominal_groups),
    \(g) sample(solo_subjects, group_size, replace = FALSE)) |>
    list_c()
}

# pseudo_id is the Stan subject index; rows are already ordered by nominal
# group, so group_id (per subject) is this table's nominal_group column.
df_membership <- tibble(
  nominal_group = rep(seq_len(n_nominal_groups), each = group_size),
  member        = rep(seq_len(group_size), times = n_nominal_groups),
  subject_id    = sampled_subjects
) |>
  mutate(pseudo_id = row_number())


# Trial-level frame ------------------------------------------------------------
# One row per (pseudo-subject, trial), ordered by pseudo_id then the
# participant's own trial_number; trial_id is the Stan trial index.

df_trials_nominal <- df_membership |>
  left_join(
    df_trials |> filter(condition == "solo"),
    by            = "subject_id",
    relationship  = "many-to-many"
  ) |>
  arrange(pseudo_id, trial_number) |>
  mutate(trial_id = row_number())


# Reconstructed social information ---------------------------------------------
# Same construction as df_social_info in 02_modeling.R, with nominal_group in
# place of group_id and pseudo_id in place of subject_id. A group-mate who
# stopped at other_n < n_samples_self samples is revealed at sample_id ==
# other_n. choice_self: 0 = chose A, 1 = chose B.

df_social_info_nominal <- df_trials_nominal |>
  select(nominal_group, option_id, pseudo_id, n_samples_self, choice_self) |>
  inner_join(
    df_trials_nominal |>
      select(nominal_group, option_id,
        other_pseudo = pseudo_id,
        other_n      = n_samples_self,
        other_choice = choice_self),
    by           = c("nominal_group", "option_id"),
    relationship = "many-to-many"
  ) |>
  filter(other_pseudo != pseudo_id, other_n < n_samples_self) |>
  group_by(pseudo_id, option_id, sample_id = other_n) |>
  summarise(
    a_chosen = sum(other_choice == 0),
    b_chosen = sum(other_choice == 1),
    .groups  = "drop"
  )


# Step-level frame -------------------------------------------------------------

df_sampling_nominal <- df_trials_nominal |>
  select(pseudo_id, trial_id, subject_id, option_id) |>
  left_join(
    df_sampling |>
      filter(condition == "solo") |>
      select(subject_id, option_id, sample_id, sampled_option,
        sampled_option_label, sampled_outcome),
    by           = c("subject_id", "option_id"),
    relationship = "many-to-many"
  ) |>
  arrange(trial_id, sample_id) |>
  left_join(df_options, by = "option_id") |>
  mutate(is_higher_outcome = case_when(
    sampled_option_label == "a" & sampled_outcome == v_a_high ~ 1,
    sampled_option_label == "a" & sampled_outcome == v_a_low  ~ 0,
    sampled_option_label == "b" & sampled_outcome == v_b_high ~ 1,
    sampled_option_label == "b" & sampled_outcome == v_b_low  ~ 0
  )) |>
  left_join(
    df_social_info_nominal,
    by = c("pseudo_id", "option_id", "sample_id")
  ) |>
  replace_na(list(a_chosen = 0, b_chosen = 0)) |>
  mutate(across(c(a_chosen, b_chosen), as.numeric))

# 1-based start index of each trial's segments (length N + 1 with the sentinel).
seg_starts_nominal <- df_sampling_nominal |>
  mutate(segment = row_number()) |>
  group_by(trial_id) |>
  summarise(min_segment = min(segment)) |>
  pull(min_segment)


# Stan data --------------------------------------------------------------------

stan_data_nominal <- df_trials_nominal %$%
  list(
    N                 = nrow(.),
    N_subj            = n_slots,
    N_group           = n_nominal_groups,
    N_seg             = nrow(df_sampling_nominal),
    option_sampled    = df_sampling_nominal$sampled_option,
    v_sampled         = df_sampling_nominal$sampled_outcome,
    v_sampled_z       = (df_sampling_nominal$sampled_outcome -
      scales$outcome_mean) / scales$outcome_sd,
    is_higher_outcome = df_sampling_nominal$is_higher_outcome,
    choice            = choice_self,
    subj_id           = pseudo_id,
    group_id          = df_membership$nominal_group,
    seg_offsets       = c(seg_starts_nominal, nrow(df_sampling_nominal) + 1),
    a_chosen          = df_sampling_nominal$a_chosen,
    b_chosen          = df_sampling_nominal$b_chosen,
    trial_num         = scale(trial_number)[, 1],
    t_offset          = scales$t_offset,
    k                 = scales$k,
    beta_scale        = as.numeric(scales$beta_scale),
    social_scale      = as.numeric(scales$social_scale)
  )

# Sanity checks on the reconstruction.
stopifnot(
  length(seg_starts_nominal) == nrow(df_trials_nominal),
  !is.unsorted(stan_data_nominal$subj_id),
  all(diff(stan_data_nominal$seg_offsets) > 0),
  !anyNA(df_sampling_nominal$is_higher_outcome),
  # Every trial's segment count equals the recorded sample size.
  all(diff(stan_data_nominal$seg_offsets) == df_trials_nominal$n_samples_self),
  # No member is their own social information.
  nrow(df_social_info_nominal) ==
    nrow(distinct(df_social_info_nominal, pseudo_id, option_id, sample_id))
)

# solo.stan declares no group level and no social information, so it takes the
# same list minus N_group / group_id / a_chosen / b_chosen / social_scale. It is
# the asocial null for the LOO comparison below: the identical trials, scored by
# a model that cannot see the reconstructed social information at all.
stan_data_nominal_solo <- stan_data_nominal[
  setdiff(names(stan_data_nominal),
    c("N_group", "group_id", "a_chosen", "b_chosen", "social_scale"))
]


# Keep the membership draw so the fit can be reproduced / traced back.
dir.create(here("output/fit"), recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    membership                    = df_membership,
    seed                          = seed_nominal,
    allow_within_group_duplicates = allow_within_group_duplicates
  ),
  here("output/fit/nominal_group_membership.rds")
)


# Models fitted to the nominal groups (Stan/cmdstanr) --------------------------
# Sampler settings per model match 02_modeling.R (solo.stan runs on the sampler
# defaults there; both group models use max_treedepth = 12, with adapt_delta
# 0.9 for group_asocial_choice and 0.95 for group_full), so each nominal fit
# differs from its real-data counterpart only in the data.

fit_nominal_model <- function(stan_file, data, path,
                              adapt_delta = NULL, max_treedepth = NULL) {
  if (file.exists(path)) return(readRDS(path))

  model <- cmdstan_model(
    here(stan_file),
    cpp_options = list(stan_threads = TRUE)
  )

  args <- list(
    data              = data,
    seed              = seed_nominal,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100
  )
  if (!is.null(adapt_delta))   args$adapt_delta   <- adapt_delta
  if (!is.null(max_treedepth)) args$max_treedepth <- max_treedepth

  fit <- do.call(model$sample, args)
  fit$save_object(path)
  fit
}

# Asocial null.
fit_nominal_solo <- fit_nominal_model(
  "function/Stan/solo.stan",
  stan_data_nominal_solo,
  here("output/fit/fit_nominal_solo.rds")
)

# Social information in sampling / stopping only; asocial consequential choice.
fit_nominal_group_asocial_choice <- fit_nominal_model(
  "function/Stan/group_asocial_choice.stan",
  stan_data_nominal,
  here("output/fit/fit_nominal_group_asocial_choice.rds"),
  adapt_delta   = 0.9,
  max_treedepth = 12
)

# Social information in all stages.
fit_nominal_group_full <- fit_nominal_model(
  "function/Stan/group_full.stan",
  stan_data_nominal,
  here("output/fit/fit_nominal_group_full.rds"),
  adapt_delta   = 0.95,
  max_treedepth = 15
)


# LOO-CV: model comparison within the nominal-group data ----------------------
# All three fits score the same 5,940 trials with a per-trial log_lik covering
# the same three stages, so they are directly comparable. If the reconstructed
# social information carries no signal, the two group models should not beat
# the asocial null here.

loo_nominal_solo                 <- fit_nominal_solo$loo()
loo_nominal_group_asocial_choice <- fit_nominal_group_asocial_choice$loo()
loo_nominal_group_full           <- fit_nominal_group_full$loo()

loo_compare(
  loo_nominal_solo,
  loo_nominal_group_asocial_choice,
  loo_nominal_group_full
)

save(
  loo_nominal_solo,
  loo_nominal_group_asocial_choice,
  loo_nominal_group_full,
  file = here("output/fit/loo_nominal.rda")
)


# Population-level social parameters: nominal vs real groups -------------------
# LOO is not comparable across the two datasets, so the comparison is on the
# posteriors of mu. Row indices follow the group_full.stan header:
#   8  beta_social_strength   9  beta_social_intercept  10 beta_confirm
#  18  eta_intercept         19  eta_strength           20 eta_social_strength
#  21  theta

pop_draws_nominal_group_full <- spread_draws(fit_nominal_group_full,
  mu[i], sigma[i])
saveRDS(
  pop_draws_nominal_group_full,
  here("output/fit/pop_draws_nominal_group_full.rds")
)

social_rows <- c(8, 9, 10, 18, 19, 20, 21)

pop_draws_real_path <- here("output/fit/pop_draws_group_full.rds")
if (file.exists(pop_draws_real_path)) {
  bind_rows(
    pop_draws_nominal_group_full |> mutate(groups = "nominal"),
    readRDS(pop_draws_real_path) |> mutate(groups = "real")
  ) |>
    filter(i %in% social_rows) |>
    group_by(groups, i) |>
    summarise(
      mean   = mean(mu),
      lower  = quantile(mu, 0.025),
      upper  = quantile(mu, 0.975),
      p_pos  = mean(mu > 0),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = groups, values_from = c(mean, lower, upper, p_pos)) |>
    print(n = Inf)
}
