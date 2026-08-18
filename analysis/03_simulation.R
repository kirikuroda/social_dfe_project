# Simulation of DFE models

library(tidyverse)
library(tidybayes)
library(furrr)
library(progressr)
library(here)
library(magrittr)
source(here("function/R/utils.R"))
source(here("function/R/simulate.R"))

load(here("data/data.rda"))

# Benchmark stepwise simulation ------------------------------------------
# No stopping rule, alternating sampling, inverse temperature fixed to 1 (see
# simulate_benchmark). P(choose B) is recorded for n = 1..40 samples. Always
# recomputed (no cache); the result is still saved for downstream use.

sim_benchmark_path <- here("output/sim/sim_benchmark.rds")

n_runs <- 10000
set.seed(1)
sim_benchmark <- df_options |>
  filter(trial_type != "attention_check") |>
  pmap_dfr(function(option_id, v_a_high, v_a_low, v_b_high, v_b_low,
                    p_a_high, p_a_low, p_b_high, p_b_low, trial_type, ...) {
    simulate_benchmark(
      n_trials          = n_runs,
      max_samples       = 40L,
      sampling_strategy = "alternating",
      v_A_high          = v_a_high,
      v_A_low           = v_a_low,
      v_B_high          = v_b_high,
      v_B_low           = v_b_low,
      p_A_high          = p_a_high,
      p_B_high          = p_b_high,
      # risk attitude: power-utility exponent (1 = risk-neutral)
      utility_pow       = 1
    ) |>
      mutate(
        option_id    = option_id,
        trial_type   = trial_type,
        rare_outcome = if_else(p_a_high == 0.1, "large", "small")
      )
  }, .progress = TRUE) |>
  group_by(option_id, trial_type, rare_outcome, step) |>
  summarise(p_choice_B = mean(p_choice_B_step), .groups = "drop")

dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
saveRDS(sim_benchmark, sim_benchmark_path)


# Cascade analysis: bootstrap approach ------------------------------------
# Bootstrap N_boot times, drawing grp_sz participants from each condition and
# assigning choice_order within each pseudo-group x option. Member-level rows
# are kept here; choice rates by choice_order x rare_outcome x trial_type are
# aggregated downstream in 05_visualization.R.
#
# Three series (only the two virtual ones are bootstrapped; real_group is the
# observed data, entered once):
#   real_group    : observed groups (real social structure)
#   virtual_group : grp_sz random group-condition participants (controls for
#                   individual-level preference shifts due to social info)
#   virtual_solo  : grp_sz random solo-condition participants (no social info)
#
# Gaps:
#   virtual_solo  → virtual_group : individual-level effect of social info
#   virtual_group → real_group    : extra cascade from actual group coordination

df_cascade_path <- here("output/sim/df_cascade.rds")

if (file.exists(df_cascade_path)) {
  df_cascade <- readRDS(df_cascade_path)
} else {
  N_boot <- 1000
  grp_sz <- 5

  df_solo  <- df_trials |> filter(condition == "solo")
  df_group <- df_trials |> filter(condition == "group")

  # --- Real groups: choice order within each group x option -----------------
  df_cascade_real_group <- df_group |>
    group_by(group_id, option_id) |>
    mutate(choice_order = rank(n_samples_self, ties.method = "min")) |>
    ungroup() |>
    mutate(condition = "real_group")

  # --- Bootstrap helper -----------------------------------------------------
  bootstrap_virtual <- function(df, subjects, label, seed) {
    plan(multisession, workers = 80)
    out <- with_progress({
      p <- progressor(along = seq_len(N_boot))
      future_map_dfr(seq_len(N_boot), \(b) {
        p()
        df |>
          inner_join(slice_sample(subjects, n = grp_sz), by = "subject_id") |>
          group_by(option_id) |>
          mutate(choice_order = rank(n_samples_self, ties.method = "min")) |>
          ungroup() |>
          mutate(boot = b, condition = label)
      }, .options = furrr_options(seed = seed))
    })
    plan(sequential)
    out
  }

  df_cascade_virtual_solo  <- bootstrap_virtual(
    df_solo,  df_solo  |> distinct(subject_id), "virtual_solo",  seed = 1
  )
  df_cascade_virtual_group <- bootstrap_virtual(
    df_group, df_group |> distinct(subject_id), "virtual_group", seed = 2
  )

  df_cascade <- bind_rows(
    df_cascade_real_group,
    df_cascade_virtual_solo,
    df_cascade_virtual_group
  ) |>
    mutate(condition = factor(
      condition,
      levels = c("virtual_solo", "virtual_group", "real_group")
    ))

  saveRDS(df_cascade, df_cascade_path)
}


# Cascade solo benchmark: a single 33-group realization -----------------------
# The virtual_solo series in df_cascade pools N_boot = 1000 five-member virtual
# groups, so its choice-order curve is far smoother (effective n = 1000) than
# the observed real_group series (only 33 groups). For an apples-to-apples
# asocial benchmark, build ONE set of 33 virtual solo groups matching the real
# design: sum(real_group_sizes) = 148 distinct solo subjects are drawn WITHOUT
# replacement (seed fixed) and partitioned into 33 groups whose sizes exactly
# match the real groups (17 of 4 + 16 of 5). Within each virtual group x option,
# choice_order = rank(n_samples_self), as for the real groups. The RAW
# member-level choices are kept so 05 can plot solo with the SAME
# stat_summary(mean +/- SE) as the real groups -> solo and group are then
# computed identically (single 33-group set, within-observation SE).
# Cache: delete output/sim/df_cascade_solo33.rds to re-run.

df_cascade_solo33_path <- here("output/sim/df_cascade_solo33.rds")

if (file.exists(df_cascade_solo33_path)) {
  df_cascade_solo33 <- readRDS(df_cascade_solo33_path)
} else {
  # Sizes of the 33 real groups (4 or 5); the virtual solo groups reproduce this
  # exact composition so the choice_order support matches the observed design.
  real_group_sizes <- df_trials |>
    filter(condition == "group") |>
    distinct(group_id, subject_id) |>
    count(group_id, name = "gsz") |>
    pull(gsz)

  # Fixed membership template: virtual group id (1..33) for each of the 148
  # slots.
  member_group <- rep(seq_along(real_group_sizes), real_group_sizes)

  solo_subjects <- df_trials |>
    filter(condition == "solo") |>
    distinct(subject_id) |>
    pull(subject_id)

  # One arbitrary (seeded) assignment of 148 distinct solo subjects to the 33
  # virtual groups by the size template.
  set.seed(1)
  assign_tbl <- tibble(
    subject_id = sample(solo_subjects, length(member_group), replace = FALSE),
    group_id   = member_group
  )

  df_cascade_solo33 <- df_trials |>
    filter(condition == "solo",
      !is.na(choice_self), !is.na(n_samples_self)) |>
    select(subject_id, option_id, trial_type, rare_outcome,
      n_samples_self, choice_self) |>
    inner_join(assign_tbl, by = "subject_id") |>
    group_by(group_id, option_id) |>
    mutate(choice_order = rank(n_samples_self, ties.method = "min")) |>
    ungroup() |>
    mutate(condition = "virtual_solo") |>
    select(condition, group_id, option_id, trial_type, rare_outcome,
      choice_order, n_samples_self, choice_self)

  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df_cascade_solo33, df_cascade_solo33_path)
}


# =============================================================================
# group_full.stan: generative simulation + generic parameter sweep
# -----------------------------------------------------------------------------
# Faithful to function/Stan/group_full.stan via simulate_group_full(), using:
#   - fit_group_full.rds (21 named subject-level parameters)
#   - the exact unit-SD standardization constants from group_scales.rds
# sim_sweep_full() can manipulate ANY of the 21 parameters: pass the parameter
# name and a grid of values; the chosen parameter is overridden for every
# member while all other parameters are drawn from each member's POSTERIOR (one
# draw per group, shared across members).
# =============================================================================

# Stan-name -> simulate_group_full() argument (identical names; one source
# list).
group_full_param_names <- c(
  "beta_intercept", "beta_step", "beta_trial", "beta_outcome",
  "beta_extremity", "beta_both_seen", "beta_strength",
  "beta_social_strength", "beta_social_intercept", "beta_confirm",
  "gamma_intercept", "gamma_value", "gamma_estim", "gamma_discover",
  "gamma_switch", "gamma_social",
  "tau_mean", "eta_intercept", "eta_strength", "eta_social_strength", "theta"
)

# Validate/normalize a `fixed` parameter-override vector (named numeric; clamps
# the listed parameters to constants for every member, applied ON TOP of the
# posterior draws). Returns a plain named numeric vector, or NULL for "no
# override" (pure posterior draws). `allowed` is the set of legal parameter
# names for the model being simulated (group_full_param_names or
# solo_param_names).
validate_fixed <- function(fixed, allowed) {
  if (is.null(fixed)) return(NULL)
  fixed <- unlist(as.list(fixed))
  if (!is.numeric(fixed) || is.null(names(fixed)) || any(names(fixed) == ""))
    stop("`fixed` must be a fully named numeric vector/list.")
  bad <- setdiff(names(fixed), allowed)
  if (length(bad))
    stop("Unknown parameter(s) in `fixed`: ", paste(bad, collapse = ", "))
  fixed
}

# Pooled standardization constants used when fitting group_full.stan.
group_scales <- readRDS(here("output/fit/group_scales.rds"))

fit_group_full <- readRDS(here("output/fit/fit_group_full.rds"))

# Helper: run simulate_group_full() for one group (gdf has the 21 param columns
# plus `subj`) facing one option, programmatically forwarding all 21 parameters.
run_group_full <- function(gdf, o, group_size, n_trials, trial_num = 0) {
  do.call(
    simulate_group_full,
    c(
      list(
        group_size   = group_size,
        n_trials     = n_trials,
        subject_id   = gdf$subj,
        beta_scale   = group_scales$beta_scale,
        social_scale = group_scales$social_scale,
        t_offset     = group_scales$t_offset,
        k            = group_scales$k,
        outcome_mean = group_scales$outcome_mean,
        outcome_sd   = group_scales$outcome_sd,
        trial_num    = trial_num,
        v_A_high     = o$v_a_high, v_A_low = o$v_a_low,
        v_B_high     = o$v_b_high, v_B_low = o$v_b_low,
        p_A_high     = o$p_a_high, p_B_high = o$p_b_high
      ),
      setNames(
        lapply(group_full_param_names, \(p) gdf[[p]]), group_full_param_names
      )
    )
  )
}

# Options faced by every simulated group (non-attention-check).
opts_full <- df_options |>
  filter(trial_type != "attention_check") |>
  mutate(rare_outcome = if_else(p_a_high == 0.1, "large", "small"))

# Per-subject posterior draws of all 21 parameters (one row per subj x .draw).
# Shared by the sweeps (non-manipulated parameters), the posterior-draw
# simulations, and the 33-group noise check below.
draws_full <- fit_group_full |>
  spread_draws(
    beta_intercept[subj], beta_step[subj], beta_trial[subj], beta_outcome[subj],
    beta_extremity[subj], beta_both_seen[subj], beta_strength[subj],
    beta_social_strength[subj], beta_social_intercept[subj], beta_confirm[subj],
    gamma_intercept[subj], gamma_value[subj], gamma_estim[subj],
    gamma_discover[subj], gamma_switch[subj], gamma_social[subj],
    tau_mean[subj], eta_intercept[subj], eta_strength[subj],
    eta_social_strength[subj], theta[subj]
  ) |>
  ungroup() |>
  select(subj, .draw, all_of(group_full_param_names))

# Split into a per-subject list, keyed by .draw (rownames) for O(1) lookup of a
# given posterior draw within each subject.
draws_by_subj <- split(draws_full[, c("subj", ".draw", group_full_param_names)],
  draws_full$subj)
draws_by_subj <- lapply(draws_by_subj, \(d) {
  rownames(d) <- as.character(d$.draw)
  d
})
draw_ids_full <- unique(draws_full$.draw)

# Pre-sample N_groups x group_size members in the MAIN process. ONE posterior
# draw is chosen PER GROUP (draw id shared by all members), so a group is a
# single coherent posterior sample of the whole hierarchy and across-group
# variation reflects posterior uncertainty (+ group composition). Subjects are
# drawn with replacement; all 21 params come jointly from the group's draw. Only
# this compact table (N_groups * group_size rows) is shipped to workers, NOT the
# full draws object, so memory stays small even with 80 workers.
make_post_groups <- function(draws_by_subj, draw_ids, N_groups, group_size,
                             seed) {
  set.seed(seed)
  n_subj <- length(draws_by_subj)
  map_dfr(seq_len(N_groups), \(grp_i) {
    did    <- sample(draw_ids, 1L)            # one posterior draw per GROUP
    chosen <- sample(n_subj, group_size, replace = TRUE)
    map_dfr(seq_along(chosen), \(m) {
      d   <- draws_by_subj[[chosen[m]]]
      # same draw id for all members
      row <- d[as.character(did), , drop = FALSE]
      row$group_id        <- grp_i
      row$member_in_group <- m
      row$draw_used        <- did
      row
    })
  })
}

# Generic parameter sweep over group_full.stan ---------------------------------
# param      : name of ONE of the 21 parameters to manipulate (see
#              group_full_param_names)
# sweep_vals : grid of values to assign to that parameter
# jitter_sd  : if non-NULL, each member's value is drawn ~ Normal(val,
#              jitter_sd) instead of all members sharing the same val (set to
#              the posterior median of the corresponding sigma row for
#              realistic heterogeneity)
# fixed      : optional named numeric vector/list of OTHER parameters to clamp
#              a constant for every member, e.g. fixed = c(eta_strength = 0) to
#              sweep one parameter with another switched off. Cannot include
#              `param`.
# N_groups   : number of virtual groups (outer parallel loop)
# group_size : members per group
# n_trials   : trials per group per option
# level      : output granularity
#   "summary" (default) -> one row per (sweep_val, choice_order, trial_type,
#       rare_outcome): mean choice rate + cluster-robust SE, treating each group
#       as the resampling unit (members within a group are NOT independent under
#       a cascade, so a binomial SE would understate uncertainty). ~220 rows.
#   "group"   -> per-group cell means (the input to the SE above). Keeps the
#       group dimension for custom aggregation; ~N_groups * 220 rows.
#   "raw"     -> every member-level decision (LARGE: N_groups * |sweep_vals| *
#       n_options * group_size rows, e.g. ~2M rows at defaults). Use only when
#       the per-member columns are needed.
sim_sweep_full <- function(
  param,
  sweep_vals,
  jitter_sd  = NULL,
  fixed      = NULL,
  N_groups   = 1000L,
  group_size = 5L,
  n_trials   = 1L,
  level      = c("summary", "group", "raw"),
  workers    = 80L,
  seed       = 1
) {
  param <- match.arg(param, group_full_param_names)
  level <- match.arg(level)

  # `fixed`: named numeric vector/list of parameters to CLAMP to a constant for
  # every member (e.g. c(eta_strength = 0)), applied on top of the group's
  # posterior draw. The swept parameter cannot also be fixed.
  if (!is.null(fixed)) {
    fixed <- unlist(as.list(fixed))
    if (is.null(names(fixed)) || any(names(fixed) == ""))
      stop("`fixed` must be a fully named numeric vector/list.")
    bad <- setdiff(names(fixed), group_full_param_names)
    if (length(bad))
      stop("Unknown parameter(s) in `fixed`: ", paste(bad, collapse = ", "))
    if (param %in% names(fixed))
      stop("The swept parameter (", param, ") cannot also appear in `fixed`.")
  }

  # Non-manipulated parameters come from each member's POSTERIOR DRAWS (one draw
  # per group, shared by all members) rather than per-subject medians, so the
  # sweep is run on top of realistic posterior parameter configurations. The
  # swept parameter is overridden below.
  groups <- make_post_groups(
    draws_by_subj, draw_ids_full, N_groups, group_size, seed
  )

  plan(multisession, workers = workers)

  res <- with_progress({
    p <- progressor(along = seq_len(N_groups))
    future_map_dfr(seq_len(N_groups), \(grp_i) {
      p()
      gdf <- groups |> filter(group_id == grp_i)

      # Member-level rows for this group across all sweep values and options
      # (small: |sweep_vals| * n_options * group_size rows, built in-worker).
      raw_grp <- map_dfr(sweep_vals, \(val) {
        gdf_mod <- gdf
        gdf_mod[[param]] <- if (is.null(jitter_sd)) {
          rep(val, group_size)
        } else {
          stats::rnorm(group_size, val, jitter_sd)
        }
        # Clamp any `fixed` parameters to their constant for every member.
        if (!is.null(fixed)) {
          for (nm in names(fixed)) gdf_mod[[nm]] <- rep(fixed[[nm]], group_size)
        }

        map_dfr(seq_len(nrow(opts_full)), \(j) {
          o <- opts_full[j, ]
          run_group_full(gdf_mod, o, group_size, n_trials) |>
            mutate(
              group_sim    = grp_i,
              sweep_param  = param,
              sweep_val    = val,
              option_id    = o$option_id,
              trial_type   = o$trial_type,
              rare_outcome = o$rare_outcome
            )
        })
      })

      if (level == "raw") return(raw_grp)

      # Per-group cell means: choice rate at each choice_order x category.
      raw_grp |>
        group_by(
          sweep_param, sweep_val, choice_order, trial_type, rare_outcome
        ) |>
        summarise(
          p_choice_B = mean(choice),
          n_obs      = n(),
          .groups    = "drop"
        ) |>
        mutate(group_sim = grp_i)
    }, .options = furrr_options(seed = seed))
  })

  plan(sequential)

  if (level != "summary") return(res)

  # Cluster-robust summary: each group's cell mean is the resampling unit
  # (members within a group are NOT independent under a cascade), so the SE is
  # sd(group means) / sqrt(n_groups). Compute se/n_groups BEFORE reassigning
  # p_choice_B (summarise evaluates expressions sequentially, so the mean last).
  res |>
    group_by(sweep_param, sweep_val, choice_order, trial_type, rare_outcome) |>
    summarise(
      se         = sd(p_choice_B) / sqrt(n()),
      n_groups   = n(),
      p_choice_B = mean(p_choice_B),
      .groups    = "drop"
    ) |>
    select(sweep_param, sweep_val, choice_order, trial_type, rare_outcome,
      p_choice_B, se, n_groups)
}

# Parameter sweeps over group_full.stan (80 cores) -----------------------------
# Two copy-stage parameters are each manipulated in turn while all others come
# from the group's posterior draw. Each returns level = "summary": mean
# P(choose B) + cluster-robust SE by choice_order x trial_type x rare_outcome.
#   theta          : social log-odds weight (strength of the social pull on the
#                    consequential choice when copying); 0 -> asocial baseline.
#   eta_intercept  : baseline copy propensity (logit) regardless of evidence.
# Each member's swept value is drawn ~ Normal(val, jitter_sd) where jitter_sd is
# the posterior median of that parameter's subject-level SD (sigma row), so the
# manipulation keeps the estimated between-member heterogeneity rather than
# clamping all members to an identical value.
# Cache: delete the corresponding output/sim/sim_sweep_full_*.rds to re-run.

# Posterior medians of subject-level SDs (mu/sigma row map: eta_intercept = 18,
# eta_strength = 19, eta_social_strength = 20, theta = 21).
sigma_med_full <- fit_group_full |>
  spread_draws(sigma[i]) |>
  filter(i %in% c(18, 21)) |>
  group_by(i) |>
  median_hdci(sigma) |>
  select(i, sigma)
get_sigma_full <- function(row) sigma_med_full$sigma[sigma_med_full$i == row]

sim_sweep_full_specs <- list(
  theta         = list(path = here("output/sim/sim_sweep_full_theta.rds"),
    sweep_vals = seq(-1, 4, by = 0.5),
    jitter_sd  = get_sigma_full(21)),
  eta_intercept = list(
    path = here("output/sim/sim_sweep_full_eta_intercept.rds"),
    sweep_vals = seq(-4, 4, by = 0.5),
    jitter_sd  = get_sigma_full(18))
)

dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)

sim_sweep_full_results <- imap(sim_sweep_full_specs, \(spec, param) {
  if (file.exists(spec$path)) {
    readRDS(spec$path)
  } else {
    res <- sim_sweep_full(
      param      = param,
      sweep_vals = spec$sweep_vals,
      jitter_sd  = spec$jitter_sd,
      N_groups   = 1000L,
      n_trials   = 1L,
      level      = "summary",
      workers    = 80L,
      seed       = 1
    )
    saveRDS(res, spec$path)
    res
  }
})

sim_sweep_full_theta         <- sim_sweep_full_results$theta
sim_sweep_full_eta_intercept <- sim_sweep_full_results$eta_intercept


# =============================================================================
# theta sweep -> adjacent-decider agreement (supplementary figure)
# =============================================================================

sim_theta_agreement_path <- here("output/sim/sim_sweep_theta_agreement.rds")

if (file.exists(sim_theta_agreement_path)) {
  sim_theta_agreement <- readRDS(sim_theta_agreement_path)
} else {
  theta_vals_agree <- seq(-1, 4, 0.5)
  pair_keep        <- c("1-2", "2-3", "3-4", "4-5")

  # Member-level choices for each theta (all members clamped to the same theta).
  sweep_theta_raw <- sim_sweep_full(
    param      = "theta",
    sweep_vals = theta_vals_agree,
    jitter_sd  = NULL,          # clamp: every member shares the swept theta
    N_groups   = 500L,
    group_size = 5L,
    n_trials   = 1L,
    level      = "raw",
    workers    = 80L,
    seed       = 1
  )

  # Adjacent choice-order agreement within each (theta, group, option): order by
  # own sample count, compare each decider with the next; keep pairs 1-2 .. 4-5.
  sim_theta_agreement <- sweep_theta_raw |>
    group_by(sweep_val, group_sim, option_id) |>
    arrange(n_samples, .by_group = TRUE) |>
    mutate(order_pair = paste0(row_number(), "-", row_number() + 1),
      agree      = as.integer(choice == lead(choice))) |>
    ungroup() |>
    filter(!is.na(agree), order_pair %in% pair_keep) |>
    # Per group: mean agreement at each order pair (averaged over options), so
    # the group is the cluster-robust resampling unit.
    group_by(sweep_val, group_sim, order_pair) |>
    summarise(agree = mean(agree), .groups = "drop") |>
    group_by(sweep_val, order_pair) |>
    summarise(se = sd(agree) / sqrt(n()), agree = mean(agree), .groups = "drop")

  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim_theta_agreement, sim_theta_agreement_path)
}


# =============================================================================
# eta_intercept sweep -> adjacent-decider agreement (supplementary figure)
# =============================================================================

sim_eta_int_agree_path <-
  here("output/sim/sim_sweep_eta_intercept_agreement.rds")

if (file.exists(sim_eta_int_agree_path)) {
  sim_eta_intercept_agreement <- readRDS(sim_eta_int_agree_path)
} else {
  eta_intercept_vals_agree <- seq(-4, 4, 0.5)
  pair_keep                <- c("1-2", "2-3", "3-4", "4-5")

  # Member-level choices for each eta_intercept (all members clamped to the
  # value).
  sweep_eta_intercept_raw <- sim_sweep_full(
    param      = "eta_intercept",
    sweep_vals = eta_intercept_vals_agree,
    jitter_sd  = NULL,          # clamp: every member shares the swept value
    N_groups   = 500L,
    group_size = 5L,
    n_trials   = 1L,
    level      = "raw",
    workers    = 80L,
    seed       = 1
  )

  sim_eta_intercept_agreement <- sweep_eta_intercept_raw |>
    group_by(sweep_val, group_sim, option_id) |>
    arrange(n_samples, .by_group = TRUE) |>
    mutate(order_pair = paste0(row_number(), "-", row_number() + 1),
      agree      = as.integer(choice == lead(choice))) |>
    ungroup() |>
    filter(!is.na(agree), order_pair %in% pair_keep) |>
    group_by(sweep_val, group_sim, order_pair) |>
    summarise(agree = mean(agree), .groups = "drop") |>
    group_by(sweep_val, order_pair) |>
    summarise(se = sd(agree) / sqrt(n()), agree = mean(agree), .groups = "drop")

  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim_eta_intercept_agreement, sim_eta_int_agree_path)
}


# =============================================================================
# group_full.stan: posterior-draw generative simulation (NO parameter sweep)
# -----------------------------------------------------------------------------
# "Straight" posterior predictive simulation: the sweeps above with nothing
# manipulated. Every virtual-group member is a real group-condition subject
# whose 21 parameters are drawn JOINTLY from ONE randomly chosen posterior draw
# of that subject (preserving the within-draw parameter correlations, theta
# included). Repeating over N_groups virtual groups (each facing every option)
# propagates full posterior uncertainty into the choice-order curves, so the
# resulting pattern can be checked against the observed group data.
# Output mirrors sim_sweep_full(level=...) and sim_post_solo for overlay.
# Cache: delete output/sim/sim_post_full.rds to re-run.
# =============================================================================

# Posterior-draw generative simulation over group_full.stan.
# group_size: virtual-group size (curves span choice_order 1..group_size).
# fixed     : optional named numeric vector clamping parameter(s) to a constant
#             for every member, applied on top of the posterior draws (NULL =
#             pure posterior draws). E.g. fixed = c(theta = 0) for asocial
#             groups.
# level: "summary" (cluster-robust mean+SE by choice_order x category, group as
#        resampling unit; matches the sweeps), "group" (per-group cell means),
#        or "raw" (every member-level decision).
sim_post_full <- function(
  N_groups   = 1000L,
  group_size = 5L,
  n_trials   = 1L,
  fixed      = NULL,
  level      = c("summary", "group", "raw"),
  workers    = 80L,
  seed       = 1
) {
  level <- match.arg(level)
  fixed <- validate_fixed(fixed, group_full_param_names)

  groups <- make_post_groups(
    draws_by_subj, draw_ids_full, N_groups, group_size, seed
  )

  plan(multisession, workers = workers)

  res <- with_progress({
    p <- progressor(along = seq_len(N_groups))
    future_map_dfr(seq_len(N_groups), \(grp_i) {
      p()
      gdf <- groups[groups$group_id == grp_i, , drop = FALSE]
      # Clamp any `fixed` parameters to their constant for every member (applied
      # on top of the group's posterior draw); NULL = pure posterior draws.
      if (!is.null(fixed)) for (nm in names(fixed)) gdf[[nm]] <- fixed[[nm]]

      raw_grp <- map_dfr(seq_len(nrow(opts_full)), \(j) {
        o <- opts_full[j, ]
        run_group_full(gdf, o, group_size, n_trials) |>
          mutate(
            group_sim    = grp_i,
            option_id    = o$option_id,
            trial_type   = o$trial_type,
            rare_outcome = o$rare_outcome
          )
      })

      if (level == "raw") return(raw_grp)

      # Per-group cell means: choice rate at each choice_order x category.
      raw_grp |>
        group_by(choice_order, trial_type, rare_outcome) |>
        summarise(p_choice_B = mean(choice), n_obs = n(), .groups = "drop") |>
        mutate(group_sim = grp_i)
    }, .options = furrr_options(seed = seed))
  })

  plan(sequential)

  if (level != "summary") return(res)

  # Cluster-robust summary (group as the resampling unit), matching the sweeps.
  res |>
    group_by(choice_order, trial_type, rare_outcome) |>
    summarise(
      se         = sd(p_choice_B) / sqrt(n()),
      n_groups   = n(),
      p_choice_B = mean(p_choice_B),
      .groups    = "drop"
    ) |>
    select(choice_order, trial_type, rare_outcome, p_choice_B, se, n_groups)
}

# Cached execution.
sim_post_full_path <- here("output/sim/sim_post_full.rds")

if (file.exists(sim_post_full_path)) {
  sim_post_full_res <- readRDS(sim_post_full_path)
} else {
  sim_post_full_res <- sim_post_full(
    N_groups = 1000L, n_trials = 1L, level = "summary", workers = 80L, seed = 1
  )
  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim_post_full_res, sim_post_full_path)
}


# =============================================================================
# solo.stan: posterior-draw generative simulation (NO parameter sweep)
# -----------------------------------------------------------------------------
# Solo analogue of sim_post_full and the asocial benchmark for the group
# choice-order curves. Every member is a real solo-condition subject whose 13
# parameters are drawn JOINTLY from ONE randomly chosen posterior draw (one draw
# id per virtual group, shared by all members), preserving within-draw
# correlations. Members are simulated INDEPENDENTLY (group_size = 1, so social
# info never flows) via simulate_group_full, then pooled into virtual groups of
# `group_size` with
# choice_order assigned by n_samples rank within each (group, option). No
# parameter is manipulated; repeating over N_groups propagates posterior
# uncertainty. Output matches sim_post_full for overlay.
# Cache: delete output/sim/sim_post_solo.rds to re-run.
# =============================================================================

solo_param_names <- c(
  "beta_intercept", "beta_step", "beta_trial", "beta_outcome",
  "beta_extremity", "beta_both_seen", "beta_strength",
  "gamma_intercept", "gamma_value", "gamma_estim", "gamma_discover",
  "gamma_switch", "tau_mean"
)

# Per-subject posterior draws of the 13 solo parameters (one row per subj x
# draw).
fit_solo_post <- readRDS(here("output/fit/fit_solo.rds"))
draws_solo <- fit_solo_post |>
  spread_draws(
    beta_intercept[subj], beta_step[subj], beta_trial[subj], beta_outcome[subj],
    beta_extremity[subj], beta_both_seen[subj], beta_strength[subj],
    gamma_intercept[subj], gamma_value[subj], gamma_estim[subj],
    gamma_discover[subj], gamma_switch[subj], tau_mean[subj]
  ) |>
  ungroup() |>
  select(subj, .draw, all_of(solo_param_names))

# Keyed by .draw (rownames) so make_post_groups can share one draw id per group.
draws_solo_by_subj <- split(draws_solo[, c("subj", ".draw", solo_param_names)],
  draws_solo$subj)
draws_solo_by_subj <- lapply(draws_solo_by_subj, \(d) {
  rownames(d) <- as.character(d$.draw)
  d
})
draw_ids_solo <- unique(draws_solo$.draw)

# One solo member (group_size = 1 -> never sees social info) on one option.
run_solo_member_post <- function(prow, o) {
  simulate_group_full(
    group_size   = 1L,
    n_trials     = 1L,
    beta_intercept  = prow$beta_intercept,  beta_step      = prow$beta_step,
    beta_trial      = prow$beta_trial,      beta_outcome   = prow$beta_outcome,
    beta_extremity  = prow$beta_extremity,
    beta_both_seen  = prow$beta_both_seen,
    beta_strength   = prow$beta_strength,
    gamma_intercept = prow$gamma_intercept, gamma_value    = prow$gamma_value,
    gamma_estim     = prow$gamma_estim,
    gamma_discover  = prow$gamma_discover,
    gamma_switch    = prow$gamma_switch,
    tau_mean        = prow$tau_mean,
    beta_scale   = group_scales$beta_scale,
    social_scale = group_scales$social_scale,
    t_offset     = group_scales$t_offset,
    k            = group_scales$k,
    outcome_mean = group_scales$outcome_mean,
    outcome_sd   = group_scales$outcome_sd,
    trial_num    = 0,
    v_A_high = o$v_a_high, v_A_low = o$v_a_low,
    v_B_high = o$v_b_high, v_B_low = o$v_b_low,
    p_A_high = o$p_a_high, p_B_high = o$p_b_high
  )
}

# Posterior-draw asocial simulation: solo subjects, one posterior draw each,
# decided independently then pooled into virtual groups. group_size sets how
# many members are pooled per virtual group (choice_order spans 1..group_size).
# fixed optionally clamps parameter(s) to a constant on top of the posterior
# draws (NULL = pure posterior draws). level as in sim_post_full.
sim_post_solo <- function(
  N_groups   = 1000L,
  group_size = 5L,
  fixed      = NULL,
  level      = c("summary", "group", "raw"),
  workers    = 80L,
  seed       = 1
) {
  level <- match.arg(level)
  fixed <- validate_fixed(fixed, solo_param_names)

  # Pre-sample members in the MAIN process (see make_post_groups): one posterior
  # draw per group (shared by all members); only this compact table reaches the
  # workers, not the full posterior-draw object.
  groups <- make_post_groups(
    draws_solo_by_subj, draw_ids_solo, N_groups, group_size, seed
  )

  plan(multisession, workers = workers)

  res <- with_progress({
    p <- progressor(along = seq_len(N_groups))
    future_map_dfr(seq_len(N_groups), \(grp_i) {
      p()
      gdf <- groups[groups$group_id == grp_i, , drop = FALSE]
      # Clamp any `fixed` parameters to their constant for every member (applied
      # on top of the group's posterior draw); NULL = pure posterior draws.
      if (!is.null(fixed)) for (nm in names(fixed)) gdf[[nm]] <- fixed[[nm]]

      raw_grp <- map_dfr(seq_len(nrow(opts_full)), \(j) {
        o <- opts_full[j, ]
        map_dfr(seq_len(group_size), \(m) run_solo_member_post(gdf[m, ], o)) |>
          mutate(
            choice_order = as.integer(rank(n_samples, ties.method = "min")),
            group_sim    = grp_i,
            option_id    = o$option_id,
            trial_type   = o$trial_type,
            rare_outcome = o$rare_outcome
          )
      })

      if (level == "raw") return(raw_grp)

      raw_grp |>
        group_by(choice_order, trial_type, rare_outcome) |>
        summarise(p_choice_B = mean(choice), n_obs = n(), .groups = "drop") |>
        mutate(group_sim = grp_i)
    }, .options = furrr_options(seed = seed))
  })

  plan(sequential)

  if (level != "summary") return(res)

  # Cluster-robust summary (group as the resampling unit), matching the sweeps.
  res |>
    group_by(choice_order, trial_type, rare_outcome) |>
    summarise(
      se         = sd(p_choice_B) / sqrt(n()),
      n_groups   = n(),
      p_choice_B = mean(p_choice_B),
      .groups    = "drop"
    ) |>
    select(choice_order, trial_type, rare_outcome, p_choice_B, se, n_groups)
}

# Cached execution.
sim_post_solo_path <- here("output/sim/sim_post_solo.rds")

if (file.exists(sim_post_solo_path)) {
  sim_post_solo_res <- readRDS(sim_post_solo_path)
} else {
  sim_post_solo_res <- sim_post_solo(
    N_groups = 1000L, level = "summary", workers = 80L, seed = 1
  )
  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim_post_solo_res, sim_post_solo_path)
}


# =============================================================================
# solo asocial agreement benchmark (supplementary figure)
# -----------------------------------------------------------------------------
# Asocial counterpart of the theta / eta_intercept agreement sweeps: independent
# solo members (solo.stan, NO social info) pooled into virtual groups of 5, with
# choice_order assigned by own sample count. Adjacent-pair agreement here is the
# no-cascade baseline (dotted line in 05_visualization.R). Because the members
# decide independently, any trend across choice order reflects only how own
# sample count relates to choice, never social influence. Same member-level
# computation as the sweeps (arrange by n_samples, score adjacent pairs
# 1-2 .. 4-5, group as the cluster-robust resampling unit).
# Cache: delete output/sim/sim_solo_agreement.rds to re-run.
# =============================================================================

sim_solo_agreement_path <- here("output/sim/sim_solo_agreement.rds")

if (file.exists(sim_solo_agreement_path)) {
  sim_solo_agreement <- readRDS(sim_solo_agreement_path)
} else {
  pair_keep <- c("1-2", "2-3", "3-4", "4-5")

  # Member-level choices for independent solo members pooled into groups of 5.
  solo_agreement_raw <- sim_post_solo(
    N_groups   = 500L,
    group_size = 5L,
    level      = "raw",
    workers    = 80L,
    seed       = 1
  )

  sim_solo_agreement <- solo_agreement_raw |>
    group_by(group_sim, option_id) |>
    arrange(n_samples, .by_group = TRUE) |>
    mutate(order_pair = paste0(row_number(), "-", row_number() + 1),
      agree      = as.integer(choice == lead(choice))) |>
    ungroup() |>
    filter(!is.na(agree), order_pair %in% pair_keep) |>
    group_by(group_sim, order_pair) |>
    summarise(agree = mean(agree), .groups = "drop") |>
    group_by(order_pair) |>
    summarise(se = sd(agree) / sqrt(n()), agree = mean(agree), .groups = "drop")

  dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim_solo_agreement, sim_solo_agreement_path)
}


# =============================================================================
# Large-group extrapolation (group_size = n): group_full.stan and solo.stan
# -----------------------------------------------------------------------------
# Same posterior-draw generative simulation as sim_post_full / sim_post_solo,
# but with a LARGE virtual group so the choice-order curve extends to
# n = group_size_big (25). Non-manipulated parameters come from the posterior
# draws (one draw per group); pass `fixed` to override any parameter(s)
# with a constant (default NULL = pure posterior draws), e.g.
#   sim_post_full(group_size = 25L, fixed = c(theta = 0))   # asocial group
# Change group_size_big to sweep a different n; the cache path is n-tagged so
# each n is cached separately.
# Cache: delete output/sim/sim_post_{full,solo}_n<n>.rds to re-run.
# =============================================================================

group_size_big <- 25L

sim_post_full_n_path <- here(
  sprintf("output/sim/sim_post_full_n%d.rds", group_size_big)
)
sim_post_solo_n_path <- here(
  sprintf("output/sim/sim_post_solo_n%d.rds", group_size_big)
)

dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)

if (file.exists(sim_post_full_n_path)) {
  sim_post_full_n_res <- readRDS(sim_post_full_n_path)
} else {
  sim_post_full_n_res <- sim_post_full(
    N_groups = 1000L, group_size = group_size_big, n_trials = 1L,
    level = "summary", workers = 80L, seed = 1
  )
  saveRDS(sim_post_full_n_res, sim_post_full_n_path)
}

if (file.exists(sim_post_solo_n_path)) {
  sim_post_solo_n_res <- readRDS(sim_post_solo_n_path)
} else {
  sim_post_solo_n_res <- sim_post_solo(
    N_groups = 1000L, group_size = group_size_big,
    level = "summary", workers = 80L, seed = 1
  )
  saveRDS(sim_post_solo_n_res, sim_post_solo_n_path)
}


# Per-group cell means (level = "group") for the N-group bootstrap (05) -------
# One row per virtual group x choice_order x trial_type x rare_outcome. The
# across-group spread of p_choice_B is what the between-condition difference
# band in 05_visualization.R resamples: groups are i.i.d. posterior-draw virtual
# groups, so independently resampling N of them from each condition's pool ==
# one synthetic N-group study per condition. Generated once (1000 groups each)
# and cached.

sim_post_full_group_path <- here("output/sim/sim_post_full_group.rds")
sim_post_solo_group_path <- here("output/sim/sim_post_solo_group.rds")

dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(sim_post_full_group_path)) {
  saveRDS(
    sim_post_full(N_groups = 1000L, level = "group", workers = 80L, seed = 1),
    sim_post_full_group_path
  )
}
if (!file.exists(sim_post_solo_group_path)) {
  saveRDS(
    sim_post_solo(N_groups = 1000L, level = "group", workers = 80L, seed = 1),
    sim_post_solo_group_path
  )
}
