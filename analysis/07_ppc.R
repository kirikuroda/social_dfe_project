# =============================================================================
# Posterior predictive check from n posterior draws: solo vs group
# -----------------------------------------------------------------------------
# Draws `n_draws` posterior samples and, for EACH draw, reproduces the observed
# design (solo subjects on the options they saw; the real 33 groups on every
# non-attention option) to generate a posterior-predictive dataset of
# (n_samples, choice, sampling sequence). Three PPC plots are produced, each
# overlaying the observed data, and are also stitched into one combined figure:
#   (1) choice rate as a function of sample size    [consequential choice]
#   (2) distribution of each subject's MEAN sample size          [stopping]
#   (3) which option is sampled at each within-trial step        [sampling]
#
# Each posterior draw is one predictive replicate (matches the R convention in
# 03_simulation.R). This script rebuilds only the intermediate objects it needs,
# so it can be run standalone after 02_modeling.R.
# =============================================================================

library(tidyverse)
library(tidybayes)
library(furrr)
library(progressr)
library(here)
library(patchwork)

source(here("function/R/simulate.R"))
source(here("function/R/plot_settings.R"))  # col_dark, save_pdf
load(here("data/data.rda"))

# Pooled stopping-predictor scales (beta_scale) and evidence-strength scaling
# k, built in 02_modeling.R and pooled across conditions, so BOTH the solo and
# group stopping stages reproduce their Stan models' sample-size (n_samples)
# behaviour.
group_scales <- readRDS(here("output/fit/group_scales.rds"))

# ---- Single knob: number of posterior draws to use --------------------------
n_draws <- 10L
seed    <- 1
workers <- 80L

set.seed(seed)

# =============================================================================
# SOLO: stop-mode PPC machinery (simulate_solo with the stopping rule applied)
# =============================================================================
fit_solo <- readRDS(here("output/fit/fit_solo.rds"))

# Standardization stats (must match 02_modeling.R).
possible_outcomes <- with(df_options,
  c(v_a_high, v_a_low, v_b_high, v_b_low[v_b_low > 0]))
outcome_mean <- mean(possible_outcomes)
outcome_sd   <- sd(possible_outcomes)
# Stopping-eligible steps only (must match 02_modeling.R and the .stan files):
# both options sampled at least once, plus the terminal step of every trial.
t_offset_pooled <- df_sampling |>
  arrange(subject_id, trial_number, sample_id) |>
  group_by(subject_id, trial_number) |>
  filter((cummax(as.integer(sampled_option == 0)) == 1 &
    cummax(as.integer(sampled_option == 1)) == 1) |
    sample_id == max(sample_id)) |>
  ungroup() |>
  summarise(offset = mean(sample_id)) |>
  pull(offset)

param_draws_all <- fit_solo |>
  spread_draws(
    beta_intercept[subj], beta_step[subj],  beta_trial[subj],
    beta_outcome[subj],   beta_extremity[subj], beta_both_seen[subj],
    beta_strength[subj],  tau_mean[subj],
    gamma_intercept[subj], gamma_value[subj], gamma_estim[subj],
    gamma_discover[subj],  gamma_switch[subj]
  )

draw_ids_solo <- sample(unique(param_draws_all$.draw), n_draws)
param_draws   <- param_draws_all |> filter(.draw %in% draw_ids_solo)

df_trials_solo <- df_trials |>
  filter(condition == "solo") |>
  mutate(trial_num_z = as.numeric(scale(trial_number)))

df_options_solo <- df_trials_solo |>
  distinct(option_id, trial_type, rare_outcome) |>
  left_join(df_options, by = c("option_id", "trial_type", "rare_outcome"))

subj_levels <- levels(as.factor(df_trials_solo$subject_id))

# Observed (subject, option) pairs so the PPC replays only the real design.
solo_design <- df_trials_solo |>
  transmute(subject_id, option_id, trial_type, rare_outcome)

trial_num_for_option <- function(opt) {
  d <- df_trials_solo |>
    filter(option_id == opt$option_id, trial_type == opt$trial_type,
      rare_outcome == opt$rare_outcome) |>
    mutate(subj_idx = match(subject_id, subj_levels))
  tibble(subj_idx = seq_along(subj_levels)) |>
    left_join(
      d |> select(subj_idx, trial_num_z),
      by = "subj_idx"
    ) |>
    mutate(trial_num_z = if_else(is.na(trial_num_z), 0, trial_num_z)) |>
    pull(trial_num_z)
}

run_ppc_solo <- function(d, opt, mode = "stop",
                         trial_num = trial_num_for_option(opt)) {
  params <- param_draws |> filter(.draw == d) |> arrange(subj)
  simulate_solo(
    beta_0 = params$beta_intercept, beta_1 = params$beta_step,
    beta_2 = params$beta_trial,     beta_3 = params$beta_outcome,
    beta_4 = params$beta_extremity, beta_5 = params$beta_both_seen,
    beta_6 = params$beta_strength,  tau    = params$tau_mean,
    gamma_0 = params$gamma_intercept, gamma_1 = params$gamma_value,
    gamma_2 = params$gamma_estim,   gamma_3 = params$gamma_discover,
    gamma_4 = params$gamma_switch,
    t_offset = t_offset_pooled, k = group_scales$k,
    beta_scale = group_scales$beta_scale,
    outcome_mean = outcome_mean, outcome_sd = outcome_sd,
    trial_num = trial_num,
    v_A_high = opt$v_a_high, v_A_low = opt$v_a_low,
    v_B_high = opt$v_b_high, v_B_low = opt$v_b_low,
    p_A_high = opt$p_a_high, p_B_high = opt$p_b_high,
    mode = mode
  )
}

# One row per (draw, subject, option): predicted n_samples + choice, restricted
# to the observed solo design.
ppc_solo <- map_dfr(draw_ids_solo, function(d) {
  map_dfr(seq_len(nrow(df_options_solo)), function(i) {
    opt <- df_options_solo[i, ]
    run_ppc_solo(d, opt) |>
      transmute(subj_idx = as.integer(subject), n_samples, choice) |>
      mutate(subject_id = subj_levels[subj_idx],
        option_id = opt$option_id, trial_type = opt$trial_type,
        rare_outcome = opt$rare_outcome, draw = d)
  })
}) |>
  inner_join(
    solo_design,
    by = c("subject_id", "option_id", "trial_type", "rare_outcome")
  ) |>
  mutate(condition = "solo")

# =============================================================================
# SOLO: independent sampling PPC -- which option is sampled at each step
# -----------------------------------------------------------------------------
# The sampling sub-model (gamma_* + cue_switch) is checked ON ITS OWN,
# separately from stopping/choice: run the sampler FORWARD without applying the
# stopping rule (mode = "stepwise"), then truncate each simulated trial to its
# observed length L. The sampler is causal (step t depends only on steps < t)
# and no stopping is applied, so the first L steps of a length-40 run are
# distributed exactly like a length-L run. P(sample B) is then plotted as a
# function of the within-trial step position (i.e. sample size so far).
#
# SOLO here is INDEPENDENT (no stopping applied). The group sampler depends on
# social info (cue_social) that exists only once other members stop and choose,
# so it cannot be run independently; the group sample sequences are instead
# taken from the full generative group run below (its consequential choice /
# sample size are NOT used in the sampling plot). Both are truncated to
# observed lengths so the model's own sample size does not shape the sampling
# curve.
# =============================================================================
solo_lengths <- df_trials_solo |>
  transmute(subject_id, option_id, trial_type, rare_outcome, L = n_samples_self)

ppc_sample_solo <- map_dfr(draw_ids_solo, function(d) {
  map_dfr(seq_len(nrow(df_options_solo)), function(i) {
    opt <- df_options_solo[i, ]
    run_ppc_solo(d, opt, mode = "stepwise") |>
      transmute(subj_idx = as.integer(subject), step, sampled_option) |>
      mutate(subject_id = subj_levels[subj_idx],
        option_id = opt$option_id, trial_type = opt$trial_type,
        rare_outcome = opt$rare_outcome, draw = d)
  })
}) |>
  inner_join(
    solo_lengths,
    by = c("subject_id", "option_id", "trial_type", "rare_outcome")
  ) |>
  filter(step <= L) |>
  mutate(condition = "solo")

saveRDS(ppc_sample_solo, here("output/sim/ppc_sample_solo.rds"))

# The combined solo + group sampling curve and its plot are built at the end,
# after the group generative run supplies the group sample sequences.

# =============================================================================
# GROUP: rebuild the real-design PPC machinery (mirrors 03_simulation.R)
# =============================================================================
group_full_param_names <- c(
  "beta_intercept", "beta_step", "beta_trial", "beta_outcome", "beta_extremity",
  "beta_both_seen", "beta_strength", "beta_social_strength",
  "beta_social_intercept", "beta_confirm", "gamma_intercept", "gamma_value",
  "gamma_estim", "gamma_discover",
  "gamma_switch", "gamma_social", "tau_mean", "eta_intercept", "eta_strength",
  "eta_social_strength", "theta"
)

fit_group_full  <- readRDS(here("output/fit/fit_group_full.rds"))

subj_map_full <- df_trials |>
  filter(condition == "group") |>
  distinct(subject_id) |>
  arrange(subject_id) |>
  mutate(subj = as.numeric(as.factor(subject_id)))

opts_full <- df_options |>
  filter(trial_type != "attention_check") |>
  mutate(rare_outcome = if_else(p_a_high == 0.1, "large", "small"))

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

draws_by_subj <- split(draws_full[, c("subj", ".draw", group_full_param_names)],
  draws_full$subj)
draws_by_subj <- lapply(draws_by_subj, \(d) {
  rownames(d) <- as.character(d$.draw)
  d
})
draw_ids_full <- unique(draws_full$.draw)

run_group_full <- function(gdf, o, group_size, n_trials, trial_num = 0) {
  do.call(simulate_group_full, c(
    list(group_size = group_size, n_trials = n_trials, subject_id = gdf$subj,
      beta_scale = group_scales$beta_scale,
      social_scale = group_scales$social_scale,
      t_offset = group_scales$t_offset, k = group_scales$k,
      outcome_mean = group_scales$outcome_mean,
      outcome_sd = group_scales$outcome_sd,
      trial_num = trial_num,
      v_A_high = o$v_a_high, v_A_low = o$v_a_low, v_B_high = o$v_b_high,
      v_B_low = o$v_b_low, p_A_high = o$p_a_high, p_B_high = o$p_b_high),
    setNames(
      lapply(group_full_param_names, \(p) gdf[[p]]), group_full_param_names
    )
  ))
}

real_groups_full <- df_trials |>
  filter(condition == "group") |>
  distinct(group_id, subject_id) |>
  inner_join(subj_map_full, by = "subject_id") |>
  arrange(group_id, subj)

# For each of n_draws replicates, ONE posterior draw is shared by the WHOLE
# experiment (coherent posterior-predictive sample of the hierarchy).
make_ppc_groups <- function(draws_by_subj, draw_ids, real_groups, R, seed) {
  set.seed(seed)
  gids    <- unique(real_groups$group_id)
  rg_by_g <- split(real_groups$subj, real_groups$group_id)
  map_dfr(seq_len(R), \(r) {
    did <- sample(draw_ids, 1L)
    map_dfr(gids, \(g) {
      subjs <- rg_by_g[[g]]
      rows  <- map_dfr(seq_along(subjs), \(m) {
        row <- draws_by_subj[[as.character(subjs[m])]][
          as.character(did), ,
          drop = FALSE
        ]
        row$member_in_group <- m
        row
      })
      rows$rep_id <- r
      rows$group_id <- g
      rows$draw_used <- did
      rows
    })
  })
}

groups <- make_ppc_groups(
  draws_by_subj, draw_ids_full, real_groups_full, n_draws, seed
)
subj_to_id <- setNames(subj_map_full$subject_id, subj_map_full$subj)

plan(multisession, workers = workers)
ppc_group <- with_progress({
  p <- progressor(along = seq_len(n_draws))
  future_map_dfr(seq_len(n_draws), \(r) {
    p()
    gr   <- groups[groups$rep_id == r, , drop = FALSE]
    gids <- unique(gr$group_id)
    did  <- gr$draw_used[1]
    map_dfr(gids, \(g) {
      gdf <- gr[gr$group_id == g, , drop = FALSE]
      gsz <- nrow(gdf)
      map_dfr(seq_len(nrow(opts_full)), \(j) {
        o <- opts_full[j, ]
        run_group_full(gdf, o, gsz, 1L) |>
          transmute(subject_id = subj_to_id[as.character(subject_id)],
            n_samples, choice, sampling_sequence,
            option_id = o$option_id, trial_type = o$trial_type,
            rare_outcome = o$rare_outcome)
      })
    }) |>
      mutate(draw = did)
  }, .options = furrr_options(seed = seed))
})
plan(sequential)
ppc_group <- ppc_group |> mutate(condition = "group")

# -----------------------------------------------------------------------------
# GROUP sampling sequences (extracted from the generative run above). ONLY the
# sample sequence is used; the run's consequential choice and n_samples do NOT
# enter the sampling plot. Each member's sequence is split into per-step options
# (1 = B) and truncated to its observed trial length L, so the model's own
# (generative) sample size does not shape the curve.
# Observed length keyed by (subject_id, option_id) only -- these identify the
# trial uniquely and avoid any rare_outcome label mismatch with opts_full.
group_lengths <- df_trials |>
  filter(condition == "group") |>
  transmute(subject_id, option_id, L = n_samples_self)

ppc_sample_group <- ppc_group |>
  select(draw, subject_id, option_id, sampling_sequence) |>
  mutate(sampled_option = str_split(sampling_sequence, ",")) |>
  select(-sampling_sequence) |>
  unnest(sampled_option) |>
  filter(sampled_option != "") |>
  group_by(draw, subject_id, option_id) |>
  mutate(step = row_number(),
    sampled_option = as.integer(sampled_option == "B")) |>
  ungroup() |>
  inner_join(group_lengths, by = c("subject_id", "option_id")) |>
  filter(step <= L) |>
  mutate(condition = "group")

saveRDS(ppc_sample_group, here("output/sim/ppc_sample_group.rds"))

# =============================================================================
# Combine predictions + observed
# =============================================================================
ppc_all <- bind_rows(
  ppc_solo  |> select(condition, draw, subject_id, trial_type, rare_outcome,
    n_samples, choice),
  ppc_group |> select(condition, draw, subject_id, trial_type, rare_outcome,
    n_samples, choice)
) |>
  # solo before group so the facets stack solo (top) / group (bottom).
  mutate(condition = factor(condition, levels = c("solo", "group")))

obs_all <- df_trials |>
  filter(condition %in% c("solo", "group")) |>
  transmute(condition, subject_id, trial_type, rare_outcome,
    n_samples = n_samples_self, choice = choice_self) |>
  mutate(condition = factor(condition, levels = c("solo", "group")))

# Problem-type factors: columns are trial_type (risky_safe / risky_risky) and
# rows are rare_outcome (large = High, small = Low), matching the 2 x 2 layout
# of the main solo figure in 05_visualization.R.
add_problem_type <- function(d) {
  d |> mutate(
    trial_type = factor(trial_type, levels = c("risky_safe", "risky_risky")),
    rare_outcome = factor(rare_outcome, levels = c("large", "small"))
  )
}

dir.create(here("output/sim"), recursive = TRUE, showWarnings = FALSE)
saveRDS(ppc_all, here("output/sim/ppc_nsamples_draws.rds"))

# Shared styling for the three PPC panels: theme_minimal at the common base size
# used by the other figures, no minor grid, and title/tag pinned to that size.
# Facet strips show the capitalized condition names (Solo / Group).
theme_ppc <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 9),
    plot.tag         = element_text(size = 9)
  )
cond_labeller <- as_labeller(c(solo = "Solo", group = "Group"))
type_labeller <- as_labeller(
  c(risky_safe = "Risky/Safe", risky_risky = "Risky/Risky")
)
rare_labeller <- as_labeller(c(large = "High", small = "Low"))

# Applied with `&` to the standalone p1 and, at the end, to the combined
# figure, so every panel across the three stages carries the same colours and
# text sizes.
theme_ppc_shared <- theme_minimal(base_size = 9) +
  theme(
    text          = element_text(color = col_dark),
    axis.text     = element_text(color = col_dark),
    axis.line     = element_line(color = col_dark),
    legend.title  = element_text(size = 7),
    legend.text   = element_text(size = 6),
    legend.margin = margin(0, 0, 0, 0)
  )

# Per-stage band/line colours for the three PPC panels.
col_sampling <- "#0070c0"   # sampling (p3)
col_stopping <- "#800074"   # stopping (p2)
col_choice   <- "#298c8c"   # consequential choice (p1)

# =============================================================================
# Plot 1: choice rate as a function of sample size
# =============================================================================
# PPC: choice rate at each n_samples, kept PER DRAW so each of the n_draws
# draws is one thin line (rather than a summarised band). Observed: choice rate
# at each
# n_samples.
ppc_curve <- ppc_all |>
  add_problem_type() |>
  group_by(condition, trial_type, rare_outcome, draw, n_samples) |>
  summarise(p = mean(choice), n = n(), .groups = "drop")

obs_curve <- obs_all |>
  add_problem_type() |>
  group_by(condition, trial_type, rare_outcome, n_samples) |>
  summarise(p = mean(choice), n = n(), .groups = "drop")

# One plot per problem type (trial_type x rare_outcome), each with Solo / Group
# side by side. The four are stitched together with patchwork below.
# The size scale is given a COMMON limit across the four plots so patchwork sees
# identical guides and collapses them into a single legend.
size_limits <- range(obs_curve$n)

make_choice_panel <- function(tt, ro, ylab) {
  ggplot(filter(ppc_curve, trial_type == tt, rare_outcome == ro),
    aes(n_samples)) +
    geom_line(aes(y = p, group = draw), color = col_choice,
      alpha = 0.5, linewidth = 0.3) +
    geom_point(data = filter(obs_curve, trial_type == tt, rare_outcome == ro),
      aes(y = p, size = n), alpha = 0.7) +
    scale_size_continuous(range = c(0.1, 1), limits = size_limits,
      name = "Observations") +
    scale_x_continuous(breaks = scales::breaks_width(5)) +
    facet_wrap(~condition, nrow = 1, labeller = cond_labeller) +
    labs(x = "Sample size", y = ylab) +
    theme_ppc
}

# The stage title sits on the top-left plot of the block, so the block reads as
# one unit without giving each of the four problem types its own title.
p1_rs_l <- make_choice_panel("risky_safe",  "large", "P(choose safe)") +
  labs(title = "Consequential choice")
p1_rs_s <- make_choice_panel("risky_safe",  "small", "P(choose safe)")
p1_rr_l <- make_choice_panel("risky_risky", "large", "P(choose jackpot)")
p1_rr_s <- make_choice_panel("risky_risky", "small", "P(choose jackpot)")

# 2 x 2 arrangement: top-left / top-right / bottom-left / bottom-right =
# Risky/Safe High, Risky/Safe Low, Risky/Risky High, Risky/Risky Low.
# This version carries the shared theme and is used for the standalone PDF; the
# combined figure below rebuilds the same block and themes it there instead.
p1 <- (p1_rs_l | p1_rs_s) / (p1_rr_l | p1_rr_s) +
  plot_layout(guides = "collect") &
  theme_ppc_shared

# =============================================================================
# Plot 2: distribution of each subject's MEAN sample size
# =============================================================================
ppc_subj_mean <- ppc_all |>
  group_by(condition, draw, subject_id) |>
  summarise(mean_n = mean(n_samples), .groups = "drop")

obs_subj_mean <- obs_all |>
  group_by(condition, subject_id) |>
  summarise(mean_n = mean(n_samples), .groups = "drop")

p2 <- ggplot(ppc_subj_mean, aes(mean_n)) +
  geom_line(aes(group = draw),
    stat = "density", color = col_stopping, alpha = 0.5, linewidth = 0.2
  ) +
  geom_density(data = obs_subj_mean, color = "black", linewidth = 0.5) +
  scale_x_continuous(breaks = scales::breaks_width(5)) +
  # axes = "all_x" keeps the shared x scale but repeats its ticks and labels
  # under every panel instead of only the bottom one.
  facet_wrap(~condition, ncol = 1, scales = "free_y", labeller = cond_labeller,
    axes = "all_x") +
  labs(x = "Sample size", y = "Density",
    title = "Stopping") +
  theme_ppc

dir.create(here("output/figure"), recursive = TRUE, showWarnings = FALSE)
ggsave(here("output/figure/ppc_choice_vs_nsamples.pdf"), p1,
  width = 10, height = 7
)
ggsave(here("output/figure/ppc_subject_mean_nsamples.pdf"), p2,
  width = 8, height = 4
)

# =============================================================================
# Plot 3: which option is sampled, as a function of sample size (solo + group)
# =============================================================================
# Solo sequences come from the independent sampler; group sequences from the
# generative run (sample sequence only). Both truncated to observed lengths.
ppc_sample_all <- bind_rows(
  ppc_sample_solo  |> select(condition, draw, step, sampled_option),
  ppc_sample_group |> select(condition, draw, step, sampled_option)
) |>
  mutate(condition = factor(condition, levels = c("solo", "group")))

# Kept PER DRAW so each of the n_draws draws is one thin line (not a band).
ppc_sample_curve <- ppc_sample_all |>
  group_by(condition, draw, step) |>
  summarise(p_B = mean(sampled_option), n = n(), .groups = "drop")

# Observed sampling curve (solo + group, non-attention trials).
non_attn_opts <- df_options |>
  filter(trial_type != "attention_check") |>
  distinct(option_id)

obs_sample_curve <- df_sampling |>
  filter(condition %in% c("solo", "group")) |>
  semi_join(non_attn_opts, by = "option_id") |>
  mutate(condition = factor(condition, levels = c("solo", "group"))) |>
  group_by(condition, step = sample_id) |>
  summarise(p_B = mean(sampled_option), n = n(), .groups = "drop")

# Restrict each panel's x-axis to steps with adequate observed support (n >=
# 20).
step_caps <- obs_sample_curve |>
  filter(n >= 20) |>
  group_by(condition) |>
  summarise(step_cap = max(step), .groups = "drop")

ppc_sample_curve <- ppc_sample_curve |>
  inner_join(step_caps, by = "condition") |>
  filter(step <= step_cap)
obs_sample_curve <- obs_sample_curve |>
  inner_join(step_caps, by = "condition") |>
  filter(step <= step_cap)

p3 <- ggplot(ppc_sample_curve, aes(step)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_line(aes(y = p_B, group = draw), color = col_sampling,
    alpha = 0.5, linewidth = 0.3) +
  geom_point(data = obs_sample_curve, aes(y = p_B, size = n), alpha = 0.7) +
  scale_size_continuous(range = c(0.1, 1), name = "Observations") +
  scale_x_continuous(breaks = seq(5, 40, 5)) +
  # As in p2: shared x scale, but ticks and labels drawn under every panel.
  facet_wrap(~condition, ncol = 1, labeller = cond_labeller, axes = "all_x") +
  labs(x = "Sample size", y = "P(sample option B)",
    title = "Sampling") +
  theme_ppc

ggsave(here("output/figure/ppc_sample_option.pdf"), p3, width = 8, height = 4)

# =============================================================================
# Combined PPC figure: the three sample-size checks in one panel
# =============================================================================
# Place the three posterior predictive checks in one supplementary figure,
# ordered by processing stage: A = which option is sampled (sampling, p3), B =
# per-subject mean sample size (stopping, p2), C = choice rate vs sample size
# (consequential choice, the four p1_* plots). A and B share the top row; C is
# the 2 x 2 block of problem types below. All six plots stay at the TOP level
# so patchwork can align their panel regions: nesting the C block (or wrapping
# it in wrap_elements) hides its axis widths and
# leaves its left edge misaligned with A. Tags are therefore given manually so
# only the three stages are labelled A/B/C.
p1 <- (p1_rs_l | p1_rs_s) / (p1_rr_l | p1_rr_s) +
  plot_layout(guides = "collect")
fig_ppc_nsamples_combined <- ((p3 | p2) / p1) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(tag_levels = list(c("A", "B", "C", "", "", ""))) &
  theme_ppc_shared

save_pdf(fig_ppc_nsamples_combined, "ppc_nsamples_combined.pdf",
  width = 17.8, height = 15
)
