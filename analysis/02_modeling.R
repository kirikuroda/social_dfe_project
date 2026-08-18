# Bayesian parameter estimation (brms and Stan/cmdstanr)

library(tidyverse)
library(here)
library(brms)
library(cmdstanr)
library(magrittr)
library(loo)
library(tidybayes)
load(here("data/data.rda"))
# parse_list_string(), compute_delta_t_at_stop()
source(here("function/R/utils.R"))

# Mixed logistic regression (brms) ---------------------------------------------
# Predict solo choices from n_samples_self x trial_type x rare_outcome.
# Random slopes for n_samples_self, trial_type, and rare_outcome at the subject

fit_choice_solo_brms <- df_trials |>
  filter(condition == "solo") |>
  mutate(n_samples = scale(n_samples_self)) |>
  brm(
    choice_self ~ n_samples * rare_outcome * trial_type +
      (1 + n_samples * rare_outcome * trial_type | subject_id),
    data    = _,
    family  = bernoulli,
    cores   = 4,
    chains  = 4,
    warmup  = 2500,
    iter    = 5000,
    seed    = 1,
    backend = "cmdstanr",
    refresh = 500,
    control = list(adapt_delta = 0.9),
    threads = threading(20),
    file    = here("output/fit/fit_choice_solo_brms.rds")
  )


# Mixed negative binomial regression (brms) ------------------------------------
#
# Predict sample size from condition x trial_type x rare_outcome.
# Random slopes for trial_type and rare_outcome at the subject level.

fit_n_samples_brms <- df_trials |>
  brm(
    n_samples_self ~ condition * trial_type * rare_outcome +
      (1 + trial_type * rare_outcome | subject_id),
    data    = _,
    family  = negbinomial,
    cores   = 4,
    chains  = 4,
    warmup  = 2500,
    iter    = 5000,
    seed    = 1,
    backend = "cmdstanr",
    refresh = 500,
    threads = threading(20),
    file    = here("output/fit/fit_n_samples_brms.rds")
  )


# Rare-outcome experience (brms) -----------------------------------------------
#
# Does the smaller sample size in the group condition lower the probability of
# experiencing a rare (p = 0.1) outcome? For each trial, code whether the
# participant observed the rare outcome of ANY risky option they sampled
# (any_rare, 0/1), pooling over both trial types and both options. Predict
# any_rare from condition (solo = reference) with subject-level random
# intercepts. The per-sample rare rate is ~0.1 in both conditions, so a negative
# condition effect here reflects the lower sample count, not biased sampling.

# Rare payoff value of each risky option: A is risky in every trial; B is risky
# only in risky_risky trials, where its high payoff occurs with p ~ 0.1 (safe B
# has p_b_high == 1 -> NA, contributing no rare outcome).
opt_rare_vals <- df_options |>
  filter(trial_type != "attention_check") |>
  mutate(
    v_a_rare = if_else(rare_outcome == "large", v_a_high, v_a_low),
    v_b_rare = case_when(
      near(p_b_high, 1) ~ NA_real_,
      p_b_high < 0.5    ~ v_b_high,
      TRUE              ~ v_b_low
    )
  ) |>
  select(option_id, v_a_rare, v_b_rare)

# Trial-level any_rare: did any own sample return its option's rare payoff?
# right_join to df_trials keeps trials with zero samples (any_rare = 0).
df_rare_experience <- df_sampling |>
  left_join(opt_rare_vals, by = "option_id") |>
  mutate(rare = (sampled_option == 0 & sampled_outcome == v_a_rare) |
    (sampled_option == 1 & sampled_outcome == v_b_rare)) |>
  group_by(subject_id, trial_number) |>
  summarise(n_rare = sum(rare, na.rm = TRUE), .groups = "drop") |>
  right_join(
    df_trials |> select(subject_id, trial_number, condition),
    by = c("subject_id", "trial_number")
  ) |>
  mutate(
    any_rare  = as.integer(coalesce(n_rare, 0L) >= 1),
    condition = fct_relevel(condition, "solo")  # solo = reference level
  )

fit_rare_experience_brms <- df_rare_experience |>
  brm(
    any_rare ~ condition + (1 | subject_id),
    data    = _,
    family  = bernoulli,
    cores   = 4,
    chains  = 4,
    warmup  = 2500,
    iter    = 5000,
    seed    = 1,
    backend = "cmdstanr",
    refresh = 500,
    threads = threading(20),
    file    = here("output/fit/fit_rare_experience_brms.rds")
  )

# Solo condition data for Stan -------------------------------------------------

# Standardization parameters for sampled outcome values.
# Exclude v_b_low = 0 (risky_safe safe option; not an actual possible outcome).
possible_outcomes <- with(df_options, c(v_a_high, v_a_low,
  v_b_high, v_b_low[v_b_low > 0]))
outcome_mean      <- mean(possible_outcomes)
outcome_sd        <- sd(possible_outcomes)

# Centering offset for t in the stopping model. Pooled across solo and
# group conditions so beta_0 has the same reference point in both Stan models;
# reduces beta_0 / beta_1 posterior correlation. Only stopping-eligible steps
# contribute, matching the `can_stop` condition in the .stan files: sampling can
# be terminated voluntarily only once BOTH options have been sampled at least
# once (which implies t >= 2), plus the terminal step of every trial, which is a
# stop by construction (this covers trials ending at the 40-sample cap).
t_offset_pooled <- df_sampling |>
  arrange(subject_id, trial_number, sample_id) |>
  group_by(subject_id, trial_number) |>
  filter((cummax(as.integer(sampled_option == 0)) == 1 &
    cummax(as.integer(sampled_option == 1)) == 1) |
    sample_id == max(sample_id)) |>
  ungroup() |>
  summarise(offset = mean(sample_id)) |>
  pull(offset)

df_trials_solo <- df_trials |>
  filter(condition == "solo") |>
  mutate(trial_id = row_number())

# Join df_options; subject_id + trial_number uniquely identifies a trial
df_sampling_solo <- df_sampling |>
  filter(condition == "solo") |>
  left_join(
    df_trials_solo |> select(subject_id, option_id, trial_id),
    by = c("subject_id", "option_id")
  ) |>
  left_join(df_options, by = "option_id") |>
  mutate(is_higher_outcome = case_when(
    sampled_option_label == "a" & sampled_outcome == v_a_high ~ 1,
    sampled_option_label == "a" & sampled_outcome == v_a_low  ~ 0,
    sampled_option_label == "b" & sampled_outcome == v_b_high ~ 1,
    sampled_option_label == "b" & sampled_outcome == v_b_low  ~ 0
  ))

# seg_offsets: 1-based start index of each trial's segments (length N+1).
# right_join preserves the trial order of df_trials_solo;
# append N_seg+1 as sentinel.
seg_starts <- df_sampling_solo |>
  mutate(segment = row_number()) |>
  group_by(trial_id) |>
  summarise(min_segment = min(segment)) |>
  pull(min_segment)

# Unit-SD standardization scales (pooled across solo and group) ----------------
# solo.stan divides each choice-stage (tau) and stopping-stage
# (beta) difference feature by its empirical SD so that all tau (resp. beta)
# coefficients are on a common "per 1 SD" scale and directly comparable. To make
# the coefficients comparable ACROSS conditions, the SDs (and the evidence-
# strength scaling constant k) are pooled over the solo and group conditions in
# the block below. The features are deterministic functions of the sampling
# sequence (MLE belief, no parameters), replayed here exactly as in the Stan
# model. (gamma is left unstandardized: its four cues already share the
# {-0.5, 0, +0.5} scale, so they are mutually comparable.)
replay_features <- function(seg, k, t_offset, outcome_mean, outcome_sd) {
  n_A_high <- 0
  n_A_low <- 0
  n_B_high <- 0
  n_B_low <- 0
  obs_A_high <- -1
  obs_A_low <- -1
  obs_B_high <- -1
  obs_B_low <- -1
  val_A_high <- 0
  val_A_low <- 0
  val_B_high <- 0
  val_B_low <- 0
  max_abs_z_A <- 0
  max_abs_z_B <- 0
  max_z_A <- 0
  max_z_B <- 0

  n_steps <- nrow(seg)

  # Stopping-stage (beta) predictors at every stopping-eligible step, matching
  # the `can_stop` condition in the Stan likelihood: both options sampled at
  # least once (which implies t >= 2), plus the terminal step t == n_steps
  # (T in the Stan code).
  beta_step_v <- beta_outcome_v <- beta_absz_v <- beta_both_v <-
    beta_strength_v <- numeric(0)
  tau <- NULL  # choice-stage features, filled at the terminal step

  for (t in seq_len(n_steps)) {
    option    <- seg$sampled_option[t]
    outcome   <- seg$sampled_outcome[t]
    is_high   <- seg$is_higher_outcome[t]
    outcome_z <- (outcome - outcome_mean) / outcome_sd

    # Update beliefs with the observed outcome
    if (option == 0) {
      if (is_high == 1) {
        obs_A_high <- outcome
        val_A_high <- outcome
        if (obs_A_low < 0) val_A_low <- outcome
        n_A_high <- n_A_high + 1
      } else {
        obs_A_low <- outcome
        val_A_low <- outcome
        if (obs_A_high < 0) val_A_high <- outcome
        n_A_low <- n_A_low + 1
      }
      max_abs_z_A <- max(max_abs_z_A, abs(outcome_z))
      max_z_A     <- max(max_z_A, outcome_z)
    } else {
      if (is_high == 1) {
        obs_B_high <- outcome
        val_B_high <- outcome
        if (obs_B_low < 0) val_B_low <- outcome
        n_B_high <- n_B_high + 1
      } else {
        obs_B_low <- outcome
        val_B_low <- outcome
        if (obs_B_high < 0) val_B_high <- outcome
        n_B_low <- n_B_low + 1
      }
      max_abs_z_B <- max(max_abs_z_B, abs(outcome_z))
      max_z_B     <- max(max_z_B, outcome_z)
    }

    # Stopping beliefs after the update
    n_A <- n_A_high + n_A_low
    n_B <- n_B_high + n_B_low
    p_high_A <- if (n_A > 0) n_A_high / n_A else 0.5
    p_high_B <- if (n_B > 0) n_B_high / n_B else 0.5
    mean_A <- p_high_A * val_A_high + (1 - p_high_A) * val_A_low
    mean_B <- p_high_B * val_B_high + (1 - p_high_B) * val_B_low
    var_A <- if (obs_A_high < 0 || obs_A_low < 0) {
      0.01
    } else {
      p_high_A * (mean_A - val_A_high)^2 +
        (1 - p_high_A) * (mean_A - val_A_low)^2
    }
    var_B <- if (obs_B_high < 0 || obs_B_low < 0) {
      0.01
    } else {
      p_high_B * (mean_B - val_B_high)^2 +
        (1 - p_high_B) * (mean_B - val_B_low)^2
    }
    se_diff <- if (n_A == 0) {
      sqrt(var_B / n_B)
    } else if (n_B == 0) {
      sqrt(var_A / n_A)
    } else {
      sqrt(var_A / n_A + var_B / n_B)
    }
    delta_t <- (mean_B - mean_A) / se_diff
    u_t <- tanh(delta_t / k)
    both_seen_either <- if ((obs_A_high >= 0 && obs_A_low >= 0) ||
      (obs_B_high >= 0 && obs_B_low >= 0)) 1 else 0

    if ((n_A > 0 && n_B > 0) || t == n_steps) {
      beta_step_v     <- c(beta_step_v, t - t_offset)
      beta_outcome_v  <- c(beta_outcome_v, outcome_z)
      beta_absz_v     <- c(beta_absz_v, abs(outcome_z))
      beta_both_v     <- c(beta_both_v, both_seen_either)
      beta_strength_v <- c(beta_strength_v, abs(u_t))
    }

    if (t == n_steps) {
      asym     <- (n_B - n_A) / (n_A + n_B)
      last_opt <- if (option == 1) 0.5 else -0.5
      recency  <- outcome_z * (if (option == 1) 1 else -1)
      both_A   <- if (obs_A_high >= 0 && obs_A_low >= 0) 1 else 0
      both_B   <- if (obs_B_high >= 0 && obs_B_low >= 0) 1 else 0
      tau <- c(
        # raw value gap (choice stage); se kept only in u_t
        delta   = mean_B - mean_A,
        asym    = asym,
        last    = last_opt,
        recency = recency,
        extreme = max_abs_z_B - max_abs_z_A,
        both    = 0.5 * (both_B - both_A),
        var     = var_B - var_A,
        maxval  = max_z_B - max_z_A
      )
    }
  }

  list(
    tau  = tau,
    beta = tibble(step = beta_step_v, outcome = beta_outcome_v,
      absz = beta_absz_v, both = beta_both_v,
      strength = beta_strength_v)
  )
}

# Replay one condition's trials and return the tau feature matrix, the beta
# predictor table, and step-level trial_num (z-scored within condition), for
# pooling. trial_num is z-scored in R (trial-level SD = 1) but enters the
# likelihood once per stopping-eligible step, so its step-level SD differs
# slightly from 1; it is standardized on the same step-level basis as the other
# beta predictors. group_split orders by trial_id (= row_number() of the trial
# frame), so trial_num aligns with the replay results by index.
run_replay <- function(samp, trial_frame, k) {
  reps <- samp |>
    group_by(trial_id) |>
    group_split() |>
    map(\(seg) {
      replay_features(seg, k, t_offset_pooled, outcome_mean, outcome_sd)
    })
  trial_num <- scale(trial_frame$trial_number)[, 1]
  n_steps   <- map_int(reps, \(x) nrow(x$beta))
  list(
    tau        = reps |> map("tau") |> do.call(rbind, args = _),
    beta       = reps |> map("beta") |> bind_rows(),
    trial_step = rep(trial_num, n_steps)
  )
}

# --- Group condition data frames ----------------------------------------------
# Built here (before the standardization scales) so the SDs and k can be pooled
# across conditions. Social-information counts are reconstructed below; the
# replay uses only the own-sampling sequence.
df_trials_group <- df_trials |>
  filter(condition == "group") |>
  mutate(trial_id = row_number())

# Social information reconstructed STRICTLY from the within-group sampling
# order. Rather than the recorded choice_other / n_samples_other arrays (which,
# due to
# real-time lag, can let the fewest-sample decider register an observed choice),
# a focal member's social evidence is built only from group-mates who stopped
# with STRICTLY fewer samples (their own n_samples_self). A predecessor who
# stopped at m samples is revealed to the focal at the focal's sample step m
# (sample_id == m), exactly as the group .stan files accumulate social counts,
# so the first decider in each group x option has no social information. The
# strictly-fewer rule also means members who tie at the fewest samples do not
# see each other. choice_self: 0 = chose A, 1 = chose B.
df_social_info <- df_trials_group |>
  select(group_id, option_id, subject_id, n_samples_self, choice_self) |>
  inner_join(
    df_trials_group |>
      select(group_id, option_id,
        other_subject = subject_id,
        other_n       = n_samples_self,
        other_choice  = choice_self),
    by = c("group_id", "option_id"),
    relationship = "many-to-many"
  ) |>
  filter(other_subject != subject_id, other_n < n_samples_self) |>
  group_by(subject_id, option_id, sample_id = other_n) |>
  summarise(
    a_chosen = sum(other_choice == 0),
    b_chosen = sum(other_choice == 1),
    .groups  = "drop"
  )

df_sampling_group <- df_sampling |>
  filter(condition == "group") |>
  left_join(
    df_trials_group |> select(subject_id, option_id, trial_id),
    by = c("subject_id", "option_id")
  ) |>
  left_join(df_options, by = "option_id") |>
  mutate(is_higher_outcome = case_when(
    sampled_option_label == "a" & sampled_outcome == v_a_high ~ 1,
    sampled_option_label == "a" & sampled_outcome == v_a_low  ~ 0,
    sampled_option_label == "b" & sampled_outcome == v_b_high ~ 1,
    sampled_option_label == "b" & sampled_outcome == v_b_low  ~ 0
  )) |>
  left_join(
    df_social_info,
    by = c("subject_id", "option_id", "sample_id")
  ) |>
  replace_na(list(a_chosen = 0, b_chosen = 0)) |>
  mutate(across(c(a_chosen, b_chosen), as.numeric))

seg_starts_group <- df_sampling_group |>
  mutate(segment = row_number()) |>
  group_by(trial_id) |>
  summarise(min_segment = min(segment)) |>
  pull(min_segment)

# Per-subject group index (length N_subj), ordered by subject numeric index
group_id_subj <- df_trials_group |>
  distinct(subject_id, group_id) |>
  mutate(
    subj_idx  = as.numeric(as.factor(subject_id)),
    group_idx = as.numeric(as.factor(group_id))
  ) |>
  arrange(subj_idx) |>
  pull(group_idx)

# --- Pooled scaling constant k ------------------------------------------------
# k = median |delta_t| at the stopping moment, pooled over both conditions so
# that the own-evidence strength |u_t| = |tanh(delta_t / k)| (hence
# beta_strength) is on the same scale in the solo and group models. social_lo_t
# enters its tanh unscaled in the group models (unchanged).
delta_t_at_stop_solo <- df_sampling_solo |>
  group_by(trial_id) |>
  group_modify(~ tibble(delta_t = compute_delta_t_at_stop(.x))) |>
  ungroup() |>
  pull(delta_t)
delta_t_at_stop_group <- df_sampling_group |>
  group_by(trial_id) |>
  group_modify(~ tibble(delta_t = compute_delta_t_at_stop(.x))) |>
  ungroup() |>
  pull(delta_t)
k_pooled <- median(
  abs(c(delta_t_at_stop_solo, delta_t_at_stop_group)),
  na.rm = TRUE
)

# --- Pooled unit-SD standardization scales ------------------------------------
# Replay both conditions with the pooled k, then take SDs over the combined set
# of stopping-eligible steps (both options sampled, or the terminal step). The
# same scales are passed to both stan_data lists so each beta coefficient is on
# a common per-1-SD scale across
# conditions. The choice stage is now a mean-variance comparison on the RAW
# (unstandardized) mean/var differences, so no tau_scale is needed.
replay_solo  <- run_replay(df_sampling_solo,  df_trials_solo,  k_pooled)
replay_group <- run_replay(df_sampling_group, df_trials_group, k_pooled)

# beta_scale order: step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|.
beta_pool       <- bind_rows(replay_solo$beta, replay_group$beta)
trial_step_pool <- c(replay_solo$trial_step, replay_group$trial_step)
beta_scale <- c(
  step     = sd(beta_pool$step),
  trial    = sd(trial_step_pool),
  outcome  = sd(beta_pool$outcome),
  absz     = sd(beta_pool$absz),
  both     = sd(beta_pool$both),
  strength = sd(beta_pool$strength)
)

# Social-evidence scales for the group models (group-only). SD of |s_t| and of
# align = u_t * s_t at stopping-eligible steps (see `can_stop`) where social
# information is present, replaying the belief update (delta_t -> u_t, pooled
# k) and the cumulative social counts exactly as the group .stan files do. Used
# to put the social-evidence-strength (beta_7 / eta_2) and alignment (beta_9 /
# eta_3) coefficients on the same per-1-SD scale as the own-evidence terms, so
# own vs social evidence is comparable within the group models.
replay_social_group <- function(seg, k) {
  n_A_high <- 0
  n_A_low <- 0
  n_B_high <- 0
  n_B_low <- 0
  obs_A_high <- -1
  obs_A_low <- -1
  obs_B_high <- -1
  obs_B_low <- -1
  val_A_high <- 0
  val_A_low <- 0
  val_B_high <- 0
  val_B_low <- 0
  social_A <- 0
  social_B <- 0
  s_abs_v <- numeric(0)
  align_v <- numeric(0)

  for (t in seq_len(nrow(seg))) {
    option  <- seg$sampled_option[t]
    outcome <- seg$sampled_outcome[t]
    is_high <- seg$is_higher_outcome[t]

    if (option == 0) {
      if (is_high == 1) {
        obs_A_high <- outcome
        val_A_high <- outcome
        if (obs_A_low < 0) val_A_low <- outcome
        n_A_high <- n_A_high + 1
      } else {
        obs_A_low <- outcome
        val_A_low <- outcome
        if (obs_A_high < 0) val_A_high <- outcome
        n_A_low <- n_A_low + 1
      }
    } else {
      if (is_high == 1) {
        obs_B_high <- outcome
        val_B_high <- outcome
        if (obs_B_low < 0) val_B_low <- outcome
        n_B_high <- n_B_high + 1
      } else {
        obs_B_low <- outcome
        val_B_low <- outcome
        if (obs_B_high < 0) val_B_high <- outcome
        n_B_low <- n_B_low + 1
      }
    }

    # Social increment is revealed with the outcome (after the belief update),
    # matching the group .stan files.
    social_A <- social_A + seg$a_chosen[t]
    social_B <- social_B + seg$b_chosen[t]

    n_A <- n_A_high + n_A_low
    n_B <- n_B_high + n_B_low
    # Same stopping-eligibility rule as replay_features() / the .stan files.
    can_stop <- (n_A > 0 && n_B > 0) || t == nrow(seg)

    if (can_stop && (social_A > 0 || social_B > 0)) {
      p_high_A <- if (n_A > 0) n_A_high / n_A else 0.5
      p_high_B <- if (n_B > 0) n_B_high / n_B else 0.5
      mean_A <- p_high_A * val_A_high + (1 - p_high_A) * val_A_low
      mean_B <- p_high_B * val_B_high + (1 - p_high_B) * val_B_low
      var_A <- if (obs_A_high < 0 || obs_A_low < 0) {
        0.01
      } else {
        p_high_A * (mean_A - val_A_high)^2 +
          (1 - p_high_A) * (mean_A - val_A_low)^2
      }
      var_B <- if (obs_B_high < 0 || obs_B_low < 0) {
        0.01
      } else {
        p_high_B * (mean_B - val_B_high)^2 +
          (1 - p_high_B) * (mean_B - val_B_low)^2
      }
      se_diff <- if (n_A == 0) {
        sqrt(var_B / n_B)
      } else if (n_B == 0) {
        sqrt(var_A / n_A)
      } else {
        sqrt(var_A / n_A + var_B / n_B)
      }
      u_t       <- tanh(((mean_B - mean_A) / se_diff) / k)
      social_lo <- log((social_B + 0.1) / (social_A + 0.1))
      s_t       <- tanh(social_lo)
      s_abs_v <- c(s_abs_v, abs(s_t))
      align_v <- c(align_v, u_t * s_t)
    }
  }
  tibble(s_abs = s_abs_v, align = align_v)
}

social_tbl <- df_sampling_group |>
  group_by(trial_id) |>
  group_split() |>
  map(\(seg) replay_social_group(seg, k_pooled)) |>
  bind_rows()
# social_scale order: |s_t|, align
social_scale <- c(s = sd(social_tbl$s_abs), align = sd(social_tbl$align))

# Persist the pooled standardization constants so downstream scripts (e.g.
# 05_visualization.R) can back-calculate model quantities such as pi_copy on the
# exact same scale as the fit, without re-running the replay above.
saveRDS(
  list(
    k            = k_pooled,
    beta_scale   = beta_scale,
    social_scale = social_scale,
    outcome_mean = outcome_mean,
    outcome_sd   = outcome_sd,
    t_offset     = t_offset_pooled
  ),
  here("output/fit/group_scales.rds")
)

stan_data_solo <- df_trials_solo %$%
  list(
    N                 = nrow(.),
    N_subj            = max(as.numeric(as.factor(subject_id))),
    N_seg             = nrow(df_sampling_solo),
    option_sampled    = df_sampling_solo$sampled_option,
    v_sampled         = df_sampling_solo$sampled_outcome,
    v_sampled_z       =
      (df_sampling_solo$sampled_outcome - outcome_mean) / outcome_sd,
    is_higher_outcome = df_sampling_solo$is_higher_outcome,
    choice            = choice_self,
    subj_id           = as.numeric(as.factor(subject_id)),
    seg_offsets       = c(seg_starts, nrow(df_sampling_solo) + 1),
    trial_num         = scale(trial_number)[, 1],
    t_offset          = t_offset_pooled,
    k                 = k_pooled,
    # Pooled unit-SD scales for the stopping predictors
    # (step, trial, outcome_z, |outcome_z|, both_seen, |u_t|)
    beta_scale        = as.numeric(beta_scale)
  )

# Solo condition model (Stan/cmdstanr) -----------------------------------------
# solo.stan: sampling + stopping + consequential choice. The choice logit uses a
# single standardized value signal, the se-scaled mean difference
# delta_t = (mean_B - mean_A) / se_diff (the same quantity as the stopping
# stage): tau_mean * delta_t, with tau_mean the inverse temperature on the
# standardized scale. The stopping predictors are unit-SD standardized via
# beta_scale (pooled across conditions).
# Cache: delete output/fit/fit_solo.rds to re-run.

fit_solo_path <- here("output/fit/fit_solo.rds")

if (file.exists(fit_solo_path)) {
  fit_solo <- readRDS(fit_solo_path)
} else {
  model_solo <- cmdstan_model(
    here("function/Stan/solo.stan"),
    cpp_options = list(stan_threads = TRUE)
  )

  fit_solo <- model_solo$sample(
    data              = stan_data_solo,
    seed              = 1,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100
  )

  dir.create(here("output/fit"), recursive = TRUE, showWarnings = FALSE)
  fit_solo$save_object(fit_solo_path)
}

# Group condition data for Stan ------------------------------------------------
# df_social_info, df_trials_group, df_sampling_group, seg_starts_group,
# group_id_subj and the pooled k / beta_scale are all built above
# (before stan_data_solo) so the standardization is pooled across conditions.

stan_data_group <- df_trials_group %$%
  list(
    N                 = nrow(.),
    N_subj            = max(as.numeric(as.factor(subject_id))),
    N_group           = max(as.numeric(as.factor(group_id))),
    N_seg             = nrow(df_sampling_group),
    option_sampled    = df_sampling_group$sampled_option,
    v_sampled         = df_sampling_group$sampled_outcome,
    v_sampled_z       =
      (df_sampling_group$sampled_outcome - outcome_mean) / outcome_sd,
    is_higher_outcome = df_sampling_group$is_higher_outcome,
    choice            = choice_self,
    subj_id           = as.numeric(as.factor(subject_id)),
    group_id          = group_id_subj,
    seg_offsets       = c(seg_starts_group, nrow(df_sampling_group) + 1),
    a_chosen          = df_sampling_group$a_chosen,
    b_chosen          = df_sampling_group$b_chosen,
    trial_num         = scale(trial_number)[, 1],
    t_offset          = t_offset_pooled,
    k                 = k_pooled,
    # Pooled stopping-predictor scales (shared with solo.stan) + social scales
    beta_scale        = as.numeric(beta_scale),
    social_scale      = as.numeric(social_scale)
  )


# Decision biasing model for the group condition (Stan/cmdstanr) ---------------
# Cache: delete output/fit/fit_group_constant.rds to re-run.

fit_group_constant_path <- here("output/fit/fit_group_constant.rds")

if (file.exists(fit_group_constant_path)) {
  fit_group_constant <- readRDS(fit_group_constant_path)
} else {
  model_group_constant <- cmdstan_model(
    here("function/Stan/group_constant.stan"),
    cpp_options = list(stan_threads = TRUE)
  )

  fit_group_constant <- model_group_constant$sample(
    data              = stan_data_group,
    seed              = 1,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100,
    max_treedepth     = 12,
    adapt_delta       = 0.9
  )

  dir.create(here("output/fit"), recursive = TRUE, showWarnings = FALSE)
  fit_group_constant$save_object(fit_group_constant_path)
}


# Null model for the group condition (Stan/cmdstanr) ---------------------------
# Cache: delete output/fit/fit_group_asocial_choice.rds to re-run.

fit_group_asocial_choice_path <- here("output/fit/fit_group_asocial_choice.rds")

if (file.exists(fit_group_asocial_choice_path)) {
  fit_group_asocial_choice <- readRDS(fit_group_asocial_choice_path)
} else {
  model_group_asocial_choice <- cmdstan_model(
    here("function/Stan/group_asocial_choice.stan"),
    cpp_options = list(stan_threads = TRUE)
  )

  fit_group_asocial_choice <- model_group_asocial_choice$sample(
    data              = stan_data_group,
    seed              = 1,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100,
    max_treedepth     = 12,
    adapt_delta       = 0.9
  )

  fit_group_asocial_choice$save_object(fit_group_asocial_choice_path)
}


# Value shaping (Stan/cmdstanr) ------------------------------------------------
# Cache: delete output/fit/fit_group_value_shaping.rds to re-run.

fit_group_value_shaping_path <- here("output/fit/fit_group_value_shaping.rds")

if (file.exists(fit_group_value_shaping_path)) {
  fit_group_value_shaping <- readRDS(fit_group_value_shaping_path)
} else {
  model_group_value_shaping <- cmdstan_model(
    here("function/Stan/group_value_shaping.stan"),
    cpp_options = list(stan_threads = TRUE)
  )

  fit_group_value_shaping <- model_group_value_shaping$sample(
    data              = stan_data_group,
    seed              = 1,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100,
    max_treedepth     = 12,
    adapt_delta       = 0.95
  )

  fit_group_value_shaping$save_object(fit_group_value_shaping_path)
}


# Full model for the group condition (Stan/cmdstanr) ---------------------------
# group_full.stan: pi_copy logit driven by own evidence strength (eta_strength *
# |u_t|) and social evidence strength (eta_social_strength * |s_t|); the
# stopping stage additionally uses the own/social alignment term
# (beta_confirm * align_t).
# The choice-stage alignment term is omitted (not separately identifiable and no
# out-of-sample gain). Shares stan_data_group. Cache: delete
# output/fit/fit_group_full.rds to re-run.

fit_group_full_path <- here("output/fit/fit_group_full.rds")

if (file.exists(fit_group_full_path)) {
  fit_group_full <- readRDS(fit_group_full_path)
} else {
  model_group_full <- cmdstan_model(
    here("function/Stan/group_full.stan"),
    cpp_options = list(stan_threads = TRUE)
  )

  fit_group_full <- model_group_full$sample(
    data              = stan_data_group,
    seed              = 1,
    chains            = 4,
    parallel_chains   = 4,
    threads_per_chain = 20,
    iter_warmup       = 2500,
    iter_sampling     = 2500,
    refresh           = 100,
    max_treedepth     = 12,
    adapt_delta       = 0.95
  )

  fit_group_full$save_object(fit_group_full_path)
}


# LOO-CV: model comparison (group condition) -----------------------------------
loo_group_full             <- fit_group_full$loo()
loo_group_constant         <- fit_group_constant$loo()
loo_group_value_shaping    <- fit_group_value_shaping$loo()
loo_group_asocial_choice   <- fit_group_asocial_choice$loo()

# Ranking among the social-choice models.
loo_compare(
  loo_group_full,
  loo_group_constant,
  loo_group_value_shaping,
  loo_group_asocial_choice
)

save(
  loo_group_full,
  loo_group_constant,
  loo_group_value_shaping,
  loo_group_asocial_choice,
  file = here("output/fit/loo.rda")
)
