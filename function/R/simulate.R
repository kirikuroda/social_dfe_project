# Simulation functions for DFE models
# Used for posterior predictive checks, posterior-draw simulations, and the
# normative sampling benchmark

simulate_solo <- function(
  beta_0 = 0, beta_1 = 0, beta_2 = 0, beta_3 = 0, beta_4 = 0, beta_5 = 0,
  beta_6 = 0,
  gamma_0 = 0, gamma_1 = 0, gamma_2 = 0, gamma_3 = 0, gamma_4 = 0,
  tau = 1,
  t_offset = 0,
  k = 1,
  beta_scale = rep(1, 6),
  outcome_mean = 50,
  outcome_sd = 1,
  trial_num = 0,
  v_A_high = 80,
  v_A_low = 20,
  v_B_high = 70,
  v_B_low = 30,
  p_A_high = 0.5,
  p_B_high = 0.5,
  n_trials = 1L,
  max_samples = 40L,
  mode = c("stop", "stepwise")
) {
  # Solo DFE simulation matching function/Stan/solo.stan.
  #
  # Parameter map (mu has 13 entries in Stan):
  #   beta_0    stopping intercept
  #   beta_1    (t - t_offset) slope
  #   beta_2    trial_num slope
  #   beta_3    outcome_z slope (signed, satisficing)
  #   beta_4    |outcome_z| slope (extremity)
  #   beta_5    both_seen_either slope
  #   beta_6    |u_t| slope (own evidence strength, |tanh(delta_t / k)|)
  #   gamma_0   sampling intercept
  #   gamma_1   cue_value
  #   gamma_2   cue_estim
  #   gamma_3   cue_discover
  #   gamma_4   cue_switch (switching)
  #   tau       consequential choice sensitivity
  #
  # Belief model: empirical (MLE),
  # mean = p_high * val_high + (1 - p_high) * val_low.
  # var = 0.01 fallback before both outcomes are observed.
  #
  # Subject-level params accept scalar (broadcast to n_subj) or length-n_subj
  # vectors. n_subj is inferred from the longest parameter vector.
  # Simulations are run n_trials times per subject; total n_sim is
  # n_subj * n_trials.
  #
  # Output (mode = "stop"):
  #   One row per simulation with stop step, delta_t at the stop, asocial
  #   choice probability, consequential choice, and the sampling and outcome
  #   sequences.
  # Output (mode = "stepwise"):
  #   One row per (simulation, step) with the hypothetical P(choose B) if the
  #   subject stopped at that step, plus the option sampled and the outcome
  #   observed at that step. Sampling still follows the cue model; stopping is
  #   NOT applied (all sims run to max_samples).

  mode <- match.arg(mode)

  # ---- Broadcast subject-level parameters ----
  subj_params <- list(
    beta_0 = beta_0, beta_1 = beta_1, beta_2 = beta_2, beta_3 = beta_3,
    beta_4 = beta_4, beta_5 = beta_5, beta_6 = beta_6,
    gamma_0 = gamma_0, gamma_1 = gamma_1, gamma_2 = gamma_2,
    gamma_3 = gamma_3, gamma_4 = gamma_4,
    tau = tau
  )
  n_subj <- max(vapply(subj_params, length, integer(1L)))
  subj_params <- lapply(subj_params, function(x) {
    if (length(x) == 1L) {
      rep(as.numeric(x), n_subj)
    } else if (length(x) == n_subj) {
      as.numeric(x)
    } else {
      stop("Subject parameters must be scalar or length n_subj.")
    }
  })

  n_sim <- n_subj * n_trials
  subj_id_vec  <- rep(seq_len(n_subj), times = n_trials)
  trial_id_vec <- rep(seq_len(n_trials), each = n_subj)
  subj_params  <- lapply(subj_params, function(x) x[subj_id_vec])

  b0 <- subj_params$beta_0
  b1 <- subj_params$beta_1
  b2 <- subj_params$beta_2
  b3 <- subj_params$beta_3
  b4 <- subj_params$beta_4
  b5 <- subj_params$beta_5
  b6 <- subj_params$beta_6
  g0 <- subj_params$gamma_0
  g1 <- subj_params$gamma_1
  g2 <- subj_params$gamma_2
  g3 <- subj_params$gamma_3
  g4 <- subj_params$gamma_4
  ta <- subj_params$tau

  if (length(trial_num) == 1L) {
    tn_vec <- rep(as.numeric(trial_num), n_sim)
  } else if (length(trial_num) == n_trials) {
    tn_vec <- rep(as.numeric(trial_num), each = n_subj)
  } else if (length(trial_num) == n_subj) {
    tn_vec <- rep(as.numeric(trial_num), times = n_trials)
  } else if (length(trial_num) == n_sim) {
    tn_vec <- as.numeric(trial_num)
  } else {
    stop(
      "trial_num must be scalar, length n_subj, length n_trials, or ",
      "length n_sim."
    )
  }

  # ---- Pre-draw all outcomes ----
  is_A_high_mat <- matrix(stats::rbinom(n_sim * max_samples, 1L, p_A_high),
    nrow = n_sim, ncol = max_samples)
  is_B_high_mat <- matrix(stats::rbinom(n_sim * max_samples, 1L, p_B_high),
    nrow = n_sim, ncol = max_samples)

  # ---- Per-simulation state ----
  n_A_high <- n_A_low <- n_B_high <- n_B_low <- rep(0L, n_sim)
  outcome_A_high <- outcome_A_low <- rep(-1.0, n_sim)
  outcome_B_high <- outcome_B_low <- rep(-1.0, n_sim)
  val_A_high <- val_A_low <- val_B_high <- val_B_low <- rep(0.0, n_sim)
  cue_switch_state <- rep(0.0, n_sim)
  sampled_options_mat  <- matrix(NA_integer_, nrow = n_sim, ncol = max_samples)
  # Records the realized (raw) sampled outcome at each step, parallel to
  # sampled_options_mat. Only used to export the per-sample outcome sequence
  # (needed to rebuild stan_data for parameter recovery); does not alter the
  # RNG stream or any existing behaviour.
  sampled_outcomes_mat <- matrix(NA_real_, nrow = n_sim, ncol = max_samples)

  if (mode == "stepwise") {
    p_B_step_mat <- matrix(0.0, nrow = n_sim, ncol = max_samples)
  } else {
    active        <- rep(TRUE, n_sim)
    stop_step_vec <- integer(n_sim)
    delta_at_stop <- numeric(n_sim)
  }

  for (t in seq_len(max_samples)) {
    if (mode == "stop") {
      act <- which(active)
      if (length(act) == 0L) break
    } else {
      act <- seq_len(n_sim)
    }
    n_act <- length(act)

    # ---- 1. SAMPLING DECISION (pre-update beliefs) ----
    if (t == 1L) {
      samp_opt <- sample(c(0L, 1L), n_act, replace = TRUE)
    } else {
      nA_pre <- n_A_high[act] + n_A_low[act]
      nB_pre <- n_B_high[act] + n_B_low[act]
      p_high_A_pre <- ifelse(nA_pre > 0, n_A_high[act] / pmax(nA_pre, 1), 0.5)
      p_high_B_pre <- ifelse(nB_pre > 0, n_B_high[act] / pmax(nB_pre, 1), 0.5)

      mean_A_pre <- p_high_A_pre * val_A_high[act] +
        (1 - p_high_A_pre) * val_A_low[act]
      mean_B_pre <- p_high_B_pre * val_B_high[act] +
        (1 - p_high_B_pre) * val_B_low[act]

      both_seen_A_pre <- (outcome_A_high[act] >= 0) & (outcome_A_low[act] >= 0)
      both_seen_B_pre <- (outcome_B_high[act] >= 0) & (outcome_B_low[act] >= 0)

      var_A_pre <- ifelse(both_seen_A_pre,
        p_high_A_pre * (mean_A_pre - val_A_high[act])^2 +
          (1 - p_high_A_pre) * (mean_A_pre - val_A_low[act])^2,
        0.01)
      var_B_pre <- ifelse(both_seen_B_pre,
        p_high_B_pre * (mean_B_pre - val_B_high[act])^2 +
          (1 - p_high_B_pre) * (mean_B_pre - val_B_low[act])^2,
        0.01)
      var_n_A_pre <- ifelse(nA_pre > 0, var_A_pre / pmax(nA_pre, 1), var_A_pre)
      var_n_B_pre <- ifelse(nB_pre > 0, var_B_pre / pmax(nB_pre, 1), var_B_pre)

      cue_value <- ifelse(mean_B_pre > mean_A_pre, 0.5,
        ifelse(mean_B_pre < mean_A_pre, -0.5, 0.0))
      cue_estim <- ifelse(var_n_B_pre > var_n_A_pre, 0.5,
        ifelse(var_n_B_pre < var_n_A_pre, -0.5, 0.0))
      cue_discover <- ifelse(both_seen_A_pre & !both_seen_B_pre, 0.5,
        ifelse(!both_seen_A_pre & both_seen_B_pre, -0.5,
          cue_estim))
      cue_switch_act <- cue_switch_state[act]

      logit_sample_B <- g0[act] +
        g1[act] * cue_value +
        g2[act] * cue_estim +
        g3[act] * cue_discover +
        g4[act] * cue_switch_act
      samp_opt <- stats::rbinom(n_act, 1L, stats::plogis(logit_sample_B))
    }
    sampled_options_mat[cbind(act, t)] <- samp_opt

    # ---- 2. OBSERVE OUTCOME & UPDATE BELIEFS ----
    samp_A_local <- which(samp_opt == 0L)
    samp_B_local <- which(samp_opt == 1L)
    samp_A_glob  <- act[samp_A_local]
    samp_B_glob  <- act[samp_B_local]

    # cue_switch update (after sampling, before belief update; matches Stan)
    cue_switch_state[act] <- ifelse(samp_opt == 0L, 0.5, -0.5)

    observed_outcome <- numeric(n_act)

    if (length(samp_A_glob) > 0L) {
      is_high_A <- is_A_high_mat[samp_A_glob, t]
      high_A <- samp_A_glob[is_high_A == 1L]
      low_A  <- samp_A_glob[is_high_A == 0L]
      if (length(high_A) > 0L) {
        outcome_A_high[high_A] <- v_A_high
        val_A_high[high_A]     <- v_A_high
        mirror <- high_A[outcome_A_low[high_A] < 0]
        if (length(mirror) > 0L) val_A_low[mirror] <- v_A_high
        n_A_high[high_A] <- n_A_high[high_A] + 1L
      }
      if (length(low_A) > 0L) {
        outcome_A_low[low_A] <- v_A_low
        val_A_low[low_A]     <- v_A_low
        mirror <- low_A[outcome_A_high[low_A] < 0]
        if (length(mirror) > 0L) val_A_high[mirror] <- v_A_low
        n_A_low[low_A] <- n_A_low[low_A] + 1L
      }
      observed_outcome[samp_A_local] <-
        ifelse(is_high_A == 1L, v_A_high, v_A_low)
    }

    if (length(samp_B_glob) > 0L) {
      is_high_B <- is_B_high_mat[samp_B_glob, t]
      high_B <- samp_B_glob[is_high_B == 1L]
      low_B  <- samp_B_glob[is_high_B == 0L]
      if (length(high_B) > 0L) {
        outcome_B_high[high_B] <- v_B_high
        val_B_high[high_B]     <- v_B_high
        mirror <- high_B[outcome_B_low[high_B] < 0]
        if (length(mirror) > 0L) val_B_low[mirror] <- v_B_high
        n_B_high[high_B] <- n_B_high[high_B] + 1L
      }
      if (length(low_B) > 0L) {
        outcome_B_low[low_B] <- v_B_low
        val_B_low[low_B]     <- v_B_low
        mirror <- low_B[outcome_B_high[low_B] < 0]
        if (length(mirror) > 0L) val_B_high[mirror] <- v_B_low
        n_B_low[low_B] <- n_B_low[low_B] + 1L
      }
      observed_outcome[samp_B_local] <-
        ifelse(is_high_B == 1L, v_B_high, v_B_low)
    }
    sampled_outcomes_mat[cbind(act, t)] <- observed_outcome
    outcome_z <- (observed_outcome - outcome_mean) / outcome_sd

    # ---- 3. POST-UPDATE STATE (for stopping + consequential choice) ----
    nA_post <- n_A_high[act] + n_A_low[act]
    nB_post <- n_B_high[act] + n_B_low[act]
    p_high_A_post <- ifelse(nA_post > 0, n_A_high[act] / pmax(nA_post, 1), 0.5)
    p_high_B_post <- ifelse(nB_post > 0, n_B_high[act] / pmax(nB_post, 1), 0.5)

    mean_A_post <- p_high_A_post * val_A_high[act] +
      (1 - p_high_A_post) * val_A_low[act]
    mean_B_post <- p_high_B_post * val_B_high[act] +
      (1 - p_high_B_post) * val_B_low[act]

    both_seen_A_post <- (outcome_A_high[act] >= 0) & (outcome_A_low[act] >= 0)
    both_seen_B_post <- (outcome_B_high[act] >= 0) & (outcome_B_low[act] >= 0)

    var_A_post <- ifelse(both_seen_A_post,
      p_high_A_post * (mean_A_post - val_A_high[act])^2 +
        (1 - p_high_A_post) * (mean_A_post - val_A_low[act])^2,
      0.01)
    var_B_post <- ifelse(both_seen_B_post,
      p_high_B_post * (mean_B_post - val_B_high[act])^2 +
        (1 - p_high_B_post) * (mean_B_post - val_B_low[act])^2,
      0.01)

    se_diff <- ifelse(nA_post == 0, sqrt(var_B_post / pmax(nB_post, 1)),
      ifelse(nB_post == 0, sqrt(var_A_post / pmax(nA_post, 1)),
        sqrt(var_A_post / pmax(nA_post, 1) +
          var_B_post / pmax(nB_post, 1))))
    delta_t <- (mean_B_post - mean_A_post) / se_diff

    # Own evidence strength: |u_t| = |tanh(delta_t / k)| (matches solo.stan).
    u_t <- tanh(delta_t / k)
    both_seen_either <- as.numeric(both_seen_A_post | both_seen_B_post)

    # ---- 4. RECORD / STOPPING DECISION ----
    if (mode == "stepwise") {
      p_B_step_mat[, t] <- stats::plogis(ta * delta_t)
    } else {
      # Logit of STOPPING (matches solo.stan / simulate_group_full): each
      # predictor is divided by its empirical step-level SD (beta_scale), and
      # the 7th predictor is own-evidence strength |u_t| = |tanh(delta_t / k)|.
      logit_stop <- b0[act] +
        b1[act] * ((t - t_offset)    / beta_scale[1]) +
        b2[act] * (tn_vec[act]       / beta_scale[2]) +
        b3[act] * (outcome_z         / beta_scale[3]) +
        b4[act] * (abs(outcome_z)    / beta_scale[4]) +
        b5[act] * (both_seen_either  / beta_scale[5]) +
        b6[act] * (abs(u_t)          / beta_scale[6])

      if (t == max_samples) {
        # The cap terminates sampling; the terminal step counts as a stop,
        # matching `can_stop` in solo.stan.
        do_stop <- rep(TRUE, n_act)
      } else if (t == 1L) {
        # Only one option can have been sampled at t == 1, so stopping is
        # impossible (see `can_stop` in solo.stan).
        do_stop <- rep(FALSE, n_act)
      } else {
        # Stopping is only possible once BOTH options have been sampled at
        # least once; ineligible sims continue deterministically.
        eligible <- (nA_post > 0) & (nB_post > 0)
        do_stop  <- eligible &
          (stats::rbinom(n_act, 1L, stats::plogis(logit_stop)) == 1L)
      }

      stopped_idx <- act[do_stop]
      if (length(stopped_idx) > 0L) {
        stop_step_vec[stopped_idx] <- t
        delta_at_stop[stopped_idx] <- delta_t[do_stop]
        active[stopped_idx]        <- FALSE
      }
    }
  }

  # ---- Output ----
  if (mode == "stepwise") {
    tibble::tibble(
      sim_id          = rep(seq_len(n_sim), times = max_samples),
      subject         = rep(subj_id_vec,  times = max_samples),
      trial           = rep(trial_id_vec, times = max_samples),
      step            = rep(seq_len(max_samples), each = n_sim),
      p_choice_B_step = as.numeric(p_B_step_mat),
      sampled_option  = as.numeric(sampled_options_mat),  # 0=A, 1=B, per step
      sampled_outcome = as.numeric(sampled_outcomes_mat)  # raw payoff, per step
    )
  } else {
    p_B_final <- stats::plogis(ta * delta_at_stop)
    choice_B  <- stats::rbinom(n_sim, 1L, p_B_final)
    sampling_seq <- vapply(seq_len(n_sim), function(i) {
      s <- stop_step_vec[i]
      if (s == 0L) return("")
      paste(ifelse(sampled_options_mat[i, seq_len(s)] == 0L, "A", "B"),
        collapse = ",")
    }, character(1L))
    # Per-sample raw outcome sequence, aligned with sampling_sequence.
    outcome_seq <- vapply(seq_len(n_sim), function(i) {
      s <- stop_step_vec[i]
      if (s == 0L) return("")
      paste(sampled_outcomes_mat[i, seq_len(s)], collapse = ",")
    }, character(1L))
    tibble::tibble(
      sim_id             = seq_len(n_sim),
      subject            = subj_id_vec,
      trial              = trial_id_vec,
      n_samples          = stop_step_vec,
      delta_at_stop      = delta_at_stop,
      p_choice_B_asocial = p_B_final,
      choice             = choice_B,
      sampling_sequence  = sampling_seq,
      outcome_sequence   = outcome_seq
    )
  }
}


simulate_benchmark <- function(
  v_A_high = 80,
  v_A_low = 20,
  v_B_high = 70,
  v_B_low = 30,
  p_A_high = 0.5,
  p_B_high = 0.5,
  n_trials = 1L,
  max_samples = 40L,
  sampling_strategy = c("alternating", "random"),
  choice_signal = c("se_scaled", "mean_diff"),
  tau = 1,
  prob_weight = 1,
  utility_pow = 1
) {
  # Benchmark DFE simulation: a stripped-down variant of simulate_solo with
  # NO cue-based sampling and NO stopping rule. Sampling follows a fixed
  # schedule (alternating or random), every simulation runs to max_samples,
  # and P(choose B) is recorded after each sample n = 1, 2, ..., max_samples
  # using the same consequential-choice logic as simulate_solo:
  #   P(choose B | n) = inv_logit(tau * delta_t_n)
  # The defaults (tau = 1, utility_pow = 1, prob_weight = 1,
  # choice_signal = "se_scaled") give the risk-neutral benchmark, in which
  # delta_t_n is the standardized mean difference after n samples. The four
  # arguments relax that benchmark one assumption at a time:
  #   tau            inverse temperature on the choice signal
  #   utility_pow    power (CRRA-style) utility exponent on outcomes
  #   prob_weight    Prelec 1-parameter probability weighting
  #   choice_signal  "se_scaled" (standardized) or "mean_diff" (raw difference)
  #
  # Belief model matches simulate_solo: empirical (MLE) p_high,
  # mean = p_high * val_high + (1 - p_high) * val_low,
  # var = 0.01 fallback before both outcomes of an option are observed. Under
  # non-default utility_pow / prob_weight the means entering the choice signal
  # are transformed accordingly (see inline comments below).
  #
  # Output: one row per (simulation, step) for every step in 1..max_samples,
  # with p_choice_B_step = the hypothetical P(choose B) if the choice were
  # made after `step` samples.

  sampling_strategy <- match.arg(sampling_strategy)
  choice_signal     <- match.arg(choice_signal)

  n_sim <- n_trials

  # ---- Pre-draw all outcomes ----
  is_A_high_mat <- matrix(stats::rbinom(n_sim * max_samples, 1L, p_A_high),
    nrow = n_sim, ncol = max_samples)
  is_B_high_mat <- matrix(stats::rbinom(n_sim * max_samples, 1L, p_B_high),
    nrow = n_sim, ncol = max_samples)

  # ---- Per-simulation belief state ----
  # `seen_*` flags track whether each outcome has been observed yet. A boolean
  # flag is used (rather than a sign-based sentinel) so that negative outcomes
  # are handled correctly.
  n_A_high <- n_A_low <- n_B_high <- n_B_low <- rep(0L, n_sim)
  seen_A_high <- seen_A_low <- seen_B_high <- seen_B_low <- rep(FALSE, n_sim)
  val_A_high <- val_A_low <- val_B_high <- val_B_low <- rep(0.0, n_sim)
  sampled_options_mat <- matrix(NA_integer_, nrow = n_sim, ncol = max_samples)
  p_B_step_mat <- matrix(0.0, nrow = n_sim, ncol = max_samples)

  for (t in seq_len(max_samples)) {
    # ---- 1. SAMPLING DECISION (fixed schedule, no cue model) ----
    if (t == 1L) {
      samp_opt <- sample(c(0L, 1L), n_sim, replace = TRUE)
    } else if (sampling_strategy == "alternating") {
      samp_opt <- 1L - sampled_options_mat[, t - 1L]
    } else {
      samp_opt <- sample(c(0L, 1L), n_sim, replace = TRUE)
    }
    sampled_options_mat[, t] <- samp_opt

    # ---- 2. OBSERVE OUTCOME & UPDATE BELIEFS ----
    samp_A_glob <- which(samp_opt == 0L)
    samp_B_glob <- which(samp_opt == 1L)

    if (length(samp_A_glob) > 0L) {
      is_high_A <- is_A_high_mat[samp_A_glob, t]
      high_A <- samp_A_glob[is_high_A == 1L]
      low_A  <- samp_A_glob[is_high_A == 0L]
      if (length(high_A) > 0L) {
        val_A_high[high_A]     <- v_A_high
        mirror <- high_A[!seen_A_low[high_A]]
        if (length(mirror) > 0L) val_A_low[mirror] <- v_A_high
        seen_A_high[high_A] <- TRUE
        n_A_high[high_A] <- n_A_high[high_A] + 1L
      }
      if (length(low_A) > 0L) {
        val_A_low[low_A]     <- v_A_low
        mirror <- low_A[!seen_A_high[low_A]]
        if (length(mirror) > 0L) val_A_high[mirror] <- v_A_low
        seen_A_low[low_A] <- TRUE
        n_A_low[low_A] <- n_A_low[low_A] + 1L
      }
    }

    if (length(samp_B_glob) > 0L) {
      is_high_B <- is_B_high_mat[samp_B_glob, t]
      high_B <- samp_B_glob[is_high_B == 1L]
      low_B  <- samp_B_glob[is_high_B == 0L]
      if (length(high_B) > 0L) {
        val_B_high[high_B]     <- v_B_high
        mirror <- high_B[!seen_B_low[high_B]]
        if (length(mirror) > 0L) val_B_low[mirror] <- v_B_high
        seen_B_high[high_B] <- TRUE
        n_B_high[high_B] <- n_B_high[high_B] + 1L
      }
      if (length(low_B) > 0L) {
        val_B_low[low_B]     <- v_B_low
        mirror <- low_B[!seen_B_high[low_B]]
        if (length(mirror) > 0L) val_B_high[mirror] <- v_B_low
        seen_B_low[low_B] <- TRUE
        n_B_low[low_B] <- n_B_low[low_B] + 1L
      }
    }

    # ---- 3. POST-UPDATE delta_t and P(choose B) ----
    nA_post <- n_A_high + n_A_low
    nB_post <- n_B_high + n_B_low
    p_high_A_post <- ifelse(nA_post > 0, n_A_high / pmax(nA_post, 1), 0.5)
    p_high_B_post <- ifelse(nB_post > 0, n_B_high / pmax(nB_post, 1), 0.5)

    # Power (CRRA-style) utility on outcomes: u(x) = sign(x) * |x|^utility_pow.
    # utility_pow = 1 -> identity (raw outcomes). Applied to each option's two
    # outcome values, so the choice-signal mean and its variance / SE below are
    # in utility units. The transform is monotonic, preserving better/worse.
    ufun  <- function(x) sign(x) * abs(x)^utility_pow
    uvh_A <- ufun(val_A_high)
    uvl_A <- ufun(val_A_low)
    uvh_B <- ufun(val_B_high)
    uvl_B <- ufun(val_B_low)

    mean_A_post <- p_high_A_post * uvh_A + (1 - p_high_A_post) * uvl_A
    mean_B_post <- p_high_B_post * uvh_B + (1 - p_high_B_post) * uvl_B

    # Rank-dependent probability weighting (Prelec 1-parameter): w(p) =
    # exp(-(-log p)^prob_weight); prob_weight = 1 -> w(p) = p (identity). The
    # decision weight w(p) is applied to the probability of the better-valued
    # outcome of each option; the (utility) means used as the choice signal are
    # distorted accordingly, while the variance / standard error below stay
    # based on the raw sampled frequencies.
    wfun <- function(p) {
      w <- exp(-(-log(p))^prob_weight)
      w[p <= 0] <- 0
      w[p >= 1] <- 1
      w
    }
    better_A   <- uvh_A >= uvl_A
    p_better_A <- ifelse(better_A, p_high_A_post, 1 - p_high_A_post)
    dw_A       <- wfun(p_better_A)
    mean_A_w   <- ifelse(better_A,
      dw_A * uvh_A + (1 - dw_A) * uvl_A,
      dw_A * uvl_A + (1 - dw_A) * uvh_A)
    better_B   <- uvh_B >= uvl_B
    p_better_B <- ifelse(better_B, p_high_B_post, 1 - p_high_B_post)
    dw_B       <- wfun(p_better_B)
    mean_B_w   <- ifelse(better_B,
      dw_B * uvh_B + (1 - dw_B) * uvl_B,
      dw_B * uvl_B + (1 - dw_B) * uvh_B)

    both_seen_A_post <- seen_A_high & seen_A_low
    both_seen_B_post <- seen_B_high & seen_B_low

    var_A_post <- ifelse(both_seen_A_post,
      p_high_A_post * (mean_A_post - uvh_A)^2 +
        (1 - p_high_A_post) * (mean_A_post - uvl_A)^2,
      0.01)
    var_B_post <- ifelse(both_seen_B_post,
      p_high_B_post * (mean_B_post - uvh_B)^2 +
        (1 - p_high_B_post) * (mean_B_post - uvl_B)^2,
      0.01)

    if (choice_signal == "se_scaled") {
      se_diff <- ifelse(nA_post == 0, sqrt(var_B_post / pmax(nB_post, 1)),
        ifelse(nB_post == 0, sqrt(var_A_post / pmax(nA_post, 1)),
          sqrt(var_A_post / pmax(nA_post, 1) +
            var_B_post / pmax(nB_post, 1))))
      delta_t <- (mean_B_w - mean_A_w) / se_diff
    } else {
      # Raw mean difference, no standard-error scaling.
      delta_t <- mean_B_w - mean_A_w
    }

    p_B_step_mat[, t] <- stats::plogis(tau * delta_t)
  }

  tibble::tibble(
    sim_id          = rep(seq_len(n_sim), times = max_samples),
    trial           = rep(seq_len(n_trials), times = max_samples),
    step            = rep(seq_len(max_samples), each = n_sim),
    p_choice_B_step = as.numeric(p_B_step_mat)
  )
}


# =============================================================================
# simulate_group_full(): generative simulation faithful to the CURRENT
# function/Stan/group_full.stan.
#
# STOPPING is driven by own/social evidence strength and confirmation:
#     logit(stop_t) = beta_intercept
#                   + beta_step      * (t - t_offset)        / beta_scale[1]
#                   + beta_trial     * trial_num             / beta_scale[2]
#                   + beta_outcome   * outcome_z             / beta_scale[3]
#                   + beta_extremity * |outcome_z|           / beta_scale[4]
#                   + beta_both_seen * both_seen_either      / beta_scale[5]
#                   + beta_strength  * |u_t|                 / beta_scale[6]
#       (+ social branch, only when social info is present:)
#                   + beta_social_strength  * |s_t|          / social_scale[1]
#                   + beta_social_intercept
#                   + beta_confirm          * align_t        / social_scale[2]
#   This is the LOGIT OF STOPPING (P(stop) = inv_logit(logit_stop)).
#
# CONSEQUENTIAL CHOICE copy propensity has three eta terms and no alignment
# term:
#     logit(pi_copy_t) = eta_intercept
#                      + eta_strength        * |u_t| / beta_scale[6]
#                      + eta_social_strength * |s_t| / social_scale[1]
#     p_B = (1 - pi) * inv_logit(tau_mean * delta_t)
#           + pi * inv_logit(theta * social_lo)
#
# where
#     u_t     = tanh(delta_t / k)            (signed, |u_t| is own strength)
#     s_t     = tanh(social_lo_t)            (signed, |s_t| is social strength)
#     align_t = u_t * s_t                    (stopping only)
#
# Every parameter is accepted as a scalar (recycled to all members) OR a vector
# of length group_size, so ANY parameter can be manipulated/swept by the caller
# simply by passing the swept value(s) for that argument while leaving the rest
# at their estimated per-subject values.
#
# beta_scale (length 6: step, trial_num, outcome_z, |outcome_z|, both_seen,
# |u_t|) and social_scale (length 2: |s_t|, align) must be the SAME unit-SD
# constants used when fitting group_full.stan; load them from
# output/fit/group_scales.rds.
# =============================================================================
simulate_group_full <- function(
  group_size = 5,
  n_trials = 1,
  max_samples = 40,
  subject_id = NULL,
  beta_intercept = 0, beta_step = 0, beta_trial = 0, beta_outcome = 0,
  beta_extremity = 0, beta_both_seen = 0, beta_strength = 0,
  beta_social_strength = 0, beta_social_intercept = 0, beta_confirm = 0,
  gamma_intercept = 0, gamma_value = 0, gamma_estim = 0, gamma_discover = 0,
  gamma_switch = 0, gamma_social = 0,
  tau_mean = 1, eta_intercept = 0, eta_strength = 0, eta_social_strength = 0,
  theta = 0,
  beta_scale = rep(1, 6),
  social_scale = rep(1, 2),
  t_offset = 0,
  k = 1,
  outcome_mean = 50,
  outcome_sd = 1,
  trial_num = 0,
  v_A_high = 80,
  v_A_low = 20,
  v_B_high = 70,
  v_B_low = 30,
  p_A_high = 0.5,
  p_B_high = 0.5
) {
  if (length(beta_scale) != 6) stop("beta_scale must have length 6.")
  if (length(social_scale) != 2) stop("social_scale must have length 2.")
  bs <- as.numeric(beta_scale)
  ss <- as.numeric(social_scale)

  coerce_param_vec <- function(x, group_size, name) {
    if (is.numeric(x) && is.atomic(x)) {
      if (length(x) == 1L) return(rep(as.numeric(x), group_size))
      if (length(x) == group_size) return(as.numeric(x))
    }
    stop(sprintf("%s length must be 1 or group_size (%d).", name, group_size))
  }

  subject_id_vec <- if (is.null(subject_id)) {
    as.character(seq_len(group_size))
  } else {
    if (length(subject_id) != group_size)
      stop(sprintf("subject_id length must equal group_size (%d).", group_size))
    as.character(subject_id)
  }

  # Stopping-stage parameters
  b_int  <- coerce_param_vec(beta_intercept,       group_size, "beta_intercept")
  b_stp  <- coerce_param_vec(beta_step,            group_size, "beta_step")
  b_trl  <- coerce_param_vec(beta_trial,           group_size, "beta_trial")
  b_out  <- coerce_param_vec(beta_outcome,         group_size, "beta_outcome")
  b_ext  <- coerce_param_vec(beta_extremity,       group_size, "beta_extremity")
  b_both <- coerce_param_vec(beta_both_seen,       group_size, "beta_both_seen")
  b_str  <- coerce_param_vec(beta_strength,        group_size, "beta_strength")
  b_sstr <- coerce_param_vec(
    beta_social_strength, group_size, "beta_social_strength"
  )
  b_sint <- coerce_param_vec(
    beta_social_intercept, group_size, "beta_social_intercept"
  )
  b_conf <- coerce_param_vec(beta_confirm,         group_size, "beta_confirm")
  # Sampling-stage parameters
  g_int  <- coerce_param_vec(gamma_intercept, group_size, "gamma_intercept")
  g_val  <- coerce_param_vec(gamma_value,     group_size, "gamma_value")
  g_est  <- coerce_param_vec(gamma_estim,     group_size, "gamma_estim")
  g_dis  <- coerce_param_vec(gamma_discover,  group_size, "gamma_discover")
  g_swi  <- coerce_param_vec(gamma_switch,    group_size, "gamma_switch")
  g_soc  <- coerce_param_vec(gamma_social,    group_size, "gamma_social")
  # Choice-stage parameters
  tau_v  <- coerce_param_vec(tau_mean,            group_size, "tau_mean")
  e_int  <- coerce_param_vec(eta_intercept,       group_size, "eta_intercept")
  e_str  <- coerce_param_vec(eta_strength,        group_size, "eta_strength")
  e_sstr <- coerce_param_vec(
    eta_social_strength, group_size, "eta_social_strength"
  )
  the_v  <- coerce_param_vec(theta,               group_size, "theta")

  # Records the realized (raw) sampled outcome per (member, step) for the
  # current trial, parallel to sampled_options_mat. Exported as
  # outcome_sequence for parameter recovery; only steps 1..stop_step are ever
  # read, so no per-trial reset is needed. Records already-drawn values -> no
  # effect on the RNG stream.
  sampled_outcomes_mat <- matrix(
    NA_real_,
    nrow = group_size, ncol = max_samples
  )

  if (length(trial_num) == 1L) {
    tn_vec <- rep(as.numeric(trial_num), n_trials)
  } else if (length(trial_num) == n_trials) {
    tn_vec <- as.numeric(trial_num)
  } else {
    stop("trial_num must be scalar or length n_trials.")
  }

  results <- vector("list", n_trials * group_size)
  row_idx <- 0L

  for (trial in seq_len(n_trials)) {
    tn_t <- tn_vec[trial]

    # Per-subject mutable belief state
    obs_A_high <- obs_A_low <- obs_B_high <- obs_B_low <- rep(-1, group_size)
    n_A_high <- n_A_low <- n_B_high <- n_B_low <- integer(group_size)
    val_A_high <- val_A_low <- val_B_high <- val_B_low <- rep(0.0, group_size)
    sampled_options_mat <- matrix(0L, nrow = group_size, ncol = max_samples)
    cue_switch_vec <- rep(0.0, group_size)

    stopped       <- logical(group_size)
    stop_step_vec <- integer(group_size)
    delta_t_stop  <- numeric(group_size)
    p_asocial_vec <- numeric(group_size)
    pi_copy_vec   <- numeric(group_size)
    align_vec     <- numeric(group_size)
    u_vec         <- numeric(group_size)
    s_vec         <- numeric(group_size)
    seen_social_A <- integer(group_size)
    seen_social_B <- integer(group_size)
    final_p_B     <- numeric(group_size)
    final_choice  <- integer(group_size)

    social_A_count <- 0L
    social_B_count <- 0L

    for (t in seq_len(max_samples)) {
      # Snapshot social info at start of step t; choices made this step are
      # only visible from step t+1 onward.
      social_A_at_t <- social_A_count
      social_B_at_t <- social_B_count
      new_social_A  <- 0L
      new_social_B  <- 0L

      # Smoothed social cue (sampling stage), shared across not-yet-stopped
      # subjects
      social_lo_pre <- log((social_B_at_t + 0.1) / (social_A_at_t + 0.1))
      cue_social    <- tanh(social_lo_pre) / 2

      for (s in seq_len(group_size)) {
        if (stopped[s]) next

        # ---- 1. Sampling decision ----
        if (t == 1L) {
          sampled_option <- sample.int(2L, 1L) - 1L
        } else {
          n_A_s <- n_A_high[s] + n_A_low[s]
          n_B_s <- n_B_high[s] + n_B_low[s]
          p_high_A_s <- if (n_A_s > 0L) n_A_high[s] / n_A_s else 0.5
          p_high_B_s <- if (n_B_s > 0L) n_B_high[s] / n_B_s else 0.5
          mean_A_s <- p_high_A_s * val_A_high[s] +
            (1 - p_high_A_s) * val_A_low[s]
          mean_B_s <- p_high_B_s * val_B_high[s] +
            (1 - p_high_B_s) * val_B_low[s]

          both_A_s <- (obs_A_high[s] >= 0 && obs_A_low[s] >= 0)
          both_B_s <- (obs_B_high[s] >= 0 && obs_B_low[s] >= 0)
          var_A_s <- if (!both_A_s) {
            0.01
          } else {
            p_high_A_s * (mean_A_s - val_A_high[s])^2 +
              (1 - p_high_A_s) * (mean_A_s - val_A_low[s])^2
          }
          var_B_s <- if (!both_B_s) {
            0.01
          } else {
            p_high_B_s * (mean_B_s - val_B_high[s])^2 +
              (1 - p_high_B_s) * (mean_B_s - val_B_low[s])^2
          }
          var_n_A <- if (n_A_s > 0L) var_A_s / n_A_s else var_A_s
          var_n_B <- if (n_B_s > 0L) var_B_s / n_B_s else var_B_s

          cue_value <- if (mean_B_s > mean_A_s) {
            0.5
          } else if (mean_B_s < mean_A_s) {
            -0.5
          } else {
            0.0
          }
          cue_estim <- if (var_n_B > var_n_A) {
            0.5
          } else if (var_n_B < var_n_A) {
            -0.5
          } else {
            0.0
          }
          cue_discover <- if (both_A_s && !both_B_s) {
            0.5
          } else if (!both_A_s && both_B_s) {
            -0.5
          } else {
            cue_estim
          }

          logit_sample_B <- g_int[s] +
            g_val[s] * cue_value +
            g_est[s] * cue_estim +
            g_dis[s] * cue_discover +
            g_swi[s] * cue_switch_vec[s] +
            g_soc[s] * cue_social
          sampled_option <- stats::rbinom(1L, 1L, stats::plogis(logit_sample_B))
        }
        sampled_options_mat[s, t] <- sampled_option

        # ---- 2. Draw outcome and update belief ----
        if (sampled_option == 0L) {
          is_high <- stats::rbinom(1L, 1L, p_A_high)
          if (is_high == 1L) {
            outcome <- v_A_high
            val_A_high[s] <- outcome
            n_A_high[s]   <- n_A_high[s] + 1L
            obs_A_high[s] <- 1
            if (obs_A_low[s] < 0) val_A_low[s] <- outcome
          } else {
            outcome <- v_A_low
            val_A_low[s] <- outcome
            n_A_low[s]   <- n_A_low[s] + 1L
            obs_A_low[s] <- 1
            if (obs_A_high[s] < 0) val_A_high[s] <- outcome
          }
        } else {
          is_high <- stats::rbinom(1L, 1L, p_B_high)
          if (is_high == 1L) {
            outcome <- v_B_high
            val_B_high[s] <- outcome
            n_B_high[s]   <- n_B_high[s] + 1L
            obs_B_high[s] <- 1
            if (obs_B_low[s] < 0) val_B_low[s] <- outcome
          } else {
            outcome <- v_B_low
            val_B_low[s] <- outcome
            n_B_low[s]   <- n_B_low[s] + 1L
            obs_B_low[s] <- 1
            if (obs_B_high[s] < 0) val_B_high[s] <- outcome
          }
        }
        cue_switch_vec[s] <- if (sampled_option == 1L) -0.5 else 0.5
        sampled_outcomes_mat[s, t] <- outcome

        # Social info available to the stopping/choice sub-model: counts
        # visible at the start of the step plus members who have stopped
        # earlier THIS step.
        social_A_for_stop <- social_A_at_t + new_social_A
        social_B_for_stop <- social_B_at_t + new_social_B

        # ---- 3. Beliefs after the update (drive stopping & choice) ----
        n_A <- n_A_high[s] + n_A_low[s]
        n_B <- n_B_high[s] + n_B_low[s]
        p_high_A <- if (n_A > 0L) n_A_high[s] / n_A else 0.5
        p_high_B <- if (n_B > 0L) n_B_high[s] / n_B else 0.5
        mean_A <- p_high_A * val_A_high[s] + (1 - p_high_A) * val_A_low[s]
        mean_B <- p_high_B * val_B_high[s] + (1 - p_high_B) * val_B_low[s]
        both_A <- (obs_A_high[s] >= 0 && obs_A_low[s] >= 0)
        both_B <- (obs_B_high[s] >= 0 && obs_B_low[s] >= 0)
        var_A <- if (!both_A) {
          0.01
        } else {
          p_high_A * (mean_A - val_A_high[s])^2 +
            (1 - p_high_A) * (mean_A - val_A_low[s])^2
        }
        var_B <- if (!both_B) {
          0.01
        } else {
          p_high_B * (mean_B - val_B_high[s])^2 +
            (1 - p_high_B) * (mean_B - val_B_low[s])^2
        }
        se_diff <- if (n_A == 0L) {
          sqrt(var_B / n_B)
        } else if (n_B == 0L) {
          sqrt(var_A / n_A)
        } else {
          sqrt(var_A / n_A + var_B / n_B)
        }
        delta_t <- (mean_B - mean_A) / se_diff

        # Own evidence strength (signed u_t, |u_t| is the magnitude).
        u_t <- tanh(delta_t / k)
        both_seen_either <- (both_A || both_B)
        outcome_z <- (outcome - outcome_mean) / outcome_sd

        # ---- 4. Stopping decision (logit of STOPPING) ---- The cap terminates
        # sampling (terminal step = stop). Otherwise stopping is only possible
        # once BOTH options have been sampled at least once, which implies t >=
        # 2; this matches `can_stop` in the group .stan files.
        do_stop <- (t == max_samples)
        if (!do_stop && n_A > 0L && n_B > 0L) {
          logit_stop <- b_int[s] +
            b_stp[s]  * ((t - t_offset)            / bs[1]) +
            b_trl[s]  * (tn_t                       / bs[2]) +
            b_out[s]  * (outcome_z                  / bs[3]) +
            b_ext[s]  * (abs(outcome_z)             / bs[4]) +
            b_both[s] * (as.numeric(both_seen_either) / bs[5]) +
            b_str[s]  * (abs(u_t)                   / bs[6])
          if (social_A_for_stop > 0L || social_B_for_stop > 0L) {
            social_lo_t <- log((social_B_for_stop + 0.1) /
              (social_A_for_stop + 0.1))
            s_t_stop   <- tanh(social_lo_t)
            align_stop <- u_t * s_t_stop
            logit_stop <- logit_stop +
              b_sstr[s] * (abs(s_t_stop) / ss[1]) +
              b_sint[s] +
              b_conf[s] * (align_stop   / ss[2])
          }
          do_stop <- stats::rbinom(1L, 1L, stats::plogis(logit_stop)) == 1L
        }

        if (do_stop) {
          # ---- 5. Consequential choice ----
          asocial <- delta_t * tau_v[s]
          if (social_A_for_stop == 0L && social_B_for_stop == 0L) {
            p_B_final <- stats::plogis(asocial)
            pi_t      <- NA_real_
            align_t   <- NA_real_
            u_out     <- NA_real_
            s_t       <- NA_real_
          } else {
            social_lo <- log((social_B_for_stop + 0.1) /
              (social_A_for_stop + 0.1))
            s_t       <- tanh(social_lo)
            align_t   <- u_t * s_t
            u_out     <- u_t
            # pi_copy: own & social evidence STRENGTH only, NO alignment term,
            # standardized by the same scales used in the stopping stage.
            pi_t      <- stats::plogis(e_int[s] +
              e_str[s]  * (abs(u_t) / bs[6]) +
              e_sstr[s] * (abs(s_t) / ss[1]))
            social    <- the_v[s] * social_lo
            p_B_final <- (1 - pi_t) * stats::plogis(asocial) +
              pi_t * stats::plogis(social)
          }
          choice_B <- stats::rbinom(1L, 1L, p_B_final)
          if (choice_B == 1L) {
            new_social_B <- new_social_B + 1L
          } else {
            new_social_A <- new_social_A + 1L
          }

          stopped[s]       <- TRUE
          stop_step_vec[s] <- t
          delta_t_stop[s]  <- delta_t
          p_asocial_vec[s] <- stats::plogis(asocial)
          pi_copy_vec[s]   <- pi_t
          align_vec[s]     <- align_t
          u_vec[s] <- if (social_A_for_stop == 0L && social_B_for_stop == 0L) {
            NA_real_
          } else {
            u_out
          }
          s_vec[s]         <- s_t
          seen_social_A[s] <- social_A_for_stop
          seen_social_B[s] <- social_B_for_stop
          final_p_B[s]     <- p_B_final
          final_choice[s]  <- choice_B
        }
      }  # end subject loop

      social_A_count <- social_A_count + new_social_A
      social_B_count <- social_B_count + new_social_B
      if (all(stopped)) break
    }  # end step loop

    choice_order <- as.integer(rank(stop_step_vec, ties.method = "min"))

    for (s in seq_len(group_size)) {
      opts <- sampled_options_mat[s, seq_len(stop_step_vec[s])]
      outs <- sampled_outcomes_mat[s, seq_len(stop_step_vec[s])]
      row_idx <- row_idx + 1L
      results[[row_idx]] <- list(
        trial              = trial,
        subject            = s,
        subject_id         = subject_id_vec[s],
        choice_order       = choice_order[s],
        n_samples          = stop_step_vec[s],
        delta_t_stop       = delta_t_stop[s],
        choice             = final_choice[s],
        choice_label       = if (final_choice[s] == 1L) "B" else "A",
        p_choice_B_asocial = p_asocial_vec[s],
        p_choice_B_final   = final_p_B[s],
        pi_copy            = pi_copy_vec[s],
        align              = align_vec[s],
        u                  = u_vec[s],
        s                  = s_vec[s],
        social_A_seen      = seen_social_A[s],
        social_B_seen      = seen_social_B[s],
        sampling_sequence = paste(
          ifelse(opts == 0L, "A", "B"),
          collapse = ","
        ),
        outcome_sequence   = paste(outs, collapse = ",")
      )
    }
  }

  results <- results[seq_len(row_idx)]
  tibble::tibble(
    trial              = vapply(results, `[[`, integer(1L),   "trial"),
    subject            = vapply(results, `[[`, integer(1L),   "subject"),
    subject_id         = vapply(results, `[[`, character(1L), "subject_id"),
    choice_order       = vapply(results, `[[`, integer(1L),   "choice_order"),
    n_samples          = vapply(results, `[[`, integer(1L),   "n_samples"),
    delta_t_stop       = vapply(results, `[[`, numeric(1L),   "delta_t_stop"),
    choice             = vapply(results, `[[`, integer(1L),   "choice"),
    choice_label       = vapply(results, `[[`, character(1L), "choice_label"),
    p_choice_B_asocial = vapply(
      results, `[[`, numeric(1L), "p_choice_B_asocial"
    ),
    p_choice_B_final = vapply(results, `[[`, numeric(1L), "p_choice_B_final"),
    pi_copy            = vapply(results, `[[`, numeric(1L),   "pi_copy"),
    align              = vapply(results, `[[`, numeric(1L),   "align"),
    u                  = vapply(results, `[[`, numeric(1L),   "u"),
    s                  = vapply(results, `[[`, numeric(1L),   "s"),
    social_A_seen      = vapply(results, `[[`, integer(1L),   "social_A_seen"),
    social_B_seen      = vapply(results, `[[`, integer(1L),   "social_B_seen"),
    sampling_sequence = vapply(
      results, `[[`, character(1L), "sampling_sequence"
    ),
    outcome_sequence = vapply(results, `[[`, character(1L), "outcome_sequence")
  )
}
