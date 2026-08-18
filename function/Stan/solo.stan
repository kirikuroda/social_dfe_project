// Solo condition cognitive model (sampling + stopping + consequential choice).
// The consequential-choice logit is driven by a single standardized value
// signal: the mean difference divided by its standard error,
//
//   logit P(choose B) = tau_mean * delta_t,
//   delta_t = (mean_B - mean_A) / se_diff,
//   se_diff = sqrt(var_A / n_A + var_B / n_B)
//
// delta_t is the same standardized evidence quantity used by the stopping
// stage, so the two stages are consistent. tau_mean is the choice sensitivity
// (inverse temperature) on the standardized scale.
//
// The stopping stage uses the step (t - t_offset), trial_num, signed outcome
// (outcome_z), outcome extremity (|outcome_z|), the both-seen indicator
// (both_seen_either), and own-evidence-strength |u_t| = |tanh(delta_t / k)|
// predictors. Each stopping predictor is divided by its empirical step-level SD
// (beta_scale). The sampling stage has an intercept (gamma_intercept, the
// baseline log-odds of sampling B over A) in addition to the cue terms; the
// gamma cues are on their native {-0.5, 0, +0.5} scale.
//
// Stopping is only possible from the step at which BOTH options have been
// sampled at least once (necessarily t >= 2); earlier steps are not decision
// points and contribute no stopping term. The terminal step t == T is always
// scored as a stop, so trials that end at the task's 40-sample cap count as
// stopping events.
//
// Parameter rows (mu has 13 entries):
//   1  beta_intercept   stopping intercept
//   2  beta_step        (t - t_offset) slope
//   3  beta_trial       trial_num slope
//   4  beta_outcome     outcome_z slope (signed)
//   5  beta_extremity   |outcome_z| slope (extremity)
//   6  beta_both_seen   both_seen_either slope
//   7  beta_strength    |u_t| slope (own evidence strength)
//   8  gamma_intercept  sampling intercept
//   9  gamma_value      cue_value
//  10  gamma_estim      cue_estim
//  11  gamma_discover   cue_discover
//  12  gamma_switch     cue_switch (switching)
//  13  tau_mean         choice slope on delta_t (standardized mean diff; inverse temperature)

functions {
  real partial_sum_lpmf(
    array[] int choice_slice,
    int start,
    int end,
    array[] int seg_offsets,
    array[] int option_sampled,
    array[] int is_higher_outcome,
    vector v_sampled,
    vector v_sampled_z,
    array[] int subj_id,
    vector trial_num,
    real t_offset,
    real k,             // personal-evidence scaling (tanh(delta_t / k))
    vector beta_scale,       // [6] unit-SD scales: step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
    vector beta_intercept,   // stopping intercept
    vector beta_step,        // (t - t_offset) slope
    vector beta_trial,       // trial_num slope
    vector beta_outcome,     // outcome_z slope (signed)
    vector beta_extremity,   // |outcome_z| slope (extremity)
    vector beta_both_seen,   // both_seen_either slope
    vector beta_strength,    // |u_t| slope (own evidence strength)
    vector gamma_intercept,  // sampling intercept
    vector gamma_value,      // sampling: cue_value (binary symmetric)
    vector gamma_estim,      // sampling: cue_estim (binary symmetric)
    vector gamma_discover,   // sampling: cue_discover (binary symmetric)
    vector gamma_switch,     // sampling: cue_switch (binary symmetric)
    vector tau_mean          // choice: delta_t (standardized mean diff; inverse temperature)
  ) {
    real lp = 0;
    for (trial in start:end) {
      int subj    = subj_id[trial];
      int chose_B = choice_slice[trial - start + 1];

      int s_start = seg_offsets[trial];
      int s_end   = seg_offsets[trial + 1];
      int T       = s_end - s_start;

      real obs_A_high = -1;
      real obs_A_low  = -1;
      real obs_B_high = -1;
      real obs_B_low  = -1;

      real n_A_high = 0;
      real n_A_low  = 0;
      real n_B_high = 0;
      real n_B_low  = 0;

      real val_A_high = 0;
      real val_A_low  = 0;
      real val_B_high = 0;
      real val_B_low  = 0;

      real cue_switch = 0.0;  // switching state: +0.5 after sampling A (points to B), -0.5 after B, 0 at t == 1

      for (t in 1:T) {
        int  seg_idx   = s_start + t - 1;
        real outcome   = v_sampled[seg_idx];
        real outcome_z = v_sampled_z[seg_idx];

        // --------------------------------------------------------
        // SAMPLING MODEL (binary symmetric drivers, pre-update):
        //   cue_value, cue_estim, cue_discover, cue_switch ∈ {-0.5, 0, +0.5}
        //     +0.5 = driver points to B, -0.5 = points to A, 0 = tie
        //   t == 1 is fully random by assumption.
        // --------------------------------------------------------
        if (t == 1) {
          lp += bernoulli_lpmf(option_sampled[seg_idx] | 0.5);
        } else {
          real n_A_s = n_A_high + n_A_low;
          real n_B_s = n_B_high + n_B_low;

          real p_high_A_s = n_A_s > 0 ? n_A_high / n_A_s : 0.5;
          real p_high_B_s = n_B_s > 0 ? n_B_high / n_B_s : 0.5;

          real mean_A_s = p_high_A_s * val_A_high + (1 - p_high_A_s) * val_A_low;
          real mean_B_s = p_high_B_s * val_B_high + (1 - p_high_B_s) * val_B_low;

          real var_A_s;
          real var_B_s;
          if (obs_A_high < 0 || obs_A_low < 0) {
            var_A_s = 0.01;
          } else {
            var_A_s = p_high_A_s * square(mean_A_s - val_A_high)
                    + (1 - p_high_A_s) * square(mean_A_s - val_A_low);
          }
          if (obs_B_high < 0 || obs_B_low < 0) {
            var_B_s = 0.01;
          } else {
            var_B_s = p_high_B_s * square(mean_B_s - val_B_high)
                    + (1 - p_high_B_s) * square(mean_B_s - val_B_low);
          }

          real var_n_A = n_A_s > 0 ? var_A_s / n_A_s : var_A_s;
          real var_n_B = n_B_s > 0 ? var_B_s / n_B_s : var_B_s;

          int both_seen_A_s = (obs_A_high >= 0 && obs_A_low >= 0) ? 1 : 0;
          int both_seen_B_s = (obs_B_high >= 0 && obs_B_low >= 0) ? 1 : 0;

          // Symmetric binary drivers (+0.5 = predict B, -0.5 = predict A, 0 = tie)
          real cue_value;
          if (mean_B_s > mean_A_s) cue_value = 0.5;
          else if (mean_B_s < mean_A_s) cue_value = -0.5;
          else cue_value = 0.0;

          real cue_estim;
          if (var_n_B > var_n_A) cue_estim = 0.5;
          else if (var_n_B < var_n_A) cue_estim = -0.5;
          else cue_estim = 0.0;

          // Discover prefers the option that still has unobserved unique
          // outcomes; falls back to cue_estim when both options share state.
          real cue_discover;
          if (both_seen_A_s == 1 && both_seen_B_s == 0) cue_discover = 0.5;
          else if (both_seen_A_s == 0 && both_seen_B_s == 1) cue_discover = -0.5;
          else cue_discover = cue_estim;

          real logit_sample_B = gamma_intercept[subj]
                              + gamma_value[subj] * cue_value
                              + gamma_estim[subj] * cue_estim
                              + gamma_discover[subj] * cue_discover
                              + gamma_switch[subj] * cue_switch;
          lp += bernoulli_logit_lupmf(option_sampled[seg_idx] | logit_sample_B);
        }

        // --------------------------------------------------------
        // UPDATE BELIEFS with observed outcome at step t
        // --------------------------------------------------------
        cue_switch = (option_sampled[seg_idx] == 1) ? -0.5 : 0.5;

        if (option_sampled[seg_idx] == 0) {
          if (is_higher_outcome[seg_idx] == 1) {
            obs_A_high = outcome;
            val_A_high     = outcome;
            if (obs_A_low < 0) val_A_low = outcome;
            n_A_high += 1;
          } else {
            obs_A_low = outcome;
            val_A_low     = outcome;
            if (obs_A_high < 0) val_A_high = outcome;
            n_A_low += 1;
          }
        } else {
          if (is_higher_outcome[seg_idx] == 1) {
            obs_B_high = outcome;
            val_B_high     = outcome;
            if (obs_B_low < 0) val_B_low = outcome;
            n_B_high += 1;
          } else {
            obs_B_low = outcome;
            val_B_low     = outcome;
            if (obs_B_high < 0) val_B_high = outcome;
            n_B_low += 1;
          }
        }

        // --------------------------------------------------------
        // STOPPING MODEL: beliefs after update at step t
        // --------------------------------------------------------
        {
          real n_A = n_A_high + n_A_low;
          real n_B = n_B_high + n_B_low;

          real p_high_A = n_A > 0 ? n_A_high / n_A : 0.5;
          real p_high_B = n_B > 0 ? n_B_high / n_B : 0.5;

          real mean_A = p_high_A * val_A_high + (1 - p_high_A) * val_A_low;
          real mean_B = p_high_B * val_B_high + (1 - p_high_B) * val_B_low;

          real var_A;
          real var_B;
          if (obs_A_high < 0 || obs_A_low < 0) {
            var_A = 0.01;
          } else {
            var_A = p_high_A * square(mean_A - val_A_high)
                  + (1 - p_high_A) * square(mean_A - val_A_low);
          }
          if (obs_B_high < 0 || obs_B_low < 0) {
            var_B = 0.01;
          } else {
            var_B = p_high_B * square(mean_B - val_B_high)
                  + (1 - p_high_B) * square(mean_B - val_B_low);
          }

          real se_diff;
          if (n_A == 0) se_diff = sqrt(var_B / n_B);
          else if (n_B == 0) se_diff = sqrt(var_A / n_A);
          else se_diff = sqrt(var_A / n_A + var_B / n_B);

          real delta_t = (mean_B - mean_A) / se_diff;

          // Own evidence strength: |u_t| = |tanh(delta_t / k)|.
          real u_t = tanh(delta_t / k);

          // 1 if at least one option has had both outcomes observed
          int both_seen_either = ((obs_A_high >= 0 && obs_A_low >= 0) ||
                                  (obs_B_high >= 0 && obs_B_low >= 0)) ? 1 : 0;

          // STOPPING ELIGIBILITY. Sampling can only be terminated voluntarily
          // once BOTH options have been sampled at least once. n_A / n_B are
          // counts AFTER the step-t update, so this implies t >= 2 (at t == 1
          // only one option can have been sampled) and the old `t > 1` guard is
          // subsumed. Steps failing this are not decision points and contribute
          // no stopping term (P(stop) is structurally 0 there).
          // The terminal step t == T is always scored as a stop: sampling also
          // ends when the task's 40-sample cap is reached, and by analysis
          // decision such forced terminations are modeled as stopping events.
          // (In these data the exception only applies at the cap: the four
          // trials that end with a single option sampled all have T == 40.)
          int can_stop = (n_A > 0 && n_B > 0) || (t == T);

          // Each predictor is divided by its empirical (step-level) SD
          // (beta_scale) so the beta coefficients are on a common "per 1 SD"
          // scale. trial_num is z-scored in R but rescaled here too, so its
          // step-level SD matches the other predictors.
          real logit_stop = beta_intercept[subj]
                              + beta_step[subj] * ((t - t_offset) / beta_scale[1])
                              + beta_trial[subj] * (trial_num[trial] / beta_scale[2])
                              + beta_outcome[subj] * (outcome_z / beta_scale[3])
                              + beta_extremity[subj] * (abs(outcome_z) / beta_scale[4])
                              + beta_both_seen[subj] * (both_seen_either / beta_scale[5])
                              + beta_strength[subj] * (abs(u_t) / beta_scale[6]);   // own evidence strength
          if (can_stop) {
            if (t < T) {
              lp += bernoulli_logit_lupmf(0 | logit_stop);  // continued sampling
            } else {
              lp += bernoulli_logit_lupmf(1 | logit_stop);  // stopped here
            }
          }

          // The consequential choice is observed at t == T independently of the
          // stopping term, so it is scored outside the can_stop branch.
          if (t == T) {
            // Choice on the standardized mean difference delta_t (the same
            // se-scaled evidence used by the stopping stage). tau_mean is the
            // inverse temperature on this standardized scale.
            real choice_logit = tau_mean[subj] * delta_t;
            lp += bernoulli_logit_lupmf(chose_B | choice_logit);
          }
        }
      }
    }
    return lp;
  }
}

data {
  int<lower=1> N;       // number of trials
  int<lower=1> N_subj;  // number of subjects
  int<lower=1> N_seg;   // total number of segments across all trials

  // flattened per-segment observations
  array[N_seg] int<lower=0, upper=1>      option_sampled;    // 0=A, 1=B
  array[N_seg] int<lower=0, upper=1>      is_higher_outcome; // 1=higher, 0=lower
  vector[N_seg]                           v_sampled;         // observed outcome values
  vector[N_seg]                           v_sampled_z;       // z-scored v_sampled (standardized in R)

  array[N] int<lower=0, upper=1>          choice;    // consequential choice: 1=B, 0=A
  array[N] int<lower=1, upper=N_subj>     subj_id;   // subject index per trial

  // segment start indices per trial (1-based; length N+1)
  array[N+1] int<lower=1, upper=N_seg+1> seg_offsets;

  // z-scored experiment trial number (z-score in R before passing)
  vector[N] trial_num;

  // Centering offset for t in the stopping model (data-derived: mean of
  // sample_id over steps where both options have been sampled, plus each
  // trial's terminal step); pooled across
  // conditions in R so that beta_intercept has a comparable reference point.
  real t_offset;

  // Personal-evidence scaling for the own-evidence strength term
  // |u_t| = |tanh(delta_t / k)| (data-supplied: median |delta_t| at stopping).
  real<lower=0> k;

  // Empirical step-level SDs (computed in R by replaying this model's belief
  // update) for unit-SD standardization of the stopping predictors.
  //   beta_scale: step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
  vector<lower=0>[6] beta_scale;
}

parameters {
  // Hyperparameters [13 rows total]; see header for row map.
  vector[13]          mu;
  vector<lower=0>[13] sigma;

  // Centered rows: subject params sampled directly (rows with strong likelihood).
  vector[N_subj] beta_intercept;   // row 1
  vector[N_subj] beta_step;   // row 2
  vector[N_subj] gamma_switch;  // row 12 (cue_switch switching)

  // Non-centered rows: z[k] corresponds to one of these rows.
  matrix[10, N_subj] z;
}

transformed parameters {
  vector[N_subj] beta_trial      = mu[3]  + sigma[3]  * z[1]';   // trial_num slope
  vector[N_subj] beta_outcome    = mu[4]  + sigma[4]  * z[2]';   // outcome_z slope (signed)
  vector[N_subj] beta_extremity  = mu[5]  + sigma[5]  * z[3]';   // |outcome_z| slope (extremity)
  vector[N_subj] beta_both_seen  = mu[6]  + sigma[6]  * z[4]';   // both_seen_either slope
  vector[N_subj] beta_strength   = mu[7]  + sigma[7]  * z[5]';   // |u_t| slope (own evidence strength)
  vector[N_subj] gamma_intercept = mu[8]  + sigma[8]  * z[6]';   // sampling intercept
  vector[N_subj] gamma_value     = mu[9]  + sigma[9]  * z[7]';   // sampling: cue_value
  vector[N_subj] gamma_estim     = mu[10] + sigma[10] * z[8]';   // sampling: cue_estim
  vector[N_subj] gamma_discover  = mu[11] + sigma[11] * z[9]';   // sampling: cue_discover
  vector[N_subj] tau_mean        = mu[13] + sigma[13] * z[10]';  // choice: delta_t (standardized mean diff; inverse temperature)
}

model {
  // Hyperpriors
  mu    ~ student_t(4, 0, 2.5);
  sigma ~ student_t(4, 0, 1);

  // Centered subject-level priors for rows with strong likelihood
  beta_intercept ~ normal(mu[1], sigma[1]);
  beta_step      ~ normal(mu[2], sigma[2]);
  gamma_switch   ~ normal(mu[12], sigma[12]);

  // Non-centered z for the remaining rows
  to_vector(z) ~ std_normal();

  // Likelihood via reduce_sum over trials
  target += reduce_sum(
    partial_sum_lpmf,
    choice,
    1,
    seg_offsets,
    option_sampled,
    is_higher_outcome,
    v_sampled,
    v_sampled_z,
    subj_id,
    trial_num,
    t_offset,
    k,
    beta_scale,
    beta_intercept,
    beta_step,
    beta_trial,
    beta_outcome,
    beta_extremity,
    beta_both_seen,
    beta_strength,
    gamma_intercept,
    gamma_value,
    gamma_estim,
    gamma_discover,
    gamma_switch,
    tau_mean
  );
}

generated quantities {
  vector[N] log_lik;

  for (trial in 1:N) {
    log_lik[trial] = partial_sum_lpmf(
      {choice[trial]} | trial, trial,
      seg_offsets, option_sampled, is_higher_outcome, v_sampled, v_sampled_z,
      subj_id, trial_num, t_offset, k, beta_scale,
      beta_intercept, beta_step, beta_trial, beta_outcome, beta_extremity, beta_both_seen, beta_strength,
      gamma_intercept, gamma_value, gamma_estim, gamma_discover, gamma_switch,
      tau_mean
    );
  }
}
