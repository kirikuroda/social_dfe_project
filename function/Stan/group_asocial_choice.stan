// Group condition null model: consequential choice is purely asocial.
//
// Identical to group_constant.stan except the consequential choice always uses
// the asocial channel (pi_copy = 0), regardless of social information.
// Used as a LOO comparison baseline for the mixture models.
//
// Sampling-stage drivers (binary symmetric except cue_social, smoothed
// in (-0.5, +0.5)), plus a baseline sampling intercept (gamma_intercept):
//   cue_value, cue_estim, cue_discover, cue_switch  ∈ {-0.5, 0, +0.5}
//   cue_social  = tanh(log((n_B + 0.1)/(n_A + 0.1))) / 2
//
// The choice stage uses a single standardized value signal, the se-scaled mean
// difference delta_t = (mean_B - mean_A) / se_diff (the same quantity as the
// stopping stage): logit = tau_mean * delta_t, with tau_mean the inverse
// temperature on the standardized scale.
//
// The stopping/sampling stage is identical to group_full.stan (so the four
// group models are comparable under LOO): signed outcome (beta_outcome*outcome_z),
// outcome extremity (beta_extremity*|outcome_z|), both-seen indicator
// (beta_both_seen*both_seen_either), own evidence strength (beta_strength*|u_t|),
// social evidence strength (beta_social_strength*|s_t|), social intercept (beta_social_intercept), and
// own/social alignment (beta_confirm*align_t) where u_t = tanh(delta_t/k),
// s_t = tanh(social_lo_t), align_t = u_t * s_t. k is data-supplied.
//
// Stopping is only possible from the step at which BOTH options have been
// sampled at least once (necessarily t >= 2); earlier steps are not decision
// points and contribute no stopping term. The terminal step t == T is always
// scored as a stop, so trials that end at the task's 40-sample cap count as
// stopping events. Same rule as solo.stan.
//
// Parameter rows (mu has 17 entries):
//   1  beta_intercept         stopping intercept
//   2  beta_step              (t - t_offset) slope
//   3  beta_trial             trial_num slope
//   4  beta_outcome           outcome_z slope (signed)
//   5  beta_extremity         |outcome_z| slope (extremity)
//   6  beta_both_seen         both_seen_either slope
//   7  beta_strength          |u_t| slope (own evidence strength)
//   8  beta_social_strength   |s_t| slope (social evidence strength)
//   9  beta_social_intercept  social-condition intercept
//  10  beta_confirm           align_t slope (confirmation, stopping)
//  11  gamma_intercept        sampling intercept
//  12  gamma_value            cue_value
//  13  gamma_estim            cue_estim
//  14  gamma_discover         cue_discover
//  15  gamma_switch           cue_switch (switching)
//  16  gamma_social           cue_social (smoothed)
//  17  tau_mean               asocial choice slope on delta_t (standardized mean diff; inverse temperature)

functions {
  // Wrapper for reduce_sum: accumulates log-likelihood over a slice of trials.
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
    array[] real a_chosen,
    array[] real b_chosen,
    vector trial_num,              // experiment trial number (z-scored)
    real t_offset,            // centering offset for t in stopping model
    real k,                   // personal-evidence scaling (tanh(delta_t / k))
    vector beta_scale,             // [6] unit-SD scales: step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
    vector social_scale,           // [2] unit-SD scales: |s_t|, align
    vector beta_intercept,         // stopping intercept
    vector beta_step,              // (t - t_offset) slope
    vector beta_trial,             // trial_num slope
    vector beta_outcome,           // outcome_z slope (signed)
    vector beta_extremity,         // |outcome_z| slope (extremity)
    vector beta_both_seen,         // both_seen_either slope
    vector beta_strength,          // |u_t| slope (own evidence strength)
    vector beta_social_strength,   // |s_t| slope (social evidence strength)
    vector beta_social_intercept,  // social-condition intercept
    vector beta_confirm,           // align_t slope (confirmation, stopping)
    vector gamma_intercept,        // sampling intercept
    vector gamma_value,            // sampling: cue_value (binary symmetric)
    vector gamma_estim,            // sampling: cue_estim (binary symmetric)
    vector gamma_discover,         // sampling: cue_discover (binary symmetric)
    vector gamma_switch,           // sampling: cue_switch (binary symmetric)
    vector gamma_social,           // sampling: cue_social (smoothed, in (-0.5, +0.5))
    vector tau_mean                // choice: delta_t (standardized mean diff; inverse temperature)
  ) {
    real lp = 0;
    for (trial in start:end) {
      int subj    = subj_id[trial];
      int chose_B = choice_slice[trial - start + 1];

      int s_start = seg_offsets[trial];
      int s_end   = seg_offsets[trial + 1];
      int T       = s_end - s_start;

      // Track observed outcome values; -1 = not yet observed
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

      real social_A = 0;  // cumulative social choice count for A
      real social_B = 0;  // cumulative social choice count for B

      real cue_switch = 0.0;  // switching state: +0.5 after sampling A (points to B), -0.5 after B, 0 at t == 1

      for (t in 1:T) {
        int  seg_idx   = s_start + t - 1;
        real outcome   = v_sampled[seg_idx];
        real outcome_z = v_sampled_z[seg_idx];

        // --------------------------------------------------------
        // SAMPLING MODEL (binary symmetric drivers, pre-belief-update):
        //   cue_value, cue_estim, cue_discover, cue_switch ∈ {-0.5, 0, +0.5}
        //     +0.5 = driver points to B, -0.5 = points to A, 0 = tie
        //   cue_social ∈ (-0.5, +0.5), smoothed tanh of cumulative
        //     social log-odds through segment t-1.
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

          // Smoothed cumulative social info through segment t-1
          // (segment-t increment is added after the sampling decision).
          // When social_A = social_B = 0, log-odds = 0 so cue_social = 0.
          real social_lo_pre = log((social_B + 0.1) / (social_A + 0.1));
          real cue_social    = tanh(social_lo_pre) / 2;

          real logit_sample_B = gamma_intercept[subj]
                              + gamma_value[subj] * cue_value
                              + gamma_estim[subj] * cue_estim
                              + gamma_discover[subj] * cue_discover
                              + gamma_switch[subj] * cue_switch
                              + gamma_social[subj] * cue_social;
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

        // Segment t's social info is revealed alongside the outcome,
        // so the increment is added after the sampling decision (and
        // belief update). The stopping sub-model below sees it; the
        // sampling sub-model above does not.
        social_A += a_chosen[seg_idx];
        social_B += b_chosen[seg_idx];

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

          // Variance of each option's outcome distribution
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

          // SE of the difference in sample means (at least one of n_A, n_B > 0)
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

          // STOPPING ELIGIBILITY (same rule as solo.stan). Sampling can only be
          // terminated voluntarily once BOTH options have been sampled at least
          // once; n_A / n_B are counts AFTER the step-t update, so this implies
          // t >= 2 and subsumes the old `t > 1` guard. Ineligible steps are not
          // decision points and contribute no stopping term. The terminal step
          // t == T is always scored as a stop, covering trials that end at the
          // task's 40-sample cap.
          int can_stop = (n_A > 0 && n_B > 0) || (t == T);

          // Logit of stopping (1 = stop, 0 = continue)
          // Predictors divided by their unit-SD scales (see header).
          real logit_stop = beta_intercept[subj]
                              + beta_step[subj] * ((t - t_offset) / beta_scale[1])
                              + beta_trial[subj] * (trial_num[trial] / beta_scale[2])
                              + beta_outcome[subj] * (outcome_z / beta_scale[3])
                              + beta_extremity[subj] * (abs(outcome_z) / beta_scale[4])
                              + beta_both_seen[subj] * (both_seen_either / beta_scale[5])
                              + beta_strength[subj] * (abs(u_t) / beta_scale[6]);   // own evidence strength
          if (social_A > 0 || social_B > 0) {
            real social_lo_t = log((social_B + 0.1) / (social_A + 0.1));
            real s_t         = tanh(social_lo_t);
            real align       = u_t * s_t;
            logit_stop += beta_social_strength[subj] * (abs(s_t) / social_scale[1])   // social evidence strength
                            + beta_social_intercept[subj]
                            + beta_confirm[subj] * (align / social_scale[2]);
          }

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
            // Consequential choice: purely asocial (pi_copy = 0). Single
            // standardized value signal delta_t = (mean_B - mean_A) / se_diff.
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
  int<lower=1> N_group; // number of groups
  int<lower=1> N_seg;   // total number of segments across all trials

  array[N_subj] int<lower=1, upper=N_group> group_id;  // group index per subject

  // flattened per-segment observations
  array[N_seg] int<lower=0, upper=1>      option_sampled;    // 0=A, 1=B
  array[N_seg] int<lower=0, upper=1>      is_higher_outcome; // 1=higher, 0=lower
  vector[N_seg]                           v_sampled;         // observed outcome values
  vector[N_seg]                           v_sampled_z;       // z-scored v_sampled (standardized in R)

  array[N] int<lower=0, upper=1>          choice;    // consequential choice: 1=B, 0=A
  array[N] int<lower=1, upper=N_subj>     subj_id;   // subject index per trial

  // segment start indices per trial (1-based; length N+1)
  array[N+1] int<lower=1, upper=N_seg+1> seg_offsets;
  array[N_seg] real<lower=0>             a_chosen;
  array[N_seg] real<lower=0>             b_chosen;

  // z-scored experiment trial number (z-score in R before passing)
  vector[N] trial_num;

  // Centering offset for t in the stopping model (data-derived: mean of
  // sample_id over steps where both options have been sampled, plus each
  // trial's terminal step); pooled across
  // conditions in R so that beta_intercept has a comparable reference point.
  real t_offset;

  // Scaling constants for the alignment term (see header).
  real<lower=0> k;

  // Unit-SD standardization scales for the stopping predictors (beta_scale
  // pooled with solo.stan) plus group-only social_scale. Each beta/social
  // coefficient is on a common "per 1 SD" scale and directly comparable.
  //   beta_scale:   step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
  //   social_scale: |s_t|, align
  vector<lower=0>[6] beta_scale;
  vector<lower=0>[2] social_scale;
}

parameters {
  // Hyperparameters [17 rows total]; see header for row map.
  vector[17]          mu;
  vector<lower=0>[17] sigma_group;  // group-level standard deviations
  vector<lower=0>[17] sigma;        // individual-level standard deviations

  // Group-level non-centered deviations (always NCP)
  matrix[17, N_group] z_group;

  // Centered subject-level rows (rows with strong likelihood):
  //   row 1  = beta_intercept
  //   row 2  = beta_step
  //   row 15 = gamma_switch (cue_switch switching)
  vector[N_subj] beta_intercept;
  vector[N_subj] beta_step;
  vector[N_subj] gamma_switch;

  // Non-centered z for the remaining 14 rows
  matrix[14, N_subj] z;
}

transformed parameters {
  // Group-level deviations from population mean (non-centered)
  matrix[17, N_group] group_effects = diag_pre_multiply(sigma_group, z_group);

  // Per-subject effective mean (population mean + their group's deviation)
  matrix[17, N_subj] mu_subj = rep_matrix(mu, N_subj)
                             + group_effects[, group_id];

  // Non-centered subject-level parameters
  vector[N_subj] beta_trial            = mu_subj[3]'  + sigma[3]  * z[1]';   // trial_num slope
  vector[N_subj] beta_outcome          = mu_subj[4]'  + sigma[4]  * z[2]';   // outcome_z slope (signed)
  vector[N_subj] beta_extremity        = mu_subj[5]'  + sigma[5]  * z[3]';   // |outcome_z| slope (extremity)
  vector[N_subj] beta_both_seen        = mu_subj[6]'  + sigma[6]  * z[4]';   // both_seen_either slope
  vector[N_subj] beta_strength         = mu_subj[7]'  + sigma[7]  * z[5]';   // |u_t| slope (own evidence strength)
  vector[N_subj] beta_social_strength  = mu_subj[8]'  + sigma[8]  * z[6]';   // |s_t| slope (social evidence strength)
  vector[N_subj] beta_social_intercept = mu_subj[9]'  + sigma[9]  * z[7]';   // social-condition intercept
  vector[N_subj] beta_confirm          = mu_subj[10]' + sigma[10] * z[8]';   // align_t slope (confirmation)
  vector[N_subj] gamma_intercept       = mu_subj[11]' + sigma[11] * z[9]';   // sampling intercept
  vector[N_subj] gamma_value           = mu_subj[12]' + sigma[12] * z[10]';  // sampling: cue_value
  vector[N_subj] gamma_estim           = mu_subj[13]' + sigma[13] * z[11]';  // sampling: cue_estim
  vector[N_subj] gamma_discover        = mu_subj[14]' + sigma[14] * z[12]';  // sampling: cue_discover
  vector[N_subj] gamma_social          = mu_subj[16]' + sigma[16] * z[13]';  // sampling: cue_social (smoothed)
  vector[N_subj] tau_mean              = mu_subj[17]' + sigma[17] * z[14]';  // choice: delta_t (standardized mean diff; inverse temperature)
}

model {
  // Hyperpriors
  mu          ~ student_t(4, 0, 2.5);
  sigma_group ~ student_t(4, 0, 1);
  sigma       ~ student_t(4, 0, 1);

  // Non-centered group-level deviations
  to_vector(z_group) ~ std_normal();

  // Non-centered z for the remaining subject-level rows
  to_vector(z) ~ std_normal();

  // Centered subject-level priors for rows with strong likelihood
  beta_intercept  ~ normal(mu_subj[1]',  sigma[1]);
  beta_step  ~ normal(mu_subj[2]',  sigma[2]);
  gamma_switch ~ normal(mu_subj[15]', sigma[15]);

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
    a_chosen,
    b_chosen,
    trial_num,
    t_offset,
    k,
    beta_scale,
    social_scale,
    beta_intercept,
    beta_step,
    beta_trial,
    beta_outcome,
    beta_extremity,
    beta_both_seen,
    beta_strength,
    beta_social_strength,
    beta_social_intercept,
    beta_confirm,
    gamma_intercept,
    gamma_value,
    gamma_estim,
    gamma_discover,
    gamma_switch,
    gamma_social,
    tau_mean
  );
}

generated quantities {
  vector[N] log_lik;

  for (trial in 1:N) {
    log_lik[trial] = partial_sum_lpmf(
      {choice[trial]} | trial, trial,
      seg_offsets, option_sampled, is_higher_outcome, v_sampled, v_sampled_z,
      subj_id, a_chosen, b_chosen, trial_num, t_offset, k,
      beta_scale, social_scale,
      beta_intercept, beta_step, beta_trial, beta_outcome, beta_extremity, beta_both_seen, beta_strength, beta_social_strength, beta_social_intercept, beta_confirm,
      gamma_intercept, gamma_value, gamma_estim, gamma_discover, gamma_switch, gamma_social,
      tau_mean
    );
  }
}
