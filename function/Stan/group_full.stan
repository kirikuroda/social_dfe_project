// Group-condition model. The stopping stage uses an own/social alignment
// (confirmation) term, but the consequential-choice (pi_copy) stage does NOT: the
// choice-stage alignment term is not separately identifiable when own and
// social evidence agree (both decision channels predict the same choice) and
// adds no out-of-sample predictive value, so it is omitted here.
//
// Stopping (logit of stopping):
//   ... + beta_strength*|u_t|  (+ social branch) + beta_social_strength*|s_t|
//                                          + beta_social_intercept + beta_confirm*align_t
//
// Consequential choice (pi_copy logit), no alignment term:
//   logit(pi_copy_t) = eta_intercept + eta_strength*|u_t| + eta_social_strength*|s_t|
//
//   u_t            = tanh(delta_t / k)                       (signed)
//   s_t            = tanh(social_lo_t)                       (signed)
//   align_t        = u_t * s_t                               in (-1, +1)   (stopping only)
//
// The asocial value channel matches solo.stan: a single standardized value
// signal asocial = tau_mean * delta_t,  delta_t = (mean_B - mean_A) / se_diff.
//
// Stopping is only possible from the step at which BOTH options have been
// sampled at least once (necessarily t >= 2); earlier steps are not decision
// points and contribute no stopping term. The terminal step t == T is always
// scored as a stop, so trials that end at the task's 40-sample cap count as
// stopping events. Same rule as solo.stan.
//
// Parameter rows (mu has 21 entries; theta is row 21, no choice-stage
// eta_confirm):
//   1  beta_intercept         stopping intercept
//   2  beta_step              (t - t_offset) slope
//   3  beta_trial             trial_num slope
//   4  beta_outcome           outcome_z slope (signed)
//   5  beta_extremity         |outcome_z| slope (extremity)
//   6  beta_both_seen         both_seen_either slope
//   7  beta_strength          |u_t| slope (own evidence strength, stopping)
//   8  beta_social_strength   |s_t| slope (social evidence strength, stopping)
//   9  beta_social_intercept  social-condition intercept
//  10  beta_confirm           align_t slope (confirmation, STOPPING ONLY)
//  11  gamma_intercept        sampling intercept
//  12  gamma_value            cue_value
//  13  gamma_estim            cue_estim
//  14  gamma_discover         cue_discover
//  15  gamma_switch           cue_switch (switching)
//  16  gamma_social           cue_social (smoothed)
//  17  tau_mean               asocial choice slope on delta_t (inverse temperature)
//  18  eta_intercept          pi_copy logit intercept
//  19  eta_strength           pi_copy logit slope on |u_t| (own evidence strength)
//  20  eta_social_strength    pi_copy logit slope on |s_t| (social evidence strength)
//  21  theta                  social log-odds weight

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
    vector trial_num,
    real t_offset,
    real k,
    vector beta_scale,             // [6] unit-SD scales: step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
    vector social_scale,           // [2] unit-SD scales: |s_t|, align
    vector beta_intercept,
    vector beta_step,
    vector beta_trial,
    vector beta_outcome,
    vector beta_extremity,
    vector beta_both_seen,
    vector beta_strength,
    vector beta_social_strength,
    vector beta_social_intercept,
    vector beta_confirm,
    vector gamma_intercept,
    vector gamma_value,
    vector gamma_estim,
    vector gamma_discover,
    vector gamma_switch,
    vector gamma_social,
    vector tau_mean,
    vector eta_intercept,
    vector eta_strength,
    vector eta_social_strength,
    vector theta
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

      real social_A = 0;
      real social_B = 0;

      real cue_switch = 0.0;

      for (t in 1:T) {
        int  seg_idx   = s_start + t - 1;
        real outcome   = v_sampled[seg_idx];
        real outcome_z = v_sampled_z[seg_idx];

        // --------------------------------------------------------
        // SAMPLING MODEL (binary symmetric drivers, pre-belief-update)
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

          real cue_value;
          if (mean_B_s > mean_A_s) cue_value = 0.5;
          else if (mean_B_s < mean_A_s) cue_value = -0.5;
          else cue_value = 0.0;

          real cue_estim;
          if (var_n_B > var_n_A) cue_estim = 0.5;
          else if (var_n_B < var_n_A) cue_estim = -0.5;
          else cue_estim = 0.0;

          real cue_discover;
          if (both_seen_A_s == 1 && both_seen_B_s == 0) cue_discover = 0.5;
          else if (both_seen_A_s == 0 && both_seen_B_s == 1) cue_discover = -0.5;
          else cue_discover = cue_estim;

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

          // Own evidence strength: signed u_t = tanh(delta_t / k); the unsigned
          // |u_t| is the own-evidence main effect. Computed every step (delta_t
          // and k are always available) so it drives the stopping main effect
          // (beta_strength * |u_t|) AND, at t == T, the copy stage below.
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

          // Predictors divided by their unit-SD scales (see header).
          real logit_stop = beta_intercept[subj]
                              + beta_step[subj] * ((t - t_offset) / beta_scale[1])
                              + beta_trial[subj] * (trial_num[trial] / beta_scale[2])
                              + beta_outcome[subj] * (outcome_z / beta_scale[3])
                              + beta_extremity[subj] * (abs(outcome_z) / beta_scale[4])
                              + beta_both_seen[subj] * (both_seen_either / beta_scale[5])
                              + beta_strength[subj] * (abs(u_t) / beta_scale[6]);   // own evidence strength

          // Social evidence strength (|s_t|) and own/social alignment
          // (align = u_t * s_t, signed) drive the STOPPING decision only; the
          // t == T copy stage below does not use align.
          real s_t   = 0;  // signed social evidence (in scope for t == T)
          real align = 0;
          if (social_A > 0 || social_B > 0) {
            real social_lo_t = log((social_B + 0.1) / (social_A + 0.1));
            s_t              = tanh(social_lo_t);
            align            = u_t * s_t;
            logit_stop  += beta_social_strength[subj] * (abs(s_t) / social_scale[1])   // social evidence strength
                             + beta_social_intercept[subj]
                             + beta_confirm[subj] * (align / social_scale[2]);
          }

          if (can_stop) {
            if (t < T) {
              lp += bernoulli_logit_lupmf(0 | logit_stop);
            } else {
              lp += bernoulli_logit_lupmf(1 | logit_stop);
            }
          }

          // The consequential choice is observed at t == T independently of the
          // stopping term, so it is scored outside the can_stop branch.
          if (t == T) {
            // Asocial value signal: single standardized value signal
            //   asocial = tau_mean * delta_t,  delta_t = (mean_B - mean_A)/se_diff.
            real asocial = tau_mean[subj] * delta_t;
            if (social_A == 0 && social_B == 0) {
              lp += bernoulli_logit_lupmf(chose_B | asocial);
            } else {
              real social_lo = log((social_B + 0.1) / (social_A + 0.1));
              // Copy propensity driven by own evidence strength (|u_t|) and
              // social evidence strength (|s_t|) ONLY; alignment enters the
              // stopping stage above but not here.
              real pi_logit = eta_intercept[subj]
                            + eta_strength[subj] * (abs(u_t) / beta_scale[6])
                            + eta_social_strength[subj] * (abs(s_t) / social_scale[1]);
              real pi       = inv_logit(pi_logit);
              real social   = theta[subj] * social_lo;
              lp += bernoulli_lupmf(chose_B | (1 - pi) * inv_logit(asocial)
                                             + pi * inv_logit(social));
            }
          }
        }
      }
    }
    return lp;
  }
}

data {
  int<lower=1> N;
  int<lower=1> N_subj;
  int<lower=1> N_group;
  int<lower=1> N_seg;

  array[N_subj] int<lower=1, upper=N_group> group_id;

  array[N_seg] int<lower=0, upper=1>      option_sampled;
  array[N_seg] int<lower=0, upper=1>      is_higher_outcome;
  vector[N_seg]                           v_sampled;
  vector[N_seg]                           v_sampled_z;

  array[N] int<lower=0, upper=1>          choice;
  array[N] int<lower=1, upper=N_subj>     subj_id;

  array[N+1] int<lower=1, upper=N_seg+1> seg_offsets;
  array[N_seg] real<lower=0>             a_chosen;
  array[N_seg] real<lower=0>             b_chosen;

  vector[N] trial_num;
  real t_offset;
  real<lower=0> k;

  // Unit-SD standardization scales for the stopping predictors (pooled with
  // solo.stan), plus group-only social_scale. eta_strength |u_t| uses
  // beta_scale[6]; eta_social_strength |s_t| uses social_scale[1]. social_scale[2]
  // (align) is still used by the stopping beta_confirm term.
  //   beta_scale:   step, trial_num, outcome_z, |outcome_z|, both_seen, |u_t|
  //   social_scale: |s_t|, align
  vector<lower=0>[6] beta_scale;
  vector<lower=0>[2] social_scale;
}

parameters {
  // Hyperparameters [21 rows total]; see header for row map.
  vector[21]          mu;
  vector<lower=0>[21] sigma_group;
  vector<lower=0>[21] sigma;

  // Group-level non-centered deviations
  matrix[21, N_group] z_group;

  // Centered subject-level rows (same rows centered as in solo.stan):
  vector[N_subj] beta_intercept;
  vector[N_subj] beta_step;
  vector[N_subj] gamma_switch;

  // Non-centered z for the remaining 18 rows
  matrix[18, N_subj] z;
}

transformed parameters {
  matrix[21, N_group] group_effects = diag_pre_multiply(sigma_group, z_group);
  matrix[21, N_subj] mu_subj = rep_matrix(mu, N_subj)
                             + group_effects[, group_id];

  vector[N_subj] beta_trial            = mu_subj[3]'  + sigma[3]  * z[1]';
  vector[N_subj] beta_outcome          = mu_subj[4]'  + sigma[4]  * z[2]';   // outcome_z slope (signed)
  vector[N_subj] beta_extremity        = mu_subj[5]'  + sigma[5]  * z[3]';   // |outcome_z| slope (extremity)
  vector[N_subj] beta_both_seen        = mu_subj[6]'  + sigma[6]  * z[4]';   // both_seen_either slope
  vector[N_subj] beta_strength         = mu_subj[7]'  + sigma[7]  * z[5]';   // |u_t| slope (own evidence strength)
  vector[N_subj] beta_social_strength  = mu_subj[8]'  + sigma[8]  * z[6]';   // |s_t| slope (social evidence strength)
  vector[N_subj] beta_social_intercept = mu_subj[9]'  + sigma[9]  * z[7]';   // social-condition intercept
  vector[N_subj] beta_confirm          = mu_subj[10]' + sigma[10] * z[8]';   // align_t slope (confirmation, stopping)
  vector[N_subj] gamma_intercept       = mu_subj[11]' + sigma[11] * z[9]';   // sampling intercept
  vector[N_subj] gamma_value           = mu_subj[12]' + sigma[12] * z[10]';
  vector[N_subj] gamma_estim           = mu_subj[13]' + sigma[13] * z[11]';
  vector[N_subj] gamma_discover        = mu_subj[14]' + sigma[14] * z[12]';
  vector[N_subj] gamma_social          = mu_subj[16]' + sigma[16] * z[13]';
  vector[N_subj] tau_mean              = mu_subj[17]' + sigma[17] * z[14]';  // choice: delta_t (inverse temperature)
  vector[N_subj] eta_intercept         = mu_subj[18]' + sigma[18] * z[15]';
  vector[N_subj] eta_strength          = mu_subj[19]' + sigma[19] * z[16]';  // |u_t| slope (own evidence strength)
  vector[N_subj] eta_social_strength   = mu_subj[20]' + sigma[20] * z[17]';  // |s_t| slope (social evidence strength)
  vector[N_subj] theta                 = mu_subj[21]' + sigma[21] * z[18]';  // social log-odds weight
}

model {
  mu          ~ student_t(4, 0, 2.5);
  sigma_group ~ student_t(4, 0, 1);
  sigma       ~ student_t(4, 0, 1);

  to_vector(z_group) ~ std_normal();
  to_vector(z)       ~ std_normal();

  beta_intercept  ~ normal(mu_subj[1]',  sigma[1]);
  beta_step  ~ normal(mu_subj[2]',  sigma[2]);
  gamma_switch ~ normal(mu_subj[15]', sigma[15]);

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
    tau_mean,
    eta_intercept,
    eta_strength,
    eta_social_strength,
    theta
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
      tau_mean, eta_intercept, eta_strength, eta_social_strength, theta
    );
  }
}
