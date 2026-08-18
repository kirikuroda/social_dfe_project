# Posterior summary and model comparison

library(tidyverse)
library(brms)
library(marginaleffects)
library(here)
library(tidybayes)
load(here("data/data.rda"))


# Logistic regression predicting choice from n_samples,
# with interactions between n_samples, trial_type, and rare_outcome.

slope_choice_solo <- slopes(
  readRDS(here("output/fit/fit_choice_solo_brms.rds")),
  variables  = "n_samples",
  newdata    = datagrid(
    rare_outcome = c("large", "small"),
    trial_type   = c("risky_safe", "risky_risky")
  ),
  re_formula = NA,
  type       = "link"
)

slope_choice_solo

# rare_outcome  trial_type Estimate   2.5 % 97.5 %
#         large risky_safe    -0.349 -0.5416 -0.173
#         large risky_risky    0.332  0.1100  0.542
#         small risky_safe     0.374  0.1970  0.566
#         small risky_risky    0.282  0.0512  0.522

# Term: n_samples
# Type: link
# Comparison: dY/dX

saveRDS(slope_choice_solo, here("output/fit/slope_choice_solo.rds"))


# Negative-binomial regression predicting n_samples from condition,
# with interactions between condition, trial_type, and rare_outcome.

avg_n_samples <- avg_comparisons(
  readRDS(here("output/fit/fit_n_samples_brms.rds")),
  variables  = "condition",
  re_formula = NA,
  type = "link"
)

avg_n_samples

#  Estimate  2.5 %  97.5 %
#    -0.158 -0.287 -0.0224

# Term: condition
# Type: link
# Comparison: group - solo

avg_n_samples_trial_type <- avg_comparisons(
  readRDS(here("output/fit/fit_n_samples_brms.rds")),
  variables  = c("trial_type"),
  re_formula = NA,
  type = "link"
)

avg_n_samples_trial_type

#  Estimate  2.5 %  97.5 %
#    -0.049 -0.068 -0.0296

# Term: trial_type
# Type: link
# Comparison: risky_risky - risky_safe

avg_rare_experience <- avg_comparisons(
  readRDS(here("output/fit/fit_rare_experience_brms.rds")),
  variables  = "condition",
  re_formula = NA,
  type = "link"
)

avg_rare_experience

#  Estimate  2.5 %  97.5 %
#    -0.201 -0.397 -0.0041

# Term: condition
# Type: link
# Comparison: group - solo

# Extract parameters from the solo-condition model -----------------------------

fit_solo <- readRDS(here("output/fit/fit_solo.rds"))

# solo.stan now models the stopping stage directly as P(stop) (outcome 1 =
# stop), so the beta rows (i = 1..7) are already in the P(stop) convention and
# need no sign flip. gamma (i = 8..12) and tau (i = 13) are unchanged.
# Downstream code reads these draws directly.
pop_draws_solo <- spread_draws(fit_solo, mu[i], sigma[i])

saveRDS(pop_draws_solo, here("output/fit/pop_draws_solo.rds"))

pop_draws_solo |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(across(c(mu, .lower, .upper), \(x) round(x, 2)))

# # A tibble: 13 × 7 (betas i=1..7 = effect on P(stop); gamma/tau as-is)
#        i    mu .lower .upper .width .point .interval
#    <int> <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
#  1     1 -0.73  -1.38  -0.06    0.95 median hdci
#  2     2  3.33   2.74   3.89    0.95 median hdci
#  3     3  0.14   0.04   0.23    0.95 median hdci
#  4     4  0.21   0.14   0.28    0.95 median hdci
#  5     5 -0.18  -0.24  -0.12    0.95 median hdci
#  6     6  0.04  -0.05   0.13    0.95 median hdci
#  7     7  0.45   0.37   0.53    0.95 median hdci
#  8     8  0.00  -0.03   0.03    0.95 median hdci
#  9     9  0.21   0.05   0.37    0.95 median hdci
# 10    10  0.31   0.11   0.52    0.95 median hdci
# 11    11  1.25   0.93   1.54    0.95 median hdci
# 12    12  3.68   2.57   4.83    0.95 median hdci
# 13    13  0.89   0.77   1.02    0.95 median hdci

# beta-only summary (stopping-stage parameters; i = 1..7 in solo.stan).
# Betas are in the P(stop) convention (modeled directly in solo.stan).
pop_draws_solo |>
  filter(i %in% 1:7) |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(param = c(
    "beta_intercept (stopping intercept)",
    "beta_step",
    "beta_trial",
    "beta_outcome",
    "beta_extremity",
    "beta_both_seen",
    "beta_personal"
  )) |>
  relocate(param, .after = i) |>
  mutate(
    mu = round(mu, 2),
    .lower = round(.lower, 2),
    .upper = round(.upper, 2)
  )

# # A tibble: 7 × 8 (betas: effect on P(stop))
#       i param                                  mu .lower .upper .width .point .interval
#   <int> <chr>                               <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1     1 beta_intercept (stopping intercept) -0.73  -1.38  -0.06   0.95 median hdci
# 2     2 beta_step                            3.33   2.74   3.89   0.95 median hdci
# 3     3 beta_trial                           0.14   0.04   0.23   0.95 median hdci
# 4     4 beta_outcome                         0.21   0.14   0.28   0.95 median hdci
# 5     5 beta_extremity                      -0.18  -0.24  -0.12   0.95 median hdci
# 6     6 beta_both_seen                       0.04  -0.05   0.13   0.95 median hdci
# 7     7 beta_personal                        0.45   0.37   0.53   0.95 median hdci

# gamma-only summary (sampling-stage parameters; i = 8..12 in solo.stan)
pop_draws_solo |>
  filter(i %in% 8:12) |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(param = c(
    "gamma_intercept (sampling intercept)",
    "gamma_value",
    "gamma_estim",
    "gamma_discover",
    "gamma_switch"
  )) |>
  relocate(param, .after = i) |>
  mutate(
    mu = round(mu, 2),
    .lower = round(.lower, 2),
    .upper = round(.upper, 2)
  )

# # A tibble: 5 × 8
#       i param                                   mu .lower .upper .width .point .interval
#   <int> <chr>                                <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1     8 gamma_intercept (sampling intercept)  0.00  -0.03   0.03  0.95 median hdci
# 2     9 gamma_value                           0.21   0.05   0.37  0.95 median hdci
# 3    10 gamma_estim                           0.31   0.11   0.52  0.95 median hdci
# 4    11 gamma_discover                        1.25   0.93   1.54  0.95 median hdci
# 5    12 gamma_switch                          3.68   2.57   4.83  0.95 median hdci


# Extract parameters from the group model (reduced: no choice-stage alignment) -
# group_full.stan (21 mu rows): pi_copy logit driven by eta_personal
# (|u_t|, own-evidence strength, i = 19) and eta_social (|s_t|, social
# strength, i = 20); plus eta_intercept (i = 18) and theta (i = 21). The
# choice-stage confirmation term (eta_confirm) is DROPPED; the stopping stage
# still has beta_alignment (i = 10). Rows 1..16 are the stopping (beta, i = 1..10)
# and sampling (gamma, i = 11..16) parameters; tau_mean (asocial sensitivity) is
# i = 17. (Variable kept as pop_draws_group_full for downstream compatibility.)

fit_group_full <- readRDS(here("output/fit/fit_group_full.rds"))

# group_full.stan now models the stopping stage directly as P(stop) (outcome
# 1 = stop), so the beta rows (i = 1..10) are already in the P(stop) convention
# and need no sign flip (see the solo block). gamma (i = 11..16), tau (i = 17),
# eta (i = 18..20) and theta (i = 21) are unchanged.
pop_draws_group_full <- spread_draws(fit_group_full, mu[i], sigma[i])

saveRDS(pop_draws_group_full, here("output/fit/pop_draws_group_full.rds"))

# beta-only summary (stopping-stage parameters; i = 1..10 in group_full.stan).
# Betas are in the P(stop) convention (modeled directly in group_full.stan).
pop_draws_group_full |>
  filter(i %in% 1:10) |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(param = c(
    "beta_intercept (stopping intercept)",
    "beta_step",
    "beta_trial",
    "beta_outcome",
    "beta_extremity",
    "beta_both_seen",
    "beta_personal",
    "beta_social",
    "beta_social_intercept",
    "beta_alignment"
  )) |>
  relocate(param, .after = i) |>
  mutate(
    mu = round(mu, 2),
    .lower = round(.lower, 2),
    .upper = round(.upper, 2)
  )

# # A tibble: 10 × 8 (betas: effect on P(stop))
#        i param                                  mu .lower .upper .width .point .interval
#    <int> <chr>                               <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
#  1     1 beta_intercept (stopping intercept)  2.16   0.77   3.53  0.95 median hdci
#  2     2 beta_step                            5.46   4.50   6.46  0.95 median hdci
#  3     3 beta_trial                          -0.16  -0.35   0.03  0.95 median hdci
#  4     4 beta_outcome                         0.20   0.12   0.29  0.95 median hdci
#  5     5 beta_extremity                      -0.09  -0.16  -0.02  0.95 median hdci
#  6     6 beta_both_seen                       0.17   0.07   0.28  0.95 median hdci
#  7     7 beta_personal                        0.39   0.29   0.49  0.95 median hdci
#  8     8 beta_social                          0.10   0.04   0.17  0.95 median hdci
#  9     9 beta_social_intercept               -1.86  -2.47  -1.29  0.95 median hdci
# 10    10 beta_alignment                       0.22   0.14   0.30  0.95 median hdci

# gamma-only summary (sampling-stage parameters; i = 11..16 in group_full.stan)
pop_draws_group_full |>
  filter(i %in% 11:16) |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(param = c(
    "gamma_intercept (sampling intercept)",
    "gamma_value",
    "gamma_estim",
    "gamma_discover",
    "gamma_switch",
    "gamma_social"
  )) |>
  relocate(param, .after = i) |>
  mutate(
    mu = round(mu, 2),
    .lower = round(.lower, 2),
    .upper = round(.upper, 2)
  )

# # A tibble: 6 × 8
#       i param                                   mu .lower .upper .width .point .interval
#   <int> <chr>                                <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1    11 gamma_intercept (sampling intercept)  0.02  -0.02   0.06  0.95 median hdci
# 2    12 gamma_value                           0.07  -0.12   0.25  0.95 median hdci
# 3    13 gamma_estim                           0.36   0.13   0.61  0.95 median hdci
# 4    14 gamma_discover                        1.20   0.86   1.52  0.95 median hdci
# 5    15 gamma_switch                          2.86   1.93   3.79  0.95 median hdci
# 6    16 gamma_social                         -0.12  -0.36   0.12  0.95 median hdci

# eta + theta summary (copy parameters; i = 18..21 in group_full.stan).
# All eta_* are on the logit scale (NO choice-stage confirmation term):
#   pi_copy = inv_logit(eta_intercept + eta_personal*|u_t|
#                       + eta_social*|s_t|).
pop_draws_group_full |>
  filter(i %in% c(18, 19, 20, 21)) |>
  group_by(i) |>
  median_hdci(mu, .width = 0.95) |>
  mutate(param = c(
    "eta_intercept (pi_copy intercept)",
    "eta_personal (pi_copy slope on |u_t|, own strength)",
    "eta_social (pi_copy slope on |s_t|, social strength)",
    "theta (social log-odds weight)"
  )) |>
  relocate(param, .after = i) |>
  mutate(
    mu = round(mu, 2),
    .lower = round(.lower, 2),
    .upper = round(.upper, 2)
  )

# # A tibble: 4 × 8
#       i param                           mu .lower .upper .width .point .interval
#   <int> <chr>                        <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1    18 eta_intercept (pi_copy inte…  0.09  -0.96   1.11   0.95 median hdci
# 2    19 eta_personal (pi_copy slope… -1.62  -2.35  -0.96   0.95 median hdci
# 3    20 eta_social (pi_copy slope o… -0.18  -0.47   0.08   0.95 median hdci
# 4    21 theta (social log-odds wei…   2.07   0.91   3.86   0.95 median hdci


# Posterior correlation between the population means of beta_step (i = 2) and
# beta_social_intercept (i = 9, the social-presence intercept) in
# group_full.stan. This is the correlation across posterior draws, i.e. how far
# the two population-level estimates trade off against each other, not an
# individual-difference correlation (sigma is diagonal in group_full.stan, so
# subject-level correlations are not modeled).
pop_draws_group_full |>
  filter(i %in% c(2, 9)) |>
  ungroup() |>
  select(.draw, i, mu) |>
  pivot_wider(names_from = i, values_from = mu, names_prefix = "mu_i") |>
  summarise(r = round(cor(mu_i2, mu_i9), 2))

# # A tibble: 1 × 1
#       r
#   <dbl>
# 1 -0.04


# Differences in parameter estimates -------------------------------------------

# beta_personal (own evidence strength slope) difference: group - solo.
# Both draws are in the P(stop) convention (modeled directly).
inner_join(
  pop_draws_solo |>
    filter(i == 7) |>
    ungroup() |>
    select(.draw, mu_solo = mu),
  pop_draws_group_full |>
    filter(i == 7) |>
    ungroup() |>
    select(.draw, mu_group = mu),
  by = ".draw"
) |>
  mutate(delta = mu_group - mu_solo) |>
  median_hdci(delta, .width = 0.95) |>
  mutate(across(c(delta, .lower, .upper), \(x) round(x, 2)))

# # A tibble: 1 × 6 (effect on P(stop))
#    delta .lower .upper .width .point .interval
#    <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1 -0.06  -0.19   0.07   0.95 median hdci

# Stopping-stage strength contrasts within the group condition:
#   beta_personal (own evidence, i=7), beta_social (social, i=8),
#   beta_alignment (i=10).
# Pivoted to long format so each contrast is one row (delta, .lower, .upper).
pop_draws_group_full |>
  filter(i %in% c(7, 8, 10)) |>
  ungroup() |>
  select(.draw, i, mu) |>
  pivot_wider(names_from = i, values_from = mu, names_prefix = "mu_i") |>
  # Betas are in the P(stop) convention (modeled directly in the Stan models).
  mutate(
    own_minus_social   = mu_i7 - mu_i8,
    alignment_minus_own    = mu_i10 - mu_i7,
    alignment_minus_social = mu_i10 - mu_i8
  ) |>
  pivot_longer(
    c(own_minus_social, alignment_minus_own, alignment_minus_social),
    names_to = "contrast", values_to = "delta"
  ) |>
  group_by(contrast) |>
  median_hdci(delta, .width = 0.95) |>
  mutate(across(c(delta, .lower, .upper), \(x) round(x, 2)))

# # A tibble: 3 × 7 (betas: effect on P(stop))
#   contrast               delta .lower .upper .width .point .interval
#   <chr>                  <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1 alignment_minus_own    -0.17  -0.31  -0.03   0.95 median hdci
# 2 alignment_minus_social  0.12   0.01   0.23   0.95 median hdci
# 3 own_minus_social        0.29   0.17   0.40   0.95 median hdci


# All shared parameters (present in both solo and group_full): difference
# group_full - solo. beta_* share the same row index in both models; gamma_*
# are offset by +3 in group_full (rows 8..10 are the group-only social betas:
# beta_social, beta_social_intercept, beta_alignment).
# gamma_switch (cue_switch) is already in the switching sign convention in the
# model, so no reporting flip is needed here. The stopping betas are in the
# P(stop) convention (modeled directly), so sign = 1 throughout.
shared_params <- tribble(
  ~param,                                  ~i_solo, ~i_group, ~sign,
  "beta_intercept (stopping intercept)",         1,        1,     1,
  "beta_step",                                   2,        2,     1,
  "beta_trial",                                  3,        3,     1,
  "beta_outcome",                                4,        4,     1,
  "beta_extremity",                              5,        5,     1,
  "beta_both_seen",                              6,        6,     1,
  "beta_personal",                               7,        7,     1,
  "gamma_intercept (sampling intercept)",        8,       11,     1,
  "gamma_value",                                 9,       12,     1,
  "gamma_estim",                                10,       13,     1,
  "gamma_discover",                             11,       14,     1,
  "gamma_switch",                               12,       15,     1
)

solo_long <- pop_draws_solo |>
  ungroup() |>
  select(.draw, i, mu) |>
  inner_join(shared_params, by = c("i" = "i_solo")) |>
  select(.draw, param, sign, mu_solo = mu)

group_long <- pop_draws_group_full |>
  ungroup() |>
  select(.draw, i, mu) |>
  inner_join(select(shared_params, param, i_group), by = c("i" = "i_group")) |>
  select(.draw, param, mu_group = mu)

inner_join(solo_long, group_long, by = c(".draw", "param")) |>
  mutate(delta = sign * (mu_group - mu_solo)) |>
  group_by(param) |>
  median_hdci(delta, .width = 0.95) |>
  mutate(across(c(delta, .lower, .upper), \(x) round(x, 2))) |>
  arrange(match(param, shared_params$param))

# # A tibble: 12 × 7 (stopping betas: effect on P(stop))
#    param                                delta .lower .upper .width .point .interval
#    <chr>                                <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
#  1 beta_intercept (stopping intercept)  2.89   1.35   4.35  0.95 median hdci
#  2 beta_step                            2.13   1.00   3.27  0.95 median hdci
#  3 beta_trial                          -0.30  -0.50  -0.08  0.95 median hdci
#  4 beta_outcome                        -0.01  -0.12   0.10  0.95 median hdci
#  5 beta_extremity                       0.10   0.01   0.19  0.95 median hdci
#  6 beta_both_seen                       0.13  -0.01   0.27  0.95 median hdci
#  7 beta_personal                       -0.06  -0.19   0.07  0.95 median hdci
#  8 gamma_intercept (sampling intercept) 0.02  -0.03   0.08  0.95 median hdci
#  9 gamma_value                         -0.14  -0.38   0.10  0.95 median hdci
# 10 gamma_estim                          0.05  -0.26   0.37  0.95 median hdci
# 11 gamma_discover                      -0.04  -0.51   0.40  0.95 median hdci
# 12 gamma_switch                        -0.81  -2.23   0.67  0.95 median hdci

# Model comparison: LOO-CV -----------------------------------------------------
load(here("output/fit/loo.rda"))
loo::loo_compare(
  loo_group_full,
  loo_group_constant,
  loo_group_value_shaping,
  loo_group_asocial_choice
)

#        elpd_diff se_diff
# model1    0.0       0.0    (group_full)
# model2  -26.5       7.9    (group_constant)
# model3 -171.4      21.7    (group_value_shaping)
# model4 -188.3      23.0    (group_asocial_choice)
