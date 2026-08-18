# Descriptive statistics

library(tidyverse)
library(here)
load(here("data/data.rda"))


# Participant demographics -----------------------------------------------------

# Age: overall
df_questionnaire |>
  summarise(
    n    = n(),
    mean = mean(age, na.rm = TRUE),
    sd   = sd(age, na.rm = TRUE),
    min  = min(age, na.rm = TRUE),
    max  = max(age, na.rm = TRUE)
  )

# Age: by condition
df_questionnaire |>
  group_by(condition) |>
  summarise(
    n    = n(),
    mean = mean(age, na.rm = TRUE),
    sd   = sd(age, na.rm = TRUE),
    min  = min(age, na.rm = TRUE),
    max  = max(age, na.rm = TRUE)
  )

# Gender: overall
df_questionnaire |>
  count(gender) |>
  mutate(pct = n / sum(n) * 100)

# Gender: by condition
df_questionnaire |>
  count(condition, gender) |>
  group_by(condition) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()


# Sample size by condition -----------------------------------------------------

df_trials |>
  group_by(condition, subject_id) |>
  summarise(mean_n = mean(n_samples_self)) |>
  group_by(condition) |>
  summarise(mean = mean(mean_n), sd = sd(mean_n))


# Consensus building along choice order ----------------------------------------
# Do adjacent deciders agree more in real interacting groups than in independent
# individuals? For each group x option, members are ordered by own sample count
# (fewer = earlier), and each adjacent pair (1-2, 2-3, 3-4, 4-5) scores 1 if the
# two chose the same option. Real groups are compared to a virtual-solo
# baseline: solo participants are randomly partitioned into pseudo-groups
# matching the real size composition (17 x 4, 16 x 5), ordered the same way,
# and pair-agreement is recomputed over many resamples. Because the baseline
# shares the solo marginal choice rates, any EXCESS agreement in real groups
# reflects social coordination, not option difficulty. The excess grows with
# choice order -- the signature of an accumulating cascade.

df_choice_pairs <- df_trials |>
  filter(!is.na(choice_self)) |>
  select(condition, group_id, subject_id, option_id, trial_type, rare_outcome,
    n_samples_self, choice_self)

# Adjacent-order agreement within each (group, option): order by sample count,
# compare each member's choice with the next member's; keep pairs 1-2 ... 4-5.
consec_agree <- function(df) {
  df |>
    group_by(group_id, option_id) |>
    arrange(n_samples_self, .by_group = TRUE) |>
    mutate(order_pair = paste0(row_number(), "-", row_number() + 1),
      agree      = as.integer(choice_self == lead(choice_self))) |>
    ungroup() |>
    filter(!is.na(agree), order_pair %in% c("1-2", "2-3", "3-4", "4-5"))
}

# Real groups: member-level adjacent pairs, then agreement at each order pair.
consensus_real_raw <- df_choice_pairs |>
  filter(condition == "group") |>
  consec_agree()

consensus_real <- consensus_real_raw |>
  group_by(order_pair) |>
  summarise(agree = mean(agree), n = n(), .groups = "drop")

# Real size composition (17 groups of 4, 16 of 5) for the virtual-solo baseline.
group_sizes <- df_trials |>
  filter(condition == "group") |>
  distinct(group_id, subject_id) |>
  count(group_id, name = "size") |>
  pull(size)

solo_choice <- df_choice_pairs |>
  filter(condition == "solo") |>
  select(-group_id)
solo_ids    <- distinct(solo_choice, subject_id)

# Virtual-solo baseline: partition solo participants into pseudo-groups
# matching the size composition, recompute pair-agreement, and summarise across
# resamples.
set.seed(1)
consensus_solo_raw <- map_dfr(seq_len(1000), function(b) {
  assign_tbl <- solo_ids |>
    slice_sample(n = sum(group_sizes)) |>
    mutate(group_id = rep(seq_along(group_sizes), group_sizes))
  solo_choice |>
    inner_join(assign_tbl, by = "subject_id") |>
    consec_agree() |>
    group_by(order_pair) |>
    summarise(agree = mean(agree), n = n(), .groups = "drop") |>
    mutate(resample = b)
})

# Across-resample band (mean and 95% interval) used by the figure below.
consensus_solo <- consensus_solo_raw |>
  group_by(order_pair) |>
  summarise(mean = mean(agree),
    lo   = quantile(agree, 0.025),
    hi   = quantile(agree, 0.975),
    .groups = "drop")

# Summary table: real vs virtual-solo and the excess at each order pair.
consensus_summary <- consensus_real |>
  left_join(consensus_solo, by = "order_pair") |>
  mutate(excess = agree - mean) |>
  select(order_pair, real = agree, n, virtual_solo = mean, lo, hi, excess)
consensus_summary

# Resample-based statistics (trial_type x rare_outcome collapsed), using the
# virtual-solo resamples as the null distribution. All p-values are two-sided:
# p = 2 * min(P(null >= observed), P(null <= observed)); the floor with 1000
# resamples is 2/1000 = 0.002 (reported as < 0.002 when no null value is as
# extreme). Statistics: per order pair; overall mean agreement across the four
# pairs; and the slope of agreement over choice order (a less-declining real
# slope means the group-vs-independent gap WIDENS with order -- the signature of
# consensus building).
pair_order <- c("1-2", "2-3", "3-4", "4-5")
p_two_sided <- function(null, obs) 2 * min(mean(null >= obs), mean(null <= obs))

# Per-pair two-sided resample p.
consensus_stats <- consensus_summary |>
  rowwise() |>
  mutate(p_resample = p_two_sided(
    consensus_solo_raw$agree[consensus_solo_raw$order_pair == order_pair],
    real
  )) |>
  ungroup()
consensus_stats

# Overall agreement, real vs solo-null, under two weightings.
# Unweighted: the plain mean of the four pair rates, so each order pair counts
# equally regardless of how many observations it rests on.
consensus_real_mean <- mean(consensus_real$agree)
solo_mean_null <- consensus_solo_raw |>
  group_by(resample) |>
  summarise(m = mean(agree), .groups = "drop") |>
  pull(m)

# Pooled: every adjacent pair counts once, so the 4-5 pair (n = 576, contributed
# only by the 16 five-member groups) does not weigh as much as the three pairs
# present in all 33 groups (n = 1188). This is the overall agreement reported in
# the paper.
consensus_real_pooled <- mean(consensus_real_raw$agree)
solo_pooled_null <- consensus_solo_raw |>
  group_by(resample) |>
  summarise(m = sum(agree * n) / sum(n), .groups = "drop") |>
  pull(m)

# Slope of agreement over choice order (1-2 .. 4-5): real vs solo-null.
pair_slope <- function(order_pair, agree) {
  unname(coef(lm(agree ~ match(order_pair, pair_order)))[2])
}
consensus_real_slope <- pair_slope(
  consensus_real$order_pair, consensus_real$agree
)
solo_slope_null <- consensus_solo_raw |>
  group_by(resample) |>
  summarise(s = pair_slope(order_pair, agree), .groups = "drop") |>
  pull(s)

# Collected scalar statistics (two-sided resample p-values against the null).
# The *_pooled columns are the observation-weighted overall agreement quoted in
# the paper; the unweighted columns keep the equal-weight-per-pair version.
consensus_scalar_stats <- tibble(
  real_mean          = consensus_real_mean,
  solo_mean          = mean(solo_mean_null),
  excess_mean        = consensus_real_mean - mean(solo_mean_null),
  p_overall          = p_two_sided(solo_mean_null, consensus_real_mean),
  real_mean_pooled   = consensus_real_pooled,
  solo_mean_pooled   = mean(solo_pooled_null),
  excess_mean_pooled = consensus_real_pooled - mean(solo_pooled_null),
  p_overall_pooled   = p_two_sided(solo_pooled_null, consensus_real_pooled),
  real_slope         = consensus_real_slope,
  solo_slope         = mean(solo_slope_null),
  p_slope            = p_two_sided(solo_slope_null, consensus_real_slope)
)
consensus_scalar_stats

# Cache both tables so downstream text and tables can quote the agreement
# numbers (per-pair excess, overall mean under either weighting, and the slope
# over choice order) without re-running the 1000 virtual-solo resamples.
save(
  consensus_stats,
  consensus_scalar_stats,
  file = here("output/sim/consensus_agreement.rda")
)
