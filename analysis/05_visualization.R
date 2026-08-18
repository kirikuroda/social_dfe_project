# Publication-quality figures and tables

library(patchwork)
library(tidyverse)
library(here)
library(ggforce)
library(ggdist)
library(tidybayes)
library(colorspace)
load(here("data/data.rda"))
source(here("function/R/plot_settings.R"))
source(here("function/R/utils.R"))  # add_option_no()

# Task and solo choices --------------------------------------------------------

# Coin helpers
# Build a data frame for 10 coins (2 rows × 5 cols).
# 9 coins show val_common; the 10th (last) shows val_rare.
build_coins <- function(val_common, val_rare,
                        fill_common = col_bg,  border_common = col_dark,
                        fill_rare   = col_bg,  border_rare   = "#cccccc",
                        text_common = col_dark, text_rare  = col_dark) {
  n <- 10L
  tibble(
    idx    = seq_len(n),
    val    = c(rep(as.character(val_common), n - 1L), as.character(val_rare)),
    fill   = c(rep(fill_common,   n - 1L), fill_rare),
    border = c(rep(border_common, n - 1L), border_rare),
    tcol   = c(rep(text_common,   n - 1L), text_rare),
    col_i  = (idx - 1L) %% 5L,
    row_i  = (idx - 1L) %/% 5L
  )
}

# Draw one coin-grid plot; row_label placed left or right of the grid.
# Font sizes: coin number = 8pt, row label = 10pt (converted to ggplot mm units)
make_coin_plot <- function(coins, row_label = NULL, label_side = "left",
                           lbl_x_offset = 0) {
  r    <- 0.42 * 0.6        # coin radius
  # centre-to-centre distance (gap between coins = 0.1 * r)
  gap  <- 2.1 * r
  # rescale grid coordinates using centre-to-centre distance
  coins <- coins |> dplyr::mutate(x = col_i * gap, y = -row_i * gap)
  x_max <- 4 * gap  # rightmost coin centre

  # xlim/ylim flush to coin edges
  xlim_l <- -r
  xlim_r <-  x_max + r
  ylim_b <- -(gap + r)
  ylim_t <-   r

  ggplot(coins) +
    ggforce::geom_circle(
      aes(x0 = x, y0 = y, r = r, fill = fill, color = border),
      linewidth = 0.2, show.legend = FALSE
    ) +
    geom_text(aes(x = x, y = y, label = val, color = tcol),
      size = 6 / .pt, show.legend = FALSE) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_fixed(xlim = c(xlim_l, xlim_r), ylim = c(ylim_b, ylim_t)) +
    theme_void() +
    theme(
      plot.margin      = margin(0, 0, 0, 0),
      plot.background  = element_rect(fill = "transparent", color = NA)
    )
}


# Coin data
# Coin colour rules (user-specified):
#   rare coin with higher payoff  → black fill, white text
#   rare coin with lower payoff   → grey fill, white text
#   common coins                  → white fill, dark text

# --- Risky/Safe, small rare outcome (p_a_high = 0.9) ---
# Safe A: all 11 (deterministic, no rare coin)
# Risky B: common = 10 (×9), rare = 20 → rare is the HIGH payoff → teal
coins_rs_l_safe  <- build_coins(11, 11,
  fill_rare = col_bg, border_rare = col_dark,
  text_rare = col_dark)
coins_rs_l_risky <- build_coins(10, 20,
  fill_rare = "black", border_rare = "black",
  text_rare = "white")

# --- Risky/Safe, large rare outcome (p_a_high = 0.1) ---
# Safe A: all 19
# Risky B: common = 20 (×9), rare = 10 → rare is the LOW payoff → grey
coins_rs_s_safe  <- build_coins(19, 19,
  fill_rare = col_bg, border_rare = col_dark,
  text_rare = col_dark)
coins_rs_s_risky <- build_coins(20, 10,
  fill_rare = col_grey, border_rare = col_grey,
  text_rare = "white")

# --- Risky/Risky, small rare outcome (p_a_high = 0.9) ---
# Option A (Risky):  common = 10 (×9), rare = 20 → rare HIGH → teal
# Option B (Jackpot): common = 5 (×9), rare = 145 → rare HIGH → teal
coins_rr_l_risky <- build_coins(10, 20,
  fill_rare = col_grey, border_rare = col_grey,
  text_rare = "white")
coins_rr_l_jack  <- build_coins(5, 65,
  fill_rare = "black", border_rare = "black",
  text_rare = "white")

# --- Risky/Risky, large rare outcome (p_a_high = 0.1) ---
# Option A (Risky):  common = 20 (×9), rare = 10 → rare LOW → grey
# Option B (Jackpot): common = 5 (×9), rare = 65 → rare HIGH → teal
coins_rr_s_risky <- build_coins(20, 10,
  fill_rare = col_grey, border_rare = col_grey,
  text_rare = "white")
coins_rr_s_jack  <- build_coins(5, 145,
  fill_rare = "black", border_rare = "black",
  text_rare = "white")


# Coin plots
# lbl_x_offset for "Safe": shift left by ~1 character width to align
# left edge with "Risky" (hjust=1, so offset shifts the anchor leftward)
safe_offset  <- -0.2
p_rs_s_safe  <- make_coin_plot(coins_rs_s_safe, "Safe", "left",
  lbl_x_offset = safe_offset)
p_rs_s_risky <- make_coin_plot(coins_rs_s_risky, "Risky",   "left")
p_rs_l_safe  <- make_coin_plot(coins_rs_l_safe, "Safe", "left",
  lbl_x_offset = safe_offset)
p_rs_l_risky <- make_coin_plot(coins_rs_l_risky, "Risky",   "left")

p_rr_s_jack  <- make_coin_plot(coins_rr_s_jack,  "Jackpot", "left")
p_rr_s_risky <- make_coin_plot(coins_rr_s_risky, "Risky",   "left")
p_rr_l_jack  <- make_coin_plot(coins_rr_l_jack,  "Jackpot", "left")
p_rr_l_risky <- make_coin_plot(coins_rr_l_risky, "Risky",   "left")


# Main plots
sim_benchmark <- readRDS(here("output/sim/sim_benchmark.rds"))
# Split into 4 plots: trial type (risky_safe / risky_risky) x rare outcome
# (large / small). One panel each, aligned 1:1 with the coin plots.
make_fig <- function(tt, ro, ylab) {
  df_trials |>
    filter(condition == "solo", trial_type == tt, rare_outcome == ro) |>
    ggplot(aes(n_samples_self, choice_self)) +
    geom_line(
      data = sim_benchmark |>
        filter(step >= 2, trial_type == tt, rare_outcome == ro) |>
        # sim_benchmark is now per-option; collapse back to a single
        # cell-average line for this 4-cell summary figure.
        group_by(step) |>
        summarise(p_choice_B = mean(p_choice_B), .groups = "drop"),
      aes(step, p_choice_B),
      color = col_dark
    ) +
    stat_summary(color = col_teal, size = 0.05, linewidth = 0.2) +
    labs(x = "Sample size", y = ylab) +
    theme_fig
}

fig_rs_l <- make_fig("risky_safe",  "large", "P(choose safe)")
fig_rs_s <- make_fig("risky_safe",  "small", "P(choose safe)")
fig_rr_l <- make_fig("risky_risky", "large", "P(choose jackpot)")
fig_rr_s <- make_fig("risky_risky", "small", "P(choose jackpot)")


# Save each part as PDF ---------------------------------------------------

width_main <- 4   # cm
height_main <- 5   # cm
height_main_single <- 3   # cm (single panel)
width_coin  <- 2   # cm
height_coin <- 1 # cm

save_pdf(fig_rs_l, "fig_solo_rs_l.pdf",
  width = width_main, height = height_main_single
)
save_pdf(fig_rs_s, "fig_solo_rs_s.pdf",
  width = width_main, height = height_main_single
)
save_pdf(fig_rr_l, "fig_solo_rr_l.pdf",
  width = width_main, height = height_main_single
)
save_pdf(fig_rr_s, "fig_solo_rr_s.pdf",
  width = width_main, height = height_main_single
)

save_pdf(p_rs_s_safe, "coin_rs_s_safe.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rs_s_risky, "coin_rs_s_risky.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rs_l_safe, "coin_rs_l_safe.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rs_l_risky, "coin_rs_l_risky.pdf",
  width = width_coin, height = height_coin
)

save_pdf(p_rr_s_jack, "coin_rr_s_jackpot.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rr_s_risky, "coin_rr_s_risky.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rr_l_jack, "coin_rr_l_jackpot.pdf",
  width = width_coin, height = height_coin
)
save_pdf(p_rr_l_risky, "coin_rr_l_risky.pdf",
  width = width_coin, height = height_coin
)


# Benchmark predictions for all 36 trials --------------------------------------
# Stepwise P(choose B) from the benchmark model (no stopping rule, alternating
# sampling, temperature = 1) for every individual trial. Panels are grouped by
# condition (trial_type x rare_outcome); within each panel one line per trial
# (option_id).

# Per-trial panel label: "Option N", where N is the sequential option number
# used in the choice-problem table (output/table/options.tex), so that panel
# numbers and table rows refer to the same problem. With ncol = 9 each row of
# panels is exactly one trial_type x rare_outcome block.
panel_order <- df_options |>
  add_option_no() |>
  filter(trial_type != "attention_check") |>
  distinct(option_id, option_no, trial_type, rare_outcome) |>
  arrange(option_no) |>
  mutate(panel_label = sprintf("Option %d", option_no)) |>
  mutate(panel_label = factor(panel_label, levels = panel_label))

add_panel <- function(df) {
  df |> left_join(select(panel_order, option_id, panel_label), by = "option_id")
}

# Empirical choice probability per trial (solo): one point per
# option_id x sample size at the observed P(choose B).
df_emp_bm36 <- df_trials |>
  filter(condition == "solo") |>
  group_by(option_id, trial_type, rare_outcome, n_samples_self) |>
  summarise(
    mean_choice = mean(choice_self),
    n           = n(),
    .groups     = "drop"
  ) |>
  add_panel()

fig_benchmark_all36 <- sim_benchmark |>
  filter(step >= 2) |>
  add_panel() |>
  ggplot(aes(step, p_choice_B)) +
  geom_line(color = col_dark, alpha = 0.8, linewidth = 0.3) +
  geom_point(
    data = df_emp_bm36,
    aes(n_samples_self, mean_choice, size = n),
    color = col_teal, alpha = 0.8, stroke = 0,
    inherit.aes = FALSE
  ) +
  scale_size_area(max_size = 2.5, name = "Observations") +
  scale_x_continuous(breaks = c(10, 20, 30, 40)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~panel_label, ncol = 9) +
  labs(x = "Sample size", y = "P(choose option B)") +
  theme_minimal(base_size = 9) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    text             = element_text(color = col_dark),
    strip.text       = element_text(size = 7)
  )

save_pdf(fig_benchmark_all36, "simple-choice-all36.pdf",
  width = 17.8, height = 8
)


# Solo: effect of sample size on choice (logistic slope, log-odds / sample).
# 95% interval as pointrange. Horizontal: x = coefficient, y = panel (C-F).
slope_choice_solo <- readRDS(here("output/fit/slope_choice_solo.rds"))

# Panel order (top -> bottom): rs/large = C, rs/small = D, rr/large = E,
# rr/small = F
panel_levels <- c("D", "E", "F", "G")

fig_slope_solo <- slope_choice_solo |>
  mutate(
    panel = case_when(
      trial_type == "risky_safe"  & rare_outcome == "large" ~ "D",
      trial_type == "risky_safe"  & rare_outcome == "small" ~ "E",
      trial_type == "risky_risky" & rare_outcome == "large" ~ "F",
      trial_type == "risky_risky" & rare_outcome == "small" ~ "G"
    ),
    # rev() so C sits at the top, F at the bottom
    # panel = factor(panel, levels = rev(panel_levels))
  ) |>
  ggplot(aes(x = estimate, y = "", xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = col_grey) +
  geom_pointrange(color = col_teal) +
  labs(x = "Effect of sample size on choice", y = NULL) +
  theme_gray(base_size = 8) +
  facet_wrap(
    ~panel, nrow = 2,
    labeller = labeller(panel = \(x) paste0("Panel ", x))
  ) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    text             = element_text(color = col_dark),
    panel.background    = element_rect(fill = "transparent", color = NA),
  )

save_pdf(fig_slope_solo, "fig_slope_solo.pdf", width = 4.2, height = 6.2)


# Posterior parameter estimates: solo vs group ---------------------------------
# Median + 95% HDCI of each population-level mu, drawn as pointranges in three
# stacked panels (sampling / stopping / consequential choice). Solo and group
# are colour-coded (col_condition); group-only parameters (social betas,
# gamma_social, eta_*, theta) appear in the group colour only.
#
# Plotting choices:
#   Sampling — every cue except the intercept.
#   Stopping — own evidence (|u|), social evidence (|s|), social presence
#              (social-condition intercept), and confirmation (align).
#   Consequential choice — split into three free-scale facets (tau / theta /
#              eta) because the parameters live on different scales: tau is the
#              choice inverse-temperature (solo + group), theta the social
#              log-odds weight (group), and the eta_* the pi_copy logit
#              coefficients (group).
#
# mu row indices (authoritative, from the current Stan models):
#   solo.stan (13): beta_intercept..beta_strength = 1..7, gamma_intercept..
#     gamma_switch = 8..12, tau_mean = 13.
#   group_full.stan (21): beta_intercept..beta_confirm = 1..10,
#     gamma_intercept..gamma_social = 11..16, tau_mean = 17,
#     eta_intercept..eta_social_strength = 18..20, theta = 21
#     (no choice-stage eta_confirm).

pop_draws_solo       <- readRDS(here("output/fit/pop_draws_solo.rds"))
pop_draws_group_full <- readRDS(here("output/fit/pop_draws_group_full.rds"))

param_map <- tribble(
  ~stage,         ~facet,             ~param,                  ~condition, ~i, ~sign,
  # --- Sampling (gamma); intercept omitted ---
  "Sampling",     "Sampling",         "gamma_value",           "solo",      9,  1,
  "Sampling",     "Sampling",         "gamma_estim",           "solo",     10,  1,
  "Sampling",     "Sampling",         "gamma_discover",        "solo",     11,  1,
  "Sampling",     "Sampling",         "gamma_switch",          "solo",     12,  1,
  "Sampling",     "Sampling",         "gamma_value",           "group",    12,  1,
  "Sampling",     "Sampling",         "gamma_estim",           "group",    13,  1,
  "Sampling",     "Sampling",         "gamma_discover",        "group",    14,  1,
  "Sampling",     "Sampling",         "gamma_switch",          "group",    15,  1,
  "Sampling",     "Sampling",         "gamma_social",          "group",    16,  1,
  # --- Stopping (beta): own / social evidence, social presence, confirmation
  # --- Stopping betas are modeled directly as P(stop) in the Stan models
  # (outcome 1 = stop), so pop_draws_* are already in the P(stop) convention
  # and no sign flip is applied here.
  "Stopping",     "Sample size",      "beta_step",             "solo",      2,  1,
  "Stopping",     "Sample size",      "beta_step",             "group",     2,  1,
  "Stopping",     "Stopping",         "beta_strength",         "solo",      7,  1,
  "Stopping",     "Stopping",         "beta_strength",         "group",     7,  1,
  "Stopping",     "Stopping",         "beta_social_strength",  "group",     8,  1,
  "Stopping",     "Stopping",         "beta_social_intercept", "group",     9,  1,
  "Stopping",     "Stopping",         "beta_confirm",          "group",    10,  1,
  # --- Consequential choice: tau / theta / eta in separate free-scale facets
  # ---
  "Consequential choice", "Temperature",         "tau",                 "solo",  13, 1,
  "Consequential choice", "Temperature",         "tau",                 "group", 17, 1,
  "Consequential choice", "Conformity exponent", "theta",               "group", 21, 1,
  "Consequential choice", "Effects on social weight",       "eta_intercept",       "group", 18, 1,
  "Consequential choice", "Effects on social weight",       "eta_strength",        "group", 19, 1,
  "Consequential choice", "Effects on social weight",       "eta_social_strength", "group", 20, 1
)

# Top-to-bottom order within each panel, and human-readable labels.
param_levels <- c(
  # Sampling
  "gamma_value", "gamma_estim", "gamma_discover", "gamma_switch",
  "gamma_social",
  # Stopping (beta_step first so it sits at the top of the panel)
  "beta_step", "beta_strength", "beta_social_strength", "beta_confirm",
  "beta_social_intercept",
  # Consequential choice
  "tau", "theta",
  "eta_intercept", "eta_strength", "eta_social_strength"
)
param_labels <- c(
  gamma_value = "Value", gamma_estim = "Estimation",
  gamma_discover = "Discovery", gamma_switch = "Switch",
  gamma_social = "Social",
  beta_step = "Samples drawn\nso far",
  beta_strength = "Personal info\nstrength",
  beta_social_strength = "Social info\nstrength",
  beta_social_intercept = "Social\npresence",
  beta_confirm = "Personal-social\nalignment",
  # tau / theta sit alone in their facets and are named on the y-axis (their
  # strips are blanked); only the multi-row eta facet keeps a strip label.
  tau = "Inverse\ntemperature", theta = "Conformity\nexponent",
  eta_strength = "Personal info\nstrength",
  eta_social_strength = "Social info\nstrength",
  eta_intercept = "Intercept"
)
stage_levels <- c("Sampling", "Stopping", "Consequential choice")
# Facet order within a panel (top -> bottom). In Stopping, "Sample size"
# (beta_step) sits above the other betas; consequential choice: tau, theta, eta.
facet_levels <- c("Sampling", "Sample size", "Stopping",
  "Temperature", "Conformity exponent", "Effects on social weight")

param_summary <- bind_rows(
  pop_draws_solo       |> ungroup() |> mutate(condition = "solo"),
  pop_draws_group_full |> ungroup() |> mutate(condition = "group")
) |>
  select(condition, .draw, i, mu) |>
  inner_join(param_map, by = c("condition", "i")) |>
  mutate(mu = sign * mu) |>
  group_by(stage, facet, param, condition) |>
  median_hdci(mu, .width = 0.95) |>
  ungroup() |>
  mutate(
    param     = factor(param, levels = rev(param_levels)),
    stage     = factor(stage, levels = stage_levels),
    facet     = factor(facet, levels = facet_levels),
    # Levels reversed (group, solo) so the dodge places solo on top and group
    # below in every facet; the legend order is restored to Solo -> Group via
    # guide_legend(reverse = TRUE) in plot_param_stage().
    condition = factor(condition, levels = c("group", "solo"))
  )

# One pointrange panel per model stage. The consequential choice is split into
# free-scale facets (tau / theta / eta) via facetted = TRUE. legend = "inside"
# draws the solo/group key in the panel's top-right (used for Sampling only).
plot_param_stage <- function(df, stage_name, x_lab = "Estimates",
                             facetted = FALSE, legend = "none",
                             x_breaks = waiver(),
                             y_labels = param_labels,
                             strip_parse = FALSE,
                             strip_labels = c(
                               "Temperature"         = "",
                               "Conformity exponent" = "",
                               "Effects on social weight" = "Effects on social weight"
                             )) {
  p <- df |>
    filter(stage == stage_name) |>
    ggplot(aes(x = mu, y = param, xmin = .lower, xmax = .upper,
      color = condition)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = col_grey) +
    geom_pointrange(
      position = position_dodge(width = 0.5), size = 0.3, linewidth = 0.4
    ) +
    scale_color_manual(values = col_condition, drop = FALSE, name = NULL,
      labels = c(solo = "Solo", group = "Group")) +
    # condition levels are (group, solo) to put solo on top in the dodge;
    # reverse the legend so it still reads Solo -> Group.
    guides(color = guide_legend(reverse = TRUE)) +
    scale_y_discrete(labels = y_labels) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = x_lab, y = NULL, title = stage_name) +
    theme_minimal(base_size = 8) +
    theme(
      axis.line        = element_line(color = col_dark),
      axis.text        = element_text(color = col_dark),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(size = 8, face = "bold"),
      text             = element_text(color = col_dark),
      legend.position  = "none",
      plot.margin      = margin(0, 8, 0, 8)
    )
  if (identical(legend, "inside")) {
    p <- p +
      theme(
        legend.position        = "inside",
        legend.position.inside = c(1, 1),
        legend.justification   = c(1, 1),
        legend.background      = element_rect(
          fill = scales::alpha("white", 0.8), color = col_dark, linewidth = 0.3
        ),
        legend.key.size = unit(0.3, "cm"),
        legend.text     = element_text(size = 7),
        legend.spacing.y = unit(0.05, "cm"),
        plot.margin = margin(0, 8, 0, 0)
      )
  }
  if (facetted) {
    # facet_wrap (not facet_grid) so each facet gets its own free x scale; a
    # single-column facet_grid would share x across rows. Temperature and
    # Conformity exponent are named on the y-axis, so their strips are blanked;
    # only "Effects on social weight" keeps a horizontal strip label placed on
    # TOP of its facet.
    p <- p +
      facet_wrap(
        ~facet, ncol = 1, scales = "free", space = "free_y",
        strip.position = "top",
        labeller = if (strip_parse) {
          as_labeller(strip_labels, label_parsed)
        } else {
          labeller(facet = strip_labels)
        }
      ) +
      theme(
        strip.background = element_blank(),
        strip.text.x.top = element_text(size = 7),
        # panel.border = element_rect(color = col_dark, fill = NA, linewidth =
        # 0.3),
        plot.margin = margin(0, 8, 0, 8)
      )
  }
  p
}

# Panel D: model-implied copying weight as a function of personal evidence ----
# The copying weight pi is shown as a function of the own-evidence strength
# |u_t|, with the social-evidence strength held NEUTRAL (|s_t| = 0) so the curve
# isolates the effect of own (personal) evidence,
#   pi(u) = inv_logit(eta_intercept + eta_strength * u / beta_scale[6]),
# evaluated per posterior draw and summarised as median + 66%/95% credible
# bands. A marginal histogram of the observed |u_t| is drawn on top. Social
# evidence is reconstructed from PREDECESSORS only (members with strictly fewer
# samples), matching the strict reconstruction used by fit_group_full.
source(here("function/R/utils.R"))  # compute_delta_t_at_stop()

gs_marg <- readRDS(here("output/fit/group_scales.rds"))
s6_marg  <- gs_marg$beta_scale[[6]]    # SD of |u_t|
ss1_marg <- gs_marg$social_scale[[1]]  # SD of |s_t|
k_marg   <- gs_marg$k                  # tanh scaling for delta_t

# delta_t at stopping per (subject_id, option_id), group condition.
picopy_delta <- df_sampling |>
  filter(condition == "group") |>
  left_join(df_options, by = "option_id") |>
  mutate(is_higher_outcome = case_when(
    sampled_option_label == "a" & sampled_outcome == v_a_high ~ 1,
    sampled_option_label == "a" & sampled_outcome == v_a_low  ~ 0,
    sampled_option_label == "b" & sampled_outcome == v_b_high ~ 1,
    sampled_option_label == "b" & sampled_outcome == v_b_low  ~ 0
  )) |>
  group_by(subject_id, option_id) |>
  group_modify(~ tibble(delta_t = compute_delta_t_at_stop(.x))) |>
  ungroup()

# Observed own/social evidence strengths (predecessor-based social counts).
picopy_dist <- df_trials |>
  filter(
    condition == "group", trial_type %in% c("risky_safe", "risky_risky")
  ) |>
  group_by(group_id, option_id) |>
  group_modify(function(cell, key) {
    ns <- cell$n_samples_self
    ch <- cell$choice_self
    cell$social_A <- vapply(
      seq_along(ns), \(i) sum(ns < ns[i] & ch == 0), numeric(1)
    )
    cell$social_B <- vapply(
      seq_along(ns), \(i) sum(ns < ns[i] & ch == 1), numeric(1)
    )
    cell
  }) |>
  ungroup() |>
  left_join(picopy_delta, by = c("subject_id", "option_id")) |>
  filter(is.finite(delta_t), (social_A + social_B) > 0) |>
  mutate(
    u_t = abs(tanh(delta_t / k_marg)),
    s_t = abs(tanh(log((social_B + 0.1) / (social_A + 0.1))))
  )

# Posterior draws of the three pi-copy population parameters.
eta_draws_marg <- pop_draws_group_full |>
  ungroup() |>
  # 18 = intercept, 19 = |u_t| slope, 20 = |s_t| slope
  filter(i %in% c(18, 19, 20)) |>
  select(.draw, i, mu) |>
  pivot_wider(names_from = i, values_from = mu, names_prefix = "eta")

picopy_curve_sum <- eta_draws_marg |>
  expand_grid(u = seq(0, 1, length.out = 101)) |>
  # social evidence held neutral (|s_t| = 0): social term drops out
  mutate(pi_copy = plogis(eta18 + eta19 * (u / s6_marg))) |>
  group_by(u) |>
  median_qi(pi_copy, .width = c(0.66, 0.95)) |>
  ungroup()

# Main curve (median + nested credible bands).
picopy_main <- ggplot() +
  geom_ribbon(data = filter(picopy_curve_sum, .width == 0.95),
    aes(u, ymin = .lower, ymax = .upper), fill = col_teal, alpha = 0.15) +
  geom_ribbon(data = filter(picopy_curve_sum, .width == 0.66),
    aes(u, ymin = .lower, ymax = .upper), fill = col_teal, alpha = 0.30) +
  geom_line(data = filter(picopy_curve_sum, .width == 0.95),
    aes(u, pi_copy), color = col_teal, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Personal strength", y = "Social weight") +
  theme_minimal(base_size = 8) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    text             = element_text(color = col_dark),
    legend.position  = "none",
    plot.margin      = margin(0, 20, 0, 8)
  )

# Same curve, faceted by a fixed social-evidence strength |s_t|. The social term
# eta_social * |s_t| / social_scale[1] is added to the linear predictor, so each
# facet shows how the personal-strength response shifts as social evidence
# becomes stronger (|s_t| = 0 reproduces the marginal curve above).
s_levels <- c(0, 0.5, 1)

picopy_curve_by_s <- eta_draws_marg |>
  expand_grid(u = seq(0, 1, length.out = 101), s = s_levels) |>
  mutate(
    pi_copy = plogis(eta18 + eta19 * (u / s6_marg) + eta20 * (s / ss1_marg))
  ) |>
  group_by(s, u) |>
  median_hdci(pi_copy, .width = c(0.5, 0.95)) |>
  ungroup() |>
  mutate(s_lab = factor(sprintf("|s| = %.2f", s)))

# Panel D: pi_copy vs personal information, faceted vertically by social
# information |s_t| (YlOrRd; 50%/95% bands). One facet per |s_t| level; |s_t| is
# labelled as "Social information".
social_strength_labs <- setNames(
  sprintf("Social info strength = %.1f", s_levels),
  as.character(s_levels))
fig_picopy_panel_d <- ggplot(picopy_curve_by_s, aes(u, pi_copy)) +
  geom_ribbon(data = filter(picopy_curve_by_s, .width == 0.95),
    aes(ymin = .lower, ymax = .upper, fill = factor(s)), alpha = 0.3) +
  geom_ribbon(data = filter(picopy_curve_by_s, .width == 0.5),
    aes(ymin = .lower, ymax = .upper, fill = factor(s)), alpha = 0.50) +
  geom_line(data = filter(picopy_curve_by_s, .width == 0.95),
    aes(color = factor(s)), linewidth = 0.5) +
  scale_color_discrete_sequential(
    palette = "OrYel", rev = TRUE, nmax = 5, order = 3:5) +
  scale_fill_discrete_sequential(
    palette = "OrYel", rev = TRUE, nmax = 5, order = 3:5) +
  facet_wrap(~s, ncol = 1, labeller = labeller(s = social_strength_labs)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Personal info strength",
    y = expression(paste("Social weight (", pi, ")")),
    title = "Social weight") +
  theme_minimal(base_size = 8) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 8, face = "bold"),
    text             = element_text(color = col_dark),
    strip.text       = element_text(size = 7),
    legend.position  = "none"
  )

# Panel C (Consequential choice): a single facetted plot (so it respects its
# column width). Only the bottom facet carries the x-axis title ("Estimates").
# The tau / theta y-labels stay plain two-line text ("Inverse\ntemperature",
# "Conformity\nexponent") from param_labels -- their Greek symbols are omitted
# because plotmath cannot combine a Greek glyph with a clean line break. The eta
# facet strip is a single line, so it keeps its eta / pi symbols via plotmath.
cc_strip <- c(
  "Temperature"              = '""',
  "Conformity exponent"      = '""',
  "Effects on social weight" = '"Effects (" * eta * ") on social weight"'
)

fig_params <- (
  (plot_param_stage(param_summary, "Sampling", legend = "inside",
    x_lab = expression(paste("Estimates (", gamma, ")"))) +
    labs(tag = "A")) |
    (plot_param_stage(param_summary, "Stopping", facetted = TRUE,
      x_lab = expression(paste("Estimates (", beta, ")")),
      # per-facet step from each facet's own range: wide "Sample
      # size" facet -> 2, the narrow lower facet -> 1.
      x_breaks = \(limits) {
        if (max(limits) > 3) {
          scales::breaks_width(2)(limits)
        } else {
          scales::breaks_width(1)(limits)
        }
      },
      strip_labels = c("Sample size" = "", "Stopping" = "")) +
      labs(tag = "B")) |
    (plot_param_stage(param_summary, "Consequential choice", facetted = TRUE,
      x_lab = "Estimates",
      strip_parse = TRUE, strip_labels = cc_strip) +
      labs(tag = "C")) |
    (fig_picopy_panel_d + labs(tag = "D"))
) +
  plot_layout(widths = c(1, 1, 1.2, 1.2)) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) &
  theme(
    # Nudge the A-D tags slightly down from the very top (y < 1).
    plot.tag.position = c(0, 1.01),
    plot.title.position = "panel",
    plot.tag          = element_text(hjust = 0, vjust = 1)
  )

save_pdf(fig_params, "param-estimates.pdf", width = 17.8, height = 7)

# LOO model comparison --------------------------------------------------------
# PSIS-LOO elpd differences for the four group models. The best-fitting Full
# model is the reference (dashed line at 0, not drawn as a point); each
# competing model is shown as its elpd difference from Full +/- 1 SE.
loo_env <- new.env()
load(here("output/fit/loo.rda"), envir = loo_env)
loo_list <- list(
  "Full"                      = loo_env$loo_group_full,
  "Decision-biasing\n(constant social weight)" = loo_env$loo_group_constant,
  "Value shaping"             = loo_env$loo_group_value_shaping,
  "Asocial choice"            = loo_env$loo_group_asocial_choice
)
loo_df <- as.data.frame(loo::loo_compare(loo_list)) |>
  rownames_to_column("model") |>
  filter(model != "Full") |>                 # Full is the 0 reference line
  transmute(model, elpd_diff, se_diff,
    lo = elpd_diff - se_diff, hi = elpd_diff + se_diff) |>
  # best (elpd closest to 0) on top: order levels by ascending elpd_diff.
  mutate(model = fct_reorder(model, elpd_diff))

fig_loo <- ggplot(loo_df, aes(elpd_diff, model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = col_grey) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
    width = 0, color = "#e02b35", linewidth = 0.4) +
  geom_point(size = 1.8, color = "#e02b35") +
  labs(
    x = expression(Delta * "Expected log predictive density"),
    y = "Alternative models"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    text             = element_text(color = col_dark),
    legend.position  = "none"
  )
save_pdf(fig_loo, "loo-comparison.pdf", width = 8.5, height = 5)

message("Saved to: ", out_dir)

# Cascade analysis plot ---------------------------------------

df_cascade <- readRDS(here("output/sim/df_cascade.rds"))

col_cascade <- c(
  virtual_solo  = "#999999",
  real_group    = col_yellow  # same as the group colour in the coefficient plot
)

# condition aesthetics shared across plots: solo = triangle, group = circle.
cascade_shape  <- c(virtual_solo = 17, real_group = 16)
cascade_labels <- c(virtual_solo = "Solo", real_group = "Group")

# Solo benchmark: a single 33-group realization (03_simulation.R), computed
# identically to the real groups so solo and group share the same +/- 1 SE.
df_cascade_solo33 <- readRDS(here("output/sim/df_cascade_solo33.rds"))

# Real 33 groups + the single 33-group solo set, stacked and plotted with the
# SAME stat_summary(mean +/- SE) so the two conditions are strictly comparable.
cascade_df <- bind_rows(
  df_cascade |>
    filter(condition == "real_group") |>
    select(condition, choice_order, trial_type, rare_outcome, choice_self),
  df_cascade_solo33 |>
    select(condition, choice_order, trial_type, rare_outcome, choice_self)
)


# Facet labels shared by the choice-order figures below.
trial_type_lab <- c(
  risky_safe  = "Risky/Safe",
  risky_risky = "Risky/Risky"
)
rare_outcome_lab <- c(
  large = "Rare = large",
  small = "Rare = small"
)


# Asocial benchmark for the sweep figure ---------------------------------------
# Posterior-draw solo simulation (one draw id per virtual group), drawn behind
# the sweep curves in fig_sweep_combined as the no-cascade reference.

solo_benchmark <- readRDS(here("output/sim/sim_post_solo.rds"))


# Combined sweep figure: theta (Conformity exponent) and eta_intercept
# (Intercept) -----------------------------------------------------------------
# Eight separate panels joined with patchwork in a 2 x 4 grid: columns 1-2 =
# theta (PuBu), columns 3-4 = eta_intercept (Reds); within each parameter the
# two columns are rare_outcome = large / small. Top row = Risky/Safe trials (Y
# = P(choose safe)), bottom row = Risky/Risky trials (Y = P(choose jackpot)).
# Each cell overlays the grey asocial benchmark line. Tags run by parameter
# block (set explicitly per cell, NOT auto-sequential, so the letter is
# decoupled from grid position): theta = A B (top) / C D (bottom),
# eta_intercept = E F (top) / G H (bottom). Facet strips and plot titles are
# removed; choice order is shown as 1..5 on the bottom row, with a "Choice
# order" x-axis title on each bottom-row panel (C, D, G, H); x/y axis lines are
# drawn. Each parameter block carries one horizontal colour legend at its top
# (above A B / E F). Outer margins are kept small. Total width 17.8 cm.

set_levels_sweep <- function(d) {
  d |>
    mutate(
      trial_type = factor(trial_type, levels = c("risky_safe", "risky_risky")),
      rare_outcome = factor(rare_outcome, levels = c("large", "small"))
    )
}

# One cell of the grid: sweep curves (+ grey asocial benchmark line) for a
# single trial_type x rare_outcome. Both axis TITLES are shown on every cell
# (`ylab` is the row's Y title, passed for all cells); `tag` is the panel
# letter (set explicitly so it is independent of the grid order); x-axis tick
# LABELS follow show_x (currently TRUE on every cell so all panels show 1..5).
make_sweep_cell <- function(sweep_df, bench_df, trial, rare, ylab, color_scale,
                            tag, show_x = FALSE) {
  d  <- filter(sweep_df, trial_type == trial, rare_outcome == rare)
  bd <- filter(bench_df, trial_type == trial, rare_outcome == rare)
  ggplot(d, aes(choice_order, p_choice_B,
    color = sweep_val, group = factor(sweep_val))) +
    geom_line(linewidth = 0.4) +
    geom_point(size = 0.8) +
    geom_line(data = bd, aes(choice_order, p_choice_B),
      inherit.aes = FALSE, color = col_dark, linewidth = 0.5,
      linetype = "1111") +
    scale_x_continuous(breaks = 1:5) +
    color_scale +
    labs(x = "Choice order", y = ylab, tag = tag) +
    theme_minimal(base_size = 9) +
    theme(
      axis.line        = element_line(color = col_dark, linewidth = 0.3),
      axis.ticks       = element_line(color = col_dark, linewidth = 0.3),
      axis.text        = element_text(color = col_dark),
      axis.text.x      = if (show_x) element_text() else element_blank(),
      axis.ticks.x     = if (show_x) {
        element_line(color = col_dark, linewidth = 0.3)
      } else {
        element_blank()
      },
      text             = element_text(color = col_dark),
      panel.grid.minor = element_blank(),
      plot.tag         = element_text(color = col_dark),
      axis.title       = element_text(color = col_dark, size = 8)
    ) +
    theme_transparent
}

# Fresh colour scale per cell (one colour mapping per parameter; legends
# dropped).
cs_theta <- function() {
  scale_color_continuous_sequential(palette = "YlGnBu",
    name = "Conformity exponent")
}
cs_eta <- function() {
  scale_color_continuous_sequential(palette = "YlOrRd",
    name = "Intercept of social weight (logit)")
}

sweep_theta_df <- set_levels_sweep(
  readRDS(here("output/sim/sim_sweep_full_theta.rds"))
)
sweep_eta_df <- set_levels_sweep(
  readRDS(here("output/sim/sim_sweep_full_eta_intercept.rds"))
)
bench_df       <- set_levels_sweep(solo_benchmark)

# Each parameter is its own 2 x 2 block with a single collected colour legend,
# placed horizontally at the TOP of that block (theta = Conformity exponent
# above A B; eta_intercept = Intercept above E F). Tags are passed explicitly
# so theta = A B / C D and eta_intercept = E F / G H. A thin plot_spacer()
# column separates the two blocks.
cell_A <- make_sweep_cell(sweep_theta_df, bench_df, "risky_safe",  "large",
  "P(choose safe)",    cs_theta(), tag = "A", show_x = TRUE)
cell_B <- make_sweep_cell(sweep_theta_df, bench_df, "risky_safe",  "small",
  "P(choose safe)",    cs_theta(), tag = "B", show_x = TRUE)
cell_C <- make_sweep_cell(sweep_theta_df, bench_df, "risky_risky", "large",
  "P(choose jackpot)", cs_theta(), tag = "C", show_x = TRUE)
cell_D <- make_sweep_cell(sweep_theta_df, bench_df, "risky_risky", "small",
  "P(choose jackpot)", cs_theta(), tag = "D", show_x = TRUE)
cell_E <- make_sweep_cell(sweep_eta_df,   bench_df, "risky_safe",  "large",
  "P(choose safe)",    cs_eta(),   tag = "E", show_x = TRUE)
cell_F <- make_sweep_cell(sweep_eta_df,   bench_df, "risky_safe",  "small",
  "P(choose safe)",    cs_eta(),   tag = "F", show_x = TRUE)
cell_G <- make_sweep_cell(sweep_eta_df,   bench_df, "risky_risky", "large",
  "P(choose jackpot)", cs_eta(),   tag = "G", show_x = TRUE)
cell_H <- make_sweep_cell(sweep_eta_df,   bench_df, "risky_risky", "small",
  "P(choose jackpot)", cs_eta(),   tag = "H", show_x = TRUE)

# A guide_area() as the TOP row of each block forces the collected legend to sit
# OUTSIDE (above) the panel tags, at the very top of the figure.
theta_block <- (guide_area() / ((cell_A | cell_B) / (cell_C | cell_D))) +
  plot_layout(guides = "collect", heights = c(1, 12))
eta_block   <- (guide_area() / ((cell_E | cell_F) / (cell_G | cell_H))) +
  plot_layout(guides = "collect", heights = c(1, 12))

fig_sweep_combined <- (theta_block | plot_spacer() | eta_block) +
  plot_layout(widths = c(1, 0.05, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(2, 2, 2, 2))) &
  theme(
    legend.direction  = "horizontal",
    legend.key.width  = unit(0.5, "cm"),
    legend.key.height = unit(0.2, "cm"),
    legend.margin     = margin(2, 2, 0, 2),
    legend.title      = element_text(size = 8, vjust = 1),
    legend.text       = element_text(size = 7),
    legend.ticks.length  = unit(0.05, "cm"),
    # legend.justification = "left",
    plot.tag.position = c(0, 1.05)
  )

save_pdf(fig_sweep_combined, "fig_sweep_combined.pdf", width = 17.8, height = 8)


# Shared helper for the choice-order simulation figures -----------------------
# Consistent trial_type / rare_outcome factor levels for the simulation
# summaries read from output/sim/ (used by the large-group figure below and by
# pool_matrix() in the difference-band block).

set_co_levels <- function(d) {
  d |>
    mutate(
      trial_type = factor(trial_type, levels = c("risky_safe", "risky_risky")),
      rare_outcome = factor(rare_outcome, levels = c("large", "small"))
    )
}


# Figure: choice-order cascade for 5- vs 25-member virtual groups (merged)
# ----- Merges the posterior-draw Group-vs-Solo choice-order curves for TWO
# virtual group sizes into a single 8-panel figure: the 5-member groups (main
# design; sim_post_{full,solo}.rds) as panels A-D, and the n_big-member
# large-group extrapolation (sim_post_{full,solo}_n<n>.rds, default 25) as
# panels E-H. Within each block the four cells are the trial_type x
# rare_outcome combinations, arranged 2 x 2: only the MEAN P(choose B) (no SE
# band); Group = yellow circles, Solo = grey triangles (main-text cascade
# colours/shapes); every cell shows both axis titles and numeric x-tick labels
# and carries its own horizontal Group/Solo legend on top. The two blocks are
# placed side by side, each under a "Group size = n" title: A-D (5-member) on
# the left, E-H (n_big-member) on the right. Requires both the n5 (default) and
# n<n_big> caches from 03_simulation.R; skipped with a message if any are
# absent.

# Large-group size used by 03_simulation.R (group_size_big); the n<n_big>
# caches must match it.
n_big <- 25L

post_merge_paths <- c(
  full5  = here("output/sim/sim_post_full.rds"),
  solo5  = here("output/sim/sim_post_solo.rds"),
  full_n = here(sprintf("output/sim/sim_post_full_n%d.rds", n_big)),
  solo_n = here(sprintf("output/sim/sim_post_solo_n%d.rds", n_big))
)

if (all(file.exists(post_merge_paths))) {
  # Combine one (full, solo) pair into a single long frame tagged by condition.
  read_post_pair <- function(full_path, solo_path) {
    bind_rows(
      set_co_levels(readRDS(full_path)) |> mutate(condition = "real_group"),
      set_co_levels(readRDS(solo_path)) |> mutate(condition = "virtual_solo")
    ) |>
      mutate(
        condition = factor(condition, levels = c("real_group", "virtual_solo"))
      )
  }

  post5_df <- read_post_pair(
    post_merge_paths[["full5"]], post_merge_paths[["solo5"]]
  )
  postn_df <- read_post_pair(
    post_merge_paths[["full_n"]], post_merge_paths[["solo_n"]]
  )

  # One (trial_type x rare_outcome) cell. Takes the data frame and group size n
  # explicitly so it serves both blocks;
  # n controls the x-axis breaks (all orders when n <= 5, else 1 and every 5th).
  make_postmerge_cell <- function(df, n, trial, rare, ylab, tag,
                                  show_x = TRUE) {
    d <- filter(df, trial_type == trial, rare_outcome == rare)
    x_breaks <- if (n <= 5) seq_len(n) else c(1, seq(5, n, 5))
    ggplot(d, aes(choice_order, p_choice_B,
      color = condition, shape = condition, group = condition)) +
      geom_line(linewidth = 0.4, show.legend = FALSE) +
      geom_point(size = 0.9) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      scale_color_manual(
        values = col_cascade, name = NULL, labels = cascade_labels,
        breaks = c("virtual_solo", "real_group")) +
      scale_shape_manual(
        values = cascade_shape, name = NULL, labels = cascade_labels,
        breaks = c("virtual_solo", "real_group")) +
      labs(x = "Choice order", y = ylab, tag = tag) +
      theme_minimal(base_size = 9) +
      theme(
        axis.line          = element_line(color = col_dark, linewidth = 0.3),
        axis.ticks         = element_line(color = col_dark, linewidth = 0.3),
        axis.text          = element_text(color = col_dark),
        axis.text.x        = if (show_x) element_text() else element_blank(),
        axis.ticks.x       = if (show_x) {
          element_line(color = col_dark, linewidth = 0.3)
        } else {
          element_blank()
        },
        text               = element_text(color = col_dark),
        panel.grid.minor   = element_blank(),
        plot.tag           = element_text(color = col_dark),
        axis.title         = element_text(color = col_dark, size = 8),
        legend.position    = "top",
        legend.direction   = "horizontal",
        legend.text        = element_text(size = 7),
        legend.key.size    = unit(0.3, "cm"),
        legend.margin      = margin(0, 0, 0, 0),
        legend.box.spacing = unit(1, "pt")
      ) +
      theme_transparent
  }

  # 5-member groups -> panels A-D.
  pm_A <- make_postmerge_cell(
    post5_df, 5L, "risky_safe", "large", "P(choose safe)", "A"
  )
  pm_B <- make_postmerge_cell(
    post5_df, 5L, "risky_safe", "small", "P(choose safe)", "B"
  )
  pm_C <- make_postmerge_cell(
    post5_df, 5L, "risky_risky", "large", "P(choose jackpot)", "C"
  )
  pm_D <- make_postmerge_cell(
    post5_df, 5L, "risky_risky", "small", "P(choose jackpot)", "D"
  )
  # n_big-member groups -> panels E-H.
  pm_E <- make_postmerge_cell(
    postn_df, n_big, "risky_safe", "large", "P(choose safe)", "E"
  )
  pm_F <- make_postmerge_cell(
    postn_df, n_big, "risky_safe", "small", "P(choose safe)", "F"
  )
  pm_G <- make_postmerge_cell(
    postn_df, n_big, "risky_risky", "large", "P(choose jackpot)", "G"
  )
  pm_H <- make_postmerge_cell(
    postn_df, n_big, "risky_risky", "small", "P(choose jackpot)", "H"
  )

  # Per-block title strip (a text grob wrapped as a patchwork element), placed
  # as a short row above each 2 x 2 block.
  title_gp <- grid::gpar(fontsize = 10, col = col_dark)
  hdr5 <- wrap_elements(full = grid::textGrob("Group size = 5", gp = title_gp))
  hdrn <- wrap_elements(full = grid::textGrob(sprintf("Group size = %d", n_big),
    gp = title_gp))

  # 5-member block (A-D) and n_big-member block (E-H), each titled, then merged
  # horizontally (5-member on the left, n_big-member on the right).
  block5 <- hdr5 / (pm_A | pm_B) / (pm_C | pm_D) +
    plot_layout(heights = c(0.08, 1, 1))
  blockn <- hdrn / (pm_E | pm_F) / (pm_G | pm_H) +
    plot_layout(heights = c(0.08, 1, 1))

  fig_post_full_5_and_n <- (block5 | blockn) +
    plot_annotation(theme = theme(plot.margin = margin(2, 2, 2, 2)))

  save_pdf(fig_post_full_5_and_n, sprintf("fig_post_full_5_and_n%d.pdf", n_big),
    width = 16, height = 9)
} else {
  message("Skipping fig_post_full_5_and_n", n_big,
    ": need both the n5 (sim_post_{full,solo}.rds) and n", n_big,
    " caches; run 03_simulation.R.")
}


# Figure: between-condition difference band, N = 33 vs N = 1000 ---------------
# Does the group-vs-solo difference, unclear with 33 groups, sharpen with 1000?
# Between-subjects contrast (matching the real design): independently resample
# N virtual groups from EACH condition's per-group pool (sim_post_*_group.rds),
# pool each into a choice-order curve, and take group - solo. Solo and group
# are independent populations, so resamples are drawn INDEPENDENTLY (no rep
# pairing); the difference-band variance is var_group/N + var_solo/N. The band
# narrows ~1/sqrt(N), so N=33 vs N=1000 shows whether the cascade clears the
# 0-line.

# Wide [group x cell] matrix of per-group choice rates for fast resampling,
# plus a key mapping each cell back to choice_order x trial_type x
# rare_outcome.
pool_matrix <- function(rds) {
  d <- set_co_levels(readRDS(rds)) |>
    arrange(choice_order, trial_type, rare_outcome) |>
    mutate(cell = paste(choice_order, trial_type, rare_outcome, sep = "|"))
  key <- distinct(d, cell, choice_order, trial_type, rare_outcome)
  w <- d |>
    select(group_sim, cell, p_choice_B) |>
    pivot_wider(names_from = cell, values_from = p_choice_B)
  list(mat = as.matrix(w[, -1]), cells = colnames(w)[-1], key = key)
}

# R bootstrap pooled curves (rows = replicates, cols = cells) for a given N.
boot_curves <- function(mat, N, R, seed) {
  set.seed(seed)
  G <- nrow(mat)
  t(vapply(
    seq_len(R),
    \(b) {
      colMeans(mat[sample.int(G, N, replace = TRUE), , drop = FALSE],
        na.rm = TRUE
      )
    },
    numeric(ncol(mat))
  ))
}

# Difference (group - solo) band for one N. Independent seeds -> the two
# bootstrap streams are independent, so (group_b - solo_b) samples the
# between-condition difference distribution with the correct summed variance.
diff_band <- function(N, R = 2000L) {
  gp <- pool_matrix(here("output/sim/sim_post_full_group.rds"))
  sp <- pool_matrix(here("output/sim/sim_post_solo_group.rds"))
  stopifnot(identical(gp$cells, sp$cells))
  d <- boot_curves(gp$mat, N, R, seed = 1) - boot_curves(sp$mat, N, R, seed = 2)
  tibble(
    cell = gp$cells,
    lo95  = apply(d, 2, quantile, 0.025), lo50 = apply(d, 2, quantile, 0.25),
    med   = apply(d, 2, median),
    meanv = apply(d, 2, mean),
    hi50 = apply(d, 2, quantile, 0.75), hi95 = apply(d, 2, quantile, 0.975)
  ) |>
    left_join(gp$key, by = "cell") |>
    mutate(N = N)
}

# N ordered 1000 then 33 so the 33-group layer is drawn LAST (in front of the
# 1000-group band); legend order is kept 33 -> 1000 via `breaks` below.
diff_df <- bind_rows(diff_band(33L), diff_band(1000L)) |>
  set_co_levels() |>
  mutate(N = factor(N, levels = c(1000L, 33L),
    labels = c("1,000 groups", "33 groups")))

diff_col <- c("33 groups" = col_grey, "1,000 groups" = col_yellow)
diff_lty <- c("33 groups" = "dashed", "1,000 groups" = "solid")
diff_shape <- c("33 groups" = 17, "1,000 groups" = 16)  # triangle / circle
# 33 lighter, 1000 darker
diff_alpha <- c("33 groups" = 0.1, "1,000 groups" = 0.3)
diff_breaks <- c("33 groups", "1,000 groups")


# Figure: pi_copy as a function of choice order (group condition) -------------
# Back-calculates the model-implied copying weight pi_copy for every group
# consequential choice, then plots it against choice order within each
# group x problem. Social information is reconstructed from PREDECESSORS only:
# a member's social evidence is built from the ACTUAL choices of group-mates who
# stopped with strictly fewer samples (n_samples_self), so the first decider
# (choice order 1) has no social information by construction and is excluded.
# This avoids the lag artefact in the recorded social counts whereby a few
# fewest-sample deciders nonetheless register one observed choice.
#
# pi_copy uses each subject's posterior-median eta parameters from
# fit_group_full and the same unit-SD scales as the fit
# (output/fit/group_scales.rds, written by 02_modeling.R):
#   pi_copy = inv_logit(eta_intercept + eta_strength * |u_t| / beta_scale[6]
#                       + eta_social_strength * |s_t| / social_scale[1])
#   u_t = tanh(delta_t / k),  delta_t = (mean_B - mean_A) / se_diff at stopping
#   s_t = tanh(log((social_B + 0.1) / (social_A + 0.1)))  from predecessors only

source(here("function/R/utils.R"))  # compute_delta_t_at_stop()

group_scales      <- readRDS(here("output/fit/group_scales.rds"))
s6                <- group_scales$beta_scale[[6]]   # SD of |u_t| (own-evidence)
# SD of |s_t| (social-evidence)
ss1 <- group_scales$social_scale[[1]]
# pooled tanh scaling for delta_t
k_g <- group_scales$k
fit_group_full    <- readRDS(here("output/fit/fit_group_full.rds"))

# Per-subject posterior-median copy (eta) parameters. The subject index matches
# the fit: as.numeric(as.factor(subject_id)) over the group-condition trials.
eta_subj <- fit_group_full |>
  spread_draws(
    eta_intercept[subj], eta_strength[subj], eta_social_strength[subj]
  ) |>
  median_hdci(eta_intercept, eta_strength, eta_social_strength) |>
  select(subj, eta_intercept, eta_strength, eta_social_strength)

subj_map_group <- df_trials |>
  filter(condition == "group") |>
  distinct(subject_id) |>
  mutate(subj = as.numeric(as.factor(subject_id)))

# delta_t at stopping per (subject_id, option_id), group condition (mirrors the
# k_group construction in 03_simulation.R).
df_delta_group <- df_sampling |>
  filter(condition == "group") |>
  left_join(df_options, by = "option_id") |>
  mutate(is_higher_outcome = case_when(
    sampled_option_label == "a" & sampled_outcome == v_a_high ~ 1,
    sampled_option_label == "a" & sampled_outcome == v_a_low  ~ 0,
    sampled_option_label == "b" & sampled_outcome == v_b_high ~ 1,
    sampled_option_label == "b" & sampled_outcome == v_b_low  ~ 0
  )) |>
  group_by(subject_id, option_id) |>
  group_modify(~ tibble(delta_t = compute_delta_t_at_stop(.x))) |>
  ungroup()

# Reconstruct predecessor-based social counts within each group x problem, then
# combine evidence into pi_copy. Predecessors = members with strictly fewer
# samples; their choice_self (0 = A, 1 = B) forms social_A / social_B.
df_picopy_order <- df_trials |>
  filter(
    condition == "group", trial_type %in% c("risky_safe", "risky_risky")
  ) |>
  group_by(group_id, option_id) |>
  group_modify(function(cell, key) {
    ns <- cell$n_samples_self
    ch <- cell$choice_self
    cell$social_A <- vapply(
      seq_along(ns), \(i) sum(ns < ns[i] & ch == 0), numeric(1)
    )
    cell$social_B <- vapply(
      seq_along(ns), \(i) sum(ns < ns[i] & ch == 1), numeric(1)
    )
    cell$choice_order <- rank(ns, ties.method = "min")
    cell
  }) |>
  ungroup() |>
  left_join(df_delta_group, by = c("subject_id", "option_id")) |>
  left_join(subj_map_group, by = "subject_id") |>
  left_join(eta_subj,       by = "subj") |>
  # pi_copy is defined only when social info is present; order 1 (no
  # predecessor) has social_A = social_B = 0 and is dropped here.
  filter(is.finite(delta_t), (social_A + social_B) > 0) |>
  mutate(
    u_t     = abs(tanh(delta_t / k_g)),
    s_t     = abs(tanh(log((social_B + 0.1) / (social_A + 0.1)))),
    pi_copy = plogis(eta_intercept + eta_strength * (u_t / s6) +
      eta_social_strength * (s_t / ss1))
  )

theme_picopy_order <- list(
  theme_minimal(base_size = 9),
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 8, color = col_dark),
    text             = element_text(color = col_dark)
  ),
  theme_transparent
)

# Both pi_copy-by-choice-order figures below show the posterior median and 95%
# HDCI of the cell-mean pi_copy: for each posterior draw, pi_copy is recomputed
# per decision with that draw's subject eta and averaged within the cell, and
# the resulting posterior of the cell mean is summarised. This matches the
# credible- band convention of the pi_copy panels above (an earlier version
# plotted the across-decision SE of the median-eta point estimate, which is not
# a credible interval). Draws are thinned to 1000 for tractability (u_t / s_t
# are fixed data, reused from df_picopy_order).
eta_draws_subj <- fit_group_full |>
  spread_draws(
    eta_intercept[subj], eta_strength[subj], eta_social_strength[subj],
    ndraws = 1000, seed = 1)

# Posterior of the cell-mean pi_copy (one value per draw x cell), summarised to
# the median + 95% HDCI per cell.
picopy_order_hdci <- df_picopy_order |>
  select(subj, trial_type, rare_outcome, choice_order, u_t, s_t) |>
  inner_join(eta_draws_subj, by = "subj", relationship = "many-to-many") |>
  mutate(pi_copy = plogis(eta_intercept + eta_strength * (u_t / s6) +
    eta_social_strength * (s_t / ss1))) |>
  group_by(.draw, trial_type, rare_outcome, choice_order) |>
  summarise(m = mean(pi_copy), .groups = "drop") |>
  group_by(trial_type, rare_outcome, choice_order) |>
  median_hdci(m, .width = 0.95) |>
  ungroup()

# Same posterior of the cell-mean pi_copy, pooled over the four cells, plus the
# rise from the 2nd to the 5th decider (differenced within draw, so the
# interval accounts for the posterior correlation between the two orders).
# Cached with the per-cell table so the text can quote how far the social
# weight climbs along choice order.
picopy_order_pooled_draws <- df_picopy_order |>
  select(subj, choice_order, u_t, s_t) |>
  inner_join(eta_draws_subj, by = "subj", relationship = "many-to-many") |>
  mutate(pi_copy = plogis(eta_intercept + eta_strength * (u_t / s6) +
    eta_social_strength * (s_t / ss1))) |>
  group_by(.draw, choice_order) |>
  summarise(m = mean(pi_copy), .groups = "drop")

picopy_order_pooled <- picopy_order_pooled_draws |>
  group_by(choice_order) |>
  median_hdci(m, .width = 0.95) |>
  ungroup()

picopy_order_rise <- picopy_order_pooled_draws |>
  filter(choice_order %in% c(2, 5)) |>
  pivot_wider(names_from = choice_order, values_from = m, names_prefix = "o") |>
  mutate(delta = o5 - o2) |>
  median_hdci(delta, .width = 0.95)

# Rise from the 2nd to the 5th decider within each trial type x rare outcome
# cell (differenced within draw, as above).
picopy_order_rise_cell <- df_picopy_order |>
  select(subj, trial_type, rare_outcome, choice_order, u_t, s_t) |>
  inner_join(eta_draws_subj, by = "subj", relationship = "many-to-many") |>
  mutate(pi_copy = plogis(eta_intercept + eta_strength * (u_t / s6) +
    eta_social_strength * (s_t / ss1))) |>
  group_by(.draw, trial_type, rare_outcome, choice_order) |>
  summarise(m = mean(pi_copy), .groups = "drop") |>
  filter(choice_order %in% c(2, 5)) |>
  pivot_wider(names_from = choice_order, values_from = m, names_prefix = "o") |>
  mutate(delta = o5 - o2) |>
  group_by(trial_type, rare_outcome) |>
  median_hdci(delta, .width = 0.95) |>
  ungroup()

save(
  picopy_order_hdci,
  picopy_order_pooled,
  picopy_order_rise,
  picopy_order_rise_cell,
  file = here("output/fit/picopy_by_choice_order.rda")
)

# Shared look of the pi_copy-by-choice-order panels, kept as a list so the
# facetting and strip labels are the only thing the figure below sets.
picopy_order_layers <- list(
  geom_line(color = "#ea801c", linewidth = 0.4),
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
    color = "#ea801c", size = 0.3, linewidth = 0.4),
  scale_x_continuous(breaks = 2:5),
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3)),
  labs(x = "Choice order", y = expression(paste("Social weight (", pi, ")"))),
  theme_picopy_order,
  theme(
    panel.spacing = unit(1.1, "lines"),
    axis.line     = element_line(color = col_dark, linewidth = 0.3)
  )
)

# Unified single-column panel ordering, shared with the cascade column below so
# the two columns of the combined figure list the 4 cells in the same order.
panel_levels_4 <- c("Risky/Safe, rare = large",  "Risky/Safe, rare = small",
  "Risky/Risky, rare = large", "Risky/Risky, rare = small")
add_panel4 <- function(df) {
  df |> mutate(panel = factor(case_when(
    trial_type == "risky_safe"  & rare_outcome == "large" ~ panel_levels_4[1],
    trial_type == "risky_safe"  & rare_outcome == "small" ~ panel_levels_4[2],
    trial_type == "risky_risky" & rare_outcome == "large" ~ panel_levels_4[3],
    trial_type == "risky_risky" & rare_outcome == "small" ~ panel_levels_4[4]
  ), levels = panel_levels_4))
}

# pi_copy by choice order: median + 95% HDCI, one panel per cell (2 x 2 grid).
# Strip labels are dropped and the x/y axis lines are drawn explicitly.
fig_picopy_order_facet <- picopy_order_hdci |>
  add_panel4() |>
  ggplot(aes(choice_order, m)) +
  picopy_order_layers +
  facet_wrap(~panel, ncol = 2) +
  theme(strip.text = element_blank())

save_pdf(fig_picopy_order_facet, "fig_picopy_by_choice_order_facet.pdf",
  width = 8.5, height = 5)


# Consensus building along choice order (paper figure)
# ------------------------- Do adjacent deciders agree more in the real
# interacting groups than in independent individuals? For each group x option,
# members are ordered by own sample count (fewer = earlier) and each adjacent
# pair (1-2 .. 4-5) scores 1 if the two chose the same option. Real groups are
# compared to a virtual-solo baseline: solo participants are randomly
# partitioned into pseudo-groups matching the real size composition (17 x 4, 16
# x 5), ordered the same way, and pair agreement is recomputed over 1000
# resamples (mean +/- 95% band). Because the baseline shares the solo marginal
# choice rates, any EXCESS agreement in real groups reflects social
# coordination; its growth with choice order is the signature of an
# accumulating cascade.
#
# The numbers themselves are computed in 01_descriptive.R (which caches the 1000
# virtual-solo resamples); this script only draws them, so run 01_descriptive.R
# first if output/sim/consensus_agreement.rda is missing or stale.

load(here("output/sim/consensus_agreement.rda"))  # consensus_stats

pair_levels <- c("1-2", "2-3", "3-4", "4-5")
solo_band <- consensus_stats |>
  transmute(x = match(order_pair, pair_levels), mean = virtual_solo, lo, hi)
real_line <- consensus_stats |>
  transmute(x = match(order_pair, pair_levels), agree = real)

# Legend order: Solo first, then Group. Solo points = triangles,
# Group points = circles (mapped so both appear in the legend).
cons_breaks <- c("Solo", "Group")

fig_consensus_order <- ggplot() +
  geom_ribbon(data = solo_band, aes(x, ymin = lo, ymax = hi),
    fill = col_grey, alpha = 0.2) +
  geom_line(data = solo_band, aes(x, mean, color = "Solo"),
    linewidth = 0.5) +
  geom_point(data = solo_band,
    aes(x, mean, color = "Solo", shape = "Solo"),
    size = 1.8) +
  geom_line(data = real_line, aes(x, agree, color = "Group"), linewidth = 0.5) +
  geom_point(data = real_line,
    aes(x, agree, color = "Group", shape = "Group"), size = 1.8) +
  scale_x_continuous(breaks = seq_along(pair_levels), labels = pair_levels) +
  ylim(0.52, 0.8) +
  scale_color_manual(name = NULL,
    values = c("Group" = col_yellow, "Solo" = col_grey),
    breaks = cons_breaks) +
  scale_shape_manual(name = NULL,
    values = c("Group" = 16, "Solo" = 17),
    breaks = cons_breaks) +
  labs(x = "Adjacent decider pair (choice order)", y = "P(same choice)") +
  theme_minimal(base_size = 9) +
  theme(
    axis.line        = element_line(color = col_dark),
    axis.text        = element_text(color = col_dark),
    text             = element_text(color = col_dark),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.key.height = unit(0.35, "cm")
  ) +
  theme_transparent


# theta / eta_intercept -> adjacent-decider agreement (supplementary figure)
# --- Simulation counterpart of the observed consensus curve above
# (fig_consensus_order): P(adjacent deciders make the SAME choice) at each choice-order pair, under group_full.stan with
# every member's theta (left) or eta_intercept (right) CLAMPED to a swept
# value. Higher values -> more agreement that grows along choice order = the
# signature of a stronger information cascade. The dotted line is the asocial
# solo benchmark (solo.stan, no social info), which is flat across choice
# order. Agreement is computed in 03_simulation.R; the two panels are joined
# side by side.

pair_labs <- c("1-2", "2-3", "3-4", "4-5")

# Asocial solo benchmark (single curve, shared by both panels; dotted).
solo_agree_df <- readRDS(here("output/sim/sim_solo_agreement.rds")) |>
  mutate(x = match(order_pair, pair_labs))

# One agreement panel: swept-value coloured curves + dotted solo benchmark.
make_agreement_panel <- function(rds, color_scale) {
  df <- readRDS(rds) |>
    mutate(x = match(order_pair, pair_labs))
  ggplot(df, aes(x, agree, color = sweep_val, group = sweep_val)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.6) +
    geom_line(data = solo_agree_df, aes(x, agree), inherit.aes = FALSE,
      color = col_dark, linewidth = 0.5, linetype = "dotted") +
    scale_x_continuous(breaks = 1:4, labels = pair_labs) +
    ylim(0.52, 0.8) +
    color_scale +
    labs(x = "Adjacent decider pair (choice order)", y = "P(same choice)") +
    theme_minimal(base_size = 9) +
    theme(
      axis.line         = element_line(color = col_dark),
      axis.text         = element_text(color = col_dark),
      text              = element_text(color = col_dark),
      panel.grid.minor  = element_blank(),
      legend.position   = "top",
      legend.key.height = unit(0.3, "cm"),
      legend.key.width  = unit(0.7, "cm")
    ) +
    theme_transparent
}

fig_agreement_theta <- make_agreement_panel(
  here("output/sim/sim_sweep_theta_agreement.rds"),
  scale_color_continuous_sequential(
    palette = "YlGnBu",
    name = "Conformity exponent"
  )
)

fig_agreement_eta <- make_agreement_panel(
  here("output/sim/sim_sweep_eta_intercept_agreement.rds"),
  scale_color_continuous_sequential(
    palette = "YlOrRd",
    name = "Intercept of social weight (logit)"
  )
)

# Side by side: theta (left) | eta_intercept (right), panel letters A / B.
fig_agreement_combined <-
  (fig_consensus_order + fig_agreement_theta + fig_agreement_eta) +
    plot_annotation(tag_levels = "A") &
    theme(
      legend.direction  = "horizontal",
      legend.key.width  = unit(0.5, "cm"),
      legend.key.height = unit(0.2, "cm"),
      legend.margin     = margin(2, 2, 0, 2),
      legend.title      = element_text(size = 8, hjust = 0.5),
      legend.title.position = "top",
      legend.text       = element_text(size = 7),
      legend.ticks.length  = unit(0.05, "cm"),
      # legend.justification = "center",
      plot.tag.position = c(0, 1)
    )

ggsave(
  file.path(out_dir, "fig_agreement_by_order_theta_eta.pdf"),
  fig_agreement_combined, width = 17.8, height = 8, units = "cm",
  bg = "transparent"
)

# Same panels split into one image per facet -----------------------------------
# 8 standalone single-panel PDFs (4 cascade + 4 pi_copy), one per trial-type x
# rare-outcome cell. Cascade keeps the per-trial-type y label (safe / jackpot);
# each file carries the cell name as its title.
cells4 <- tribble(
  ~tt,           ~ro,     ~slug,      ~title,
  "risky_safe",  "large", "rs_large", "Risky/Safe, rare = large",
  "risky_safe",  "small", "rs_small", "Risky/Safe, rare = small",
  "risky_risky", "large", "rr_large", "Risky/Risky, rare = large",
  "risky_risky", "small", "rr_small", "Risky/Risky, rare = small"
)

for (i in seq_len(nrow(cells4))) {
  cell <- cells4[i, ]
  ylab <- if (cell$tt == "risky_safe") "P(choose safe)" else "P(choose jackpot)"

  # real_group + the single 33-group solo set (cascade_df, defined above), so
  # solo and group share the same +/- 1 SE via stat_summary.
  p_casc <- cascade_df |>
    filter(trial_type == cell$tt, rare_outcome == cell$ro) |>
    ggplot(aes(choice_order, choice_self,
      color = condition, fill = condition, shape = condition)) +
    stat_summary() +
    stat_summary(geom = "line", show.legend = FALSE) +
    scale_color_manual(
      values = col_cascade, name = NULL, labels = cascade_labels
    ) +
    scale_fill_manual(
      values = col_cascade, name = NULL, labels = cascade_labels
    ) +
    scale_shape_manual(
      values = cascade_shape, name = NULL, labels = cascade_labels
    ) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    scale_y_continuous(labels = scales::label_number(0.01)) +
    labs(x = "Choice order", y = ylab) +
    theme_picopy_order +
    theme(legend.position = c(0.5, 1.12), axis.title = element_text(size = 8),
      legend.text = element_text(size = 6, margin = margin(l = 1)),
      legend.key.size = unit(6, "pt"),
      legend.key.spacing.x = unit(6, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(2, "pt"),
      legend.direction = "horizontal",
      plot.margin = margin(4, 10, 0, 0))

  # Per-cell between-condition difference band (33 vs 1,000 groups): 33 groups
  # dashed and drawn in front of 1000. diff_df has its own `cell` column, so
  # use .env$ to reach the loop variable (a bare `cell$tt` would resolve to the
  # data column and error). Panel F (the rr_large difference panel) alone drops
  # the 0.10 tick, which sits at the very top of its axis; the other cells keep
  # the default breaks. Written out rather than via seq(), whose inexact 0
  # would push the whole axis into scientific notation.
  noise_y_breaks <- if (cell$slug == "rr_large") {
    c(-0.15, -0.10, -0.05, 0, 0.05)
  } else {
    waiver()
  }

  p_noise <- diff_df |>
    filter(trial_type == .env$cell$tt, rare_outcome == .env$cell$ro) |>
    ggplot(aes(choice_order, group = N)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = col_dark) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95, fill = N, alpha = N)) +
    geom_line(aes(y = meanv, color = N, linetype = N), linewidth = 0.4,
      data = ~ filter(.x, N == "1,000 groups")) +
    geom_point(aes(y = meanv, color = N, shape = N), size = 0.9,
      data = ~ filter(.x, N == "1,000 groups")) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    scale_y_continuous(breaks = noise_y_breaks) +
    # Legend keyed on the band (fill + alpha merged); line/point aesthetics are
    # dropped from the legend so the keys read as shaded bands.
    scale_color_manual(
      name = NULL, values = c("#f2c45f", "#ea801c"), breaks = diff_breaks,
      guide = "none"
    ) +
    scale_fill_manual(
      name = NULL, values = c("#f2c45f", "#ea801c"), breaks = diff_breaks
    ) +
    scale_linetype_manual(
      name = NULL, values = diff_lty, breaks = diff_breaks, guide = "none"
    ) +
    scale_shape_manual(
      name = NULL, values = diff_shape, breaks = diff_breaks, guide = "none"
    ) +
    scale_alpha_manual(
      name = NULL, values = c(0.2, 0.4), breaks = diff_breaks
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = c(0.3, 0.6)))) +
    labs(x = "Choice order", y = "P(group) - P(solo)") +
    theme_picopy_order +
    theme(legend.position = c(0.4, 1.12), axis.title = element_text(size = 8),
      legend.text = element_text(size = 6, margin = margin(l = 1)),
      legend.key.size = unit(6, "pt"),
      legend.key = element_rect(color = "black", linewidth = 0.1),
      legend.key.spacing.x = unit(4, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0, "pt"),
      legend.direction = "horizontal",
      plot.margin = margin(4, 4, 0, 10))

  # Side-by-side cascade | between-condition difference for this cell (4
  # files). Panel tags run A,B (rs_large), C,D (rs_small), E,F (rr_large), G,H
  # (rr_small).
  cell_tags <- LETTERS[(2 * i - 1):(2 * i)]
  p_cell <- (p_casc | p_noise) +
    theme(plot.margin = margin(0, 0, 0, 0))
  save_pdf(p_cell, paste0("fig_cascade_noise_", cell$slug, ".pdf"),
    width = 7, height = 3)
}
