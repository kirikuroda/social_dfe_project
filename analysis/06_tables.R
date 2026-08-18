# Generate LaTeX tables for supplementary materials

library(tidyverse)
library(here)
library(brms)
library(marginaleffects)
library(tidybayes)
library(posterior)
library(knitr)
library(kableExtra)
load(here("data/data.rda"))
source(here("function/R/utils.R"))  # add_option_no()

# Force the float placement of a rendered (kableExtra) LaTeX table to [htbp].
force_htbp <- function(tex) {
  sub("\\\\begin\\{table\\}\\[[!htbpH]*\\]", "\\\\begin{table}[htbp]", tex)
}


# n_samples effect (solo condition, fit_choice_solo_brms) -----------
df_slope <- readRDS(here("output/fit/slope_choice_solo.rds")) |>
  as_tibble() |>
  mutate(
    trial_type_fmt   = case_when(
      trial_type   == "risky_safe"  ~ "Risky/Safe",
      trial_type   == "risky_risky" ~ "Risky/Risky"
    ),
    rare_outcome_fmt = case_when(
      rare_outcome == "large" ~ "High",
      rare_outcome == "small" ~ "Low"
    ),
    figure_panel = case_when(
      trial_type_fmt == "Risky/Safe" & rare_outcome_fmt == "High" ~ "1D",
      trial_type_fmt == "Risky/Safe" & rare_outcome_fmt == "Low"  ~ "1E",
      trial_type_fmt == "Risky/Risky" & rare_outcome_fmt == "High" ~ "1F",
      trial_type_fmt == "Risky/Risky" & rare_outcome_fmt == "Low"  ~ "1G"
    ),
    trial_type = factor(trial_type, levels = c("risky_safe", "risky_risky")),
    rare_outcome = factor(rare_outcome, levels = c("large", "small"))
  ) |>
  arrange(trial_type, rare_outcome)

rows_1 <- df_slope |>
  pmap_chr(\(trial_type_fmt, figure_panel, estimate, conf.low, conf.high, ...) {
    sprintf(
      "%s & %s & $%.2f$ & $%.2f$ & $%.2f$ \\\\",
      trial_type_fmt, figure_panel, estimate, conf.low, conf.high
    )
  })

tex_1 <- c(
  "\\vspace{2em}",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Effects of sample size on consequential choices in the solo",
  "condition.",
  "Estimates are marginal slopes for the standardized sample size on the",
  "log-odds scale with 95\\% highest posterior density intervals (HPDI), from",
  "a",
  "Bayesian mixed logistic regression.",
  "\\label{tab:solo-slopes}}",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "\\multicolumn{2}{l}{} & & \\multicolumn{2}{c}{95\\% HPDI} \\\\",
  "\\cmidrule(lr){4-5}",
  "Trial type & Figure & Estimate & Lower & Upper \\\\",
  "\\midrule",
  rows_1,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_1, here("output/table/solo-slopes.tex"))


# All choice problems ----------------------------------------------------------
# Prepare data
# Figure panel each problem appears in (trial_type x rare_outcome); attention
# checks have no panel.
df_tbl <- df_options |>
  add_option_no() |>
  mutate(
    type = case_when(
      trial_type == "risky_risky"     ~ "Risky/Risky",
      trial_type == "risky_safe"      ~ "Risky/Safe",
      trial_type == "attention_check" ~ "Attention check"
    ),
    figure = case_when(
      trial_type == "risky_safe" & rare_outcome == "large" ~
        "1D, 3A, 3B, 4A, 4E",
      trial_type == "risky_safe" & rare_outcome == "small" ~
        "1E, 3C, 3D, 4B, 4F",
      trial_type == "risky_risky" & rare_outcome == "large" ~
        "1F, 3E, 3F, 4C, 4G",
      trial_type == "risky_risky" & rare_outcome == "small" ~
        "1G, 3G, 3H, 4D, 4H",
      TRUE                                                  ~ "---"
    ),
    v_b_low_fmt = if_else(
      trial_type == "risky_safe", "---", sprintf("%.1f", v_b_low)
    )
  )

# Format a single data row. The leading No. column is the sequential option
# number that the per-option figure panels also use.
fmt_row <- function(r) {
  sprintf(
    "%d & %s & %s & %.1f & %.1f & %.1f & %.1f & %s & %.1f \\\\",
    r$option_no, r$figure, r$type,
    r$v_a_high, r$v_a_low, r$p_a_high,
    r$v_b_high, r$v_b_low_fmt, r$p_b_high
  )
}

# Build row block with separators: addlinespace between figure panels, midrule
# before the attention-check block.
rows <- character(0)
for (i in seq_len(nrow(df_tbl))) {
  if (i > 1) {
    prev <- df_tbl[i - 1, ]
    cur <- df_tbl[i, ]
    if (cur$trial_type == "attention_check" &&
      prev$trial_type != "attention_check") {
      rows <- c(rows, "\\midrule")
    } else if (cur$figure != prev$figure) {
      rows <- c(rows, "\\addlinespace")
    }
  }
  rows <- c(rows, fmt_row(df_tbl[i, ]))
}

# Note text
note <- paste(
  "In the main trials (excluding attention checks), the expected values of",
  "Option A and Option B were equal,",
  "though values are rounded to two decimal places.",
  "Payoffs are in points (1 point = \\pounds 0.03).",
  "The labels ``Option A'' and ``Option B'' were added for clarity;",
  "participants did not see these labels during the experiment."
)

# Assemble full LaTeX table
tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  paste0(
    "\\caption{All choice problems used in the experiment. ",
    "For each option, High and Low are the two possible payoffs (in points) ",
    "and $P$(High) is the probability of the High payoff. ",
    "The Figure column lists the panels in which each problem type appears.",
    "\\label{tab:options}}"
  ),
  "\\resizebox{\\linewidth}{!}{%",
  "{\\fontsize{8}{8}\\selectfont\\renewcommand{\\arraystretch}{0.5}",
  "\\begin{tabular}{rllrrrrrr}",
  "\\toprule",
  "& & & \\multicolumn{3}{c}{Option A} & \\multicolumn{3}{c}{Option B} \\\\",
  "\\cmidrule(lr){4-6}\\cmidrule(lr){7-9}",
  "No. & Figure & Type & High & Low & $P$(High) & High & Low & $P$(High) \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "}%",  # close font group
  "}",  # close \resizebox
  "\\end{table}"
)

writeLines(tex, here("output/table/options.tex"))

# Table: Posterior summary + convergence diagnostics (solo & group models) -----
# Summarize population-level hyperparameters (mu, sigma) for BOTH the solo and
# the group full models with the posterior median, 95% HPDI, Rhat, and bulk
# ESS, then render as a single LaTeX table. Parameters are matched by name
# across models; group-only rows (social stopping betas, gamma_social, and the
# copy- stage eta/theta) show "---" in the solo columns. Row indices follow the
# headers of function/Stan/solo.stan and function/Stan/group_full.stan.

# Master parameter map: common LaTeX label and the mu/sigma row index in each
# model (NA = parameter absent from that model). Own-evidence terms are labelled
# "personal" and their social counterparts "social"; beta_confirm (group row 10)
# is the own/social alignment term and is labelled beta_alignment.
# Rows are grouped by decision stage in the order the stages occur within a
# trial: sampling (gamma), stopping (beta), then the consequential choice
# (tau / eta / theta). `stage` drives the pack_rows() section headings below.
param_map <- tribble(
  ~order, ~stage,                 ~param,                              ~i_solo,     ~i_group,
  1L,  "Sampling",             "$\\gamma_0$",                          8L,          11L,
  2L,  "Sampling",             "$\\gamma_{\\text{value}}$",            9L,          12L,
  3L,  "Sampling",             "$\\gamma_{\\text{estimation}}$",       10L,         13L,
  4L,  "Sampling",             "$\\gamma_{\\text{discovery}}$",        11L,         14L,
  5L,  "Sampling",             "$\\gamma_{\\text{switch}}$",           12L,         15L,
  6L,  "Sampling",             "$\\gamma_{\\text{social}}$",           NA_integer_, 16L,
  7L,  "Stopping",             "$\\beta_0$",                           1L,          1L,
  8L,  "Stopping",             "$\\beta_{\\text{step}}$",              2L,          2L,
  9L,  "Stopping",             "$\\beta_{\\text{trial}}$",             3L,          3L,
  10L, "Stopping",             "$\\beta_{\\text{outcome}}$",           4L,          4L,
  11L, "Stopping",             "$\\beta_{\\text{extremity}}$",         5L,          5L,
  12L, "Stopping",             "$\\beta_{\\text{two-outcomes}}$",      6L,          6L,
  13L, "Stopping",             "$\\beta_{\\text{personal}}$",          7L,          7L,
  14L, "Stopping",             "$\\beta_{\\text{social}}$",            NA_integer_, 8L,
  15L, "Stopping",             "$\\beta_{\\text{social-presence}}$",   NA_integer_, 9L,
  16L, "Stopping",             "$\\beta_{\\text{alignment}}$",         NA_integer_, 10L,
  17L, "Consequential choice", "$\\tau$",                              13L,         17L,
  18L, "Consequential choice", "$\\eta_0$",                            NA_integer_, 18L,
  19L, "Consequential choice", "$\\eta_{\\text{personal}}$",           NA_integer_, 19L,
  20L, "Consequential choice", "$\\eta_{\\text{social}}$",             NA_integer_, 20L,
  21L, "Consequential choice", "$\\theta$",                            NA_integer_, 21L
)

# Rows per stage block (each parameter contributes a Mean and an SD row), used
# to place the rules that separate the blocks in the rendered table.
stage_levels <- c("Sampling", "Stopping", "Consequential choice")
stage_rows <- param_map |>
  mutate(stage = factor(stage, levels = stage_levels)) |>
  count(stage, name = "n_param") |>
  arrange(stage) |>
  pull(n_param) * 2L

# The stage blocks are separated by a rule and carry no heading row: split the
# rendered table into lines, locate the body (between the header \midrule and
# \bottomrule), and insert a \midrule after the last row of each block but the
# last. kbl() writes exactly one line per table row, so the block sizes index
# the body lines directly.
insert_stage_rules <- function(tex, block_sizes) {
  lines <- strsplit(tex, "\n", fixed = TRUE)[[1]]
  body_start <- max(which(lines == "\\midrule")) + 1L
  body_end   <- which(lines == "\\bottomrule") - 1L
  stopifnot(body_end - body_start + 1L == sum(block_sizes))
  cuts <- head(body_start + cumsum(block_sizes) - 1L, -1L)
  for (pos in rev(cuts)) lines <- append(lines, "\\midrule", after = pos)
  paste(lines, collapse = "\n")
}

# Posterior median + 95% HPDI + Rhat + bulk ESS for every mu/sigma row of a fit.
summarise_model <- function(fit) {
  diag <- fit$draws(variables = c("mu", "sigma")) |>
    summarise_draws(rhat, ess_bulk) |>
    mutate(
      hyper = str_extract(variable, "^[^\\[]+"),
      i     = as.integer(str_extract(variable, "(?<=\\[)\\d+"))
    ) |>
    select(hyper, i, rhat, ess_bulk)
  fit |>
    spread_draws(mu[i], sigma[i]) |>
    pivot_longer(c(mu, sigma), names_to = "hyper", values_to = "value") |>
    group_by(hyper, i) |>
    median_hdci(value, .width = 0.95) |>
    left_join(diag, by = c("hyper", "i")) |>
    ungroup() |>
    transmute(hyper, i, median = value, lower = .lower, upper = .upper,
      rhat = rhat, ess = ess_bulk)
}

# Read, summarise, and release each (large) fit before loading the next.
fit_solo     <- readRDS(here("output/fit/fit_solo.rds"))
solo_summary <- summarise_model(fit_solo)
rm(fit_solo)
invisible(gc())

fit_group_full <- readRDS(here("output/fit/fit_group_full.rds"))
group_summary  <- summarise_model(fit_group_full)
rm(fit_group_full)
invisible(gc())

# Coefficient cells (median, HPDI) to 2 decimals. Shade by the 95% HPDI of each
# population mean (mu): light orange if the interval is entirely above 0, light
# blue if entirely below 0, no shading if it spans 0. Fills are 20% tints (over
# white) of orange #E69F00 and blue #0072B2; SD (sigma) rows are left unshaded.
# Requires \usepackage[table]{xcolor}.
pos_fill <- "FAECCC"  # light orange (HPDI entirely positive)
neg_fill <- "CCE3F0"  # light blue   (HPDI entirely negative)

# Per-condition fill colour (HTML hex) from the HPDI (NA = no shading).
band_colour <- function(type, lower, upper) {
  case_when(
    type != "Mean"              ~ NA_character_,
    is.na(lower) | is.na(upper) ~ NA_character_,
    lower > 0                   ~ pos_fill,
    upper < 0                   ~ neg_fill,
    TRUE                        ~ NA_character_
  )
}

# Format a coefficient cell, prefixing \cellcolor when a colour is supplied.
shade <- function(x, colour) {
  out <- if_else(is.na(x), "---", sprintf("%.2f", x))
  if_else(
    is.na(x) | is.na(colour), out,
    paste0("\\cellcolor[HTML]{", colour, "}", out)
  )
}

table_models <- param_map |>
  crossing(hyper = c("mu", "sigma")) |>
  mutate(Type = if_else(hyper == "mu", "Mean", "SD")) |>
  left_join(
    solo_summary |>
      rename_with(~ paste0("solo_", .x), c(median, lower, upper, rhat, ess)),
    by = c("i_solo" = "i", "hyper")
  ) |>
  left_join(
    group_summary |>
      rename_with(~ paste0("grp_", .x), c(median, lower, upper, rhat, ess)),
    by = c("i_group" = "i", "hyper")
  ) |>
  arrange(order, Type) |>
  mutate(
    col_s = band_colour(Type, solo_lower, solo_upper),
    col_g = band_colour(Type, grp_lower,  grp_upper)
  ) |>
  transmute(
    Parameter = if_else(Type == "Mean", param, ""),
    Type      = Type,
    s_med = shade(solo_median, col_s),
    s_lo  = shade(solo_lower, col_s),
    s_up  = shade(solo_upper, col_s),
    g_med = shade(grp_median, col_g),
    g_lo  = shade(grp_lower, col_g),
    g_up  = shade(grp_upper, col_g)
  )

tex_models <- table_models |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "l", rep("r", 6)),
    linesep   = "",
    col.names = c("Parameter", "Type",
      "Median", "Lower", "Upper",
      "Median", "Lower", "Upper"),
    caption   = paste(
      "Posterior summary (median and 95\\% HPDI) for the solo- and",
      "group-condition computational models."
    ),
    label     = "summary-models"
  ) |>
  kable_styling(
    latex_options = c("hold_position", "scale_down"), font_size = 10
  ) |>
  add_header_above(c(" " = 2, " " = 1, "95% HPDI" = 2,
    " " = 1, "95% HPDI" = 2)) |>
  add_header_above(c(" " = 2, "Solo" = 3, "Group" = 3)) |>
  footnote(
    general           = "NOTEPLACEHOLDER",
    general_title     = "Note.",
    threeparttable    = TRUE,
    escape            = FALSE,
    footnote_as_chunk = TRUE
  )

# kableExtra's footnote() strips backslashes and percent signs from its text
# even with escape = FALSE, so the LaTeX note (math symbols, \%) is injected
# via a placeholder after rendering.
note_latex <- paste(
  "Parameters specific to the group model (the social stopping terms,",
  "$\\gamma_{\\text{social}}$, and the copy-stage $\\eta$ and $\\theta$)",
  "have no",
  "solo-condition counterpart and are marked ---.",
  "Mean cells are shaded when the 95\\% HPDI excludes zero: light orange if it",
  "lies entirely above zero, light blue if entirely below.",
  "HPDI = highest posterior density interval."
)
tex_models <- sub("NOTEPLACEHOLDER", note_latex, tex_models, fixed = TRUE)
# Reduce the indentation of the table note: flushleft drops the tablenotes left
# margin so the note (and its wrapped lines) sit flush under the table.
tex_models <- sub(
  "\\begin{tablenotes}[para]", "\\begin{tablenotes}[para,flushleft]",
  tex_models,
  fixed = TRUE
)
tex_models <- force_htbp(tex_models)
tex_models <- insert_stage_rules(tex_models, stage_rows)

writeLines(tex_models, here("output/table/summary-models.tex"))

# Coefficient tables for brms regressions --------------------------------------
# Population-level (fixed) effects with posterior median and 95% HPDI.

fit_choice_solo <- readRDS(here("output/fit/fit_choice_solo_brms.rds"))
fit_n_samples   <- readRDS(here("output/fit/fit_n_samples_brms.rds"))

# Build a median + 95% HPDI table of population-level effects for a brms fit.
brms_coef_table <- function(fit, labels) {
  fit |>
    gather_draws(`b_.*`, regex = TRUE) |>
    median_hdci(.value, .width = 0.95) |>
    mutate(term = str_remove(.variable, "^b_")) |>
    right_join(labels, by = "term") |>
    arrange(order) |>
    transmute(
      Parameter = label,
      Median    = sprintf("%.3f", .value),
      Lower     = sprintf("%.3f", .lower),
      Upper     = sprintf("%.3f", .upper)
    )
}

# Render a coefficient table as LaTeX.
brms_coef_tex <- function(tbl, caption, label) {
  tbl |>
    kbl(
      format   = "latex", booktabs = TRUE, escape = FALSE,
      align    = c("l", "r", "r", "r"),
      linesep  = "",
      caption  = caption, label = label
    ) |>
    kable_styling(latex_options = c("hold_position")) |>
    add_header_above(c(" " = 2, "95% HPDI" = 2)) |>
    force_htbp()
}

# fit_choice_solo_brms: mixed logistic regression (log-odds scale)
labels_choice_solo <- tibble(
  order = 1:8,
  term  = c(
    "Intercept", "n_samples", "rare_outcomesmall", "trial_typerisky_risky",
    "n_samples:rare_outcomesmall", "n_samples:trial_typerisky_risky",
    "rare_outcomesmall:trial_typerisky_risky",
    "n_samples:rare_outcomesmall:trial_typerisky_risky"
  ),
  label = c(
    "Intercept",
    "Sample size",
    "Rare outcome (Low)",
    "Trial (Risky/Risky)",
    "Sample size $\\times$ Rare outcome (Low)",
    "Sample size $\\times$ Trial (Risky/Risky)",
    "Rare outcome (Low) $\\times$ Trial (Risky/Risky)",
    "Sample size $\\times$ Rare outcome (Low) $\\times$ Trial (Risky/Risky)"
  )
)

tex_coef_choice_solo <- brms_coef_table(fit_choice_solo, labels_choice_solo) |>
  brms_coef_tex(
    caption = paste(
      "Population-level effects of the Bayesian mixed logistic regression",
      "predicting consequential choices from sample size, rare-outcome",
      "magnitude, and trial type and their interactions in the solo condition",
      "(log-odds scale)."
    ),
    label = "coef-solo-choice"
  )

writeLines(tex_coef_choice_solo, here("output/table/coef-solo-choice.tex"))

# fit_n_samples_brms: mixed negative binomial regression (log scale)
labels_n_samples <- tibble(
  order = 1:8,
  term  = c(
    "Intercept", "conditiongroup", "trial_typerisky_risky", "rare_outcomesmall",
    "conditiongroup:trial_typerisky_risky", "conditiongroup:rare_outcomesmall",
    "trial_typerisky_risky:rare_outcomesmall",
    "conditiongroup:trial_typerisky_risky:rare_outcomesmall"
  ),
  label = c(
    "Intercept",
    "Condition (Group)",
    "Trial (Risky/Risky)",
    "Rare outcome (Low)",
    "Condition (Group) $\\times$ Trial (Risky/Risky)",
    "Condition (Group) $\\times$ Rare outcome (Low)",
    "Trial (Risky/Risky) $\\times$ Rare outcome (Low)",
    paste(
      "Condition (Group) $\\times$ Trial (Risky/Risky) $\\times$",
      "Rare outcome (Low)"
    )
  )
)

tex_coef_n_samples <- brms_coef_table(fit_n_samples, labels_n_samples) |>
  brms_coef_tex(
    caption = paste(
      "Population-level effects of the Bayesian mixed negative-binomial",
      "regression predicting the number of samples drawn from condition, trial",
      "type, and rare-outcome magnitude and their interactions (log scale)."
    ),
    label = "coef-n-samples"
  )

writeLines(tex_coef_n_samples, here("output/table/coef-n-samples.tex"))

# fit_rare_experience_brms: mixed logistic regression (log-odds scale) --------
# Probability of experiencing at least one rare outcome, by condition.
fit_rare_experience <- readRDS(here("output/fit/fit_rare_experience_brms.rds"))

labels_rare_experience <- tibble(
  order = 1:2,
  term  = c("Intercept", "conditiongroup"),
  label = c("Intercept (Solo)", "Condition (Group)")
)

tex_coef_rare_experience <-
  brms_coef_table(fit_rare_experience, labels_rare_experience) |>
  brms_coef_tex(
    caption = paste(
      "Population-level effects of the Bayesian mixed logistic regression",
      "predicting whether participants experienced at least one rare outcome",
      "from condition (log-odds scale). Solo is the reference level."
    ),
    label = "coef-rare-experience"
  )

writeLines(
  tex_coef_rare_experience, here("output/table/coef-rare-experience.tex")
)
