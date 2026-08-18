# Shared utility functions

library(tidyverse)

# Convert Python-style list strings (e.g., "[1, 2, 3]") to R vectors
parse_list_string <- function(x) {
  if (is.na(x) || is.null(x)) return(NA)
  x_clean <- gsub("'", '"', x)
  result <- tryCatch(
    {
      jsonlite::fromJSON(x_clean)
    },
    error = function(e) {
      NA
    })
  result
}

# Read multiple CSVs and row-bind them into a single data frame
read_and_bind <- function(paths, ...) {
  map(paths, \(x) read_csv(x, ...)) |>
    list_rbind()
}

# Sequential option number used in the choice-problem table and in the
# per-option figure panels. Ordering: risky_safe -> risky_risky -> attention
# check; within each, large -> small rare outcome, then option_id. Main trials
# get 1-36 and attention checks 37-40. Single source of truth so that the table
# and the figures refer to the same problem by the same number.
add_option_no <- function(options) {
  options |>
    mutate(
      .tt_ord = case_when(trial_type == "risky_safe"  ~ 1L,
        trial_type == "risky_risky" ~ 2L,
        TRUE                        ~ 3L),
      .ro_ord = case_when(rare_outcome == "large" ~ 1L,
        rare_outcome == "small" ~ 2L,
        TRUE                    ~ 3L)
    ) |>
    arrange(.tt_ord, .ro_ord, option_id) |>
    mutate(option_no = row_number()) |>
    select(-.tt_ord, -.ro_ord)
}

# Build a vector of CSV paths for a given export type (e.g., "task",
# "redirect"). Reads the globals `sessions` (one export directory per data
# collection session) and `dates` (the matching collection date), which are
# defined in analysis/00_preprocessing.R; this function is unusable without
# them.
make_paths <- function(type) {
  file.path(sessions, paste0("social_gambling_", type, "_", dates, ".csv"))
}

# Replay the Stan models' MLE belief update on a trial's sampling sequence and
# return delta_t at the stopping moment. `seg` must have columns
# sampled_option (0/1), sampled_outcome (numeric), is_higher_outcome (0/1).
# Two uses: in 02_modeling.R it supplies k = median(|delta_t|), pooled over the
# solo and group conditions, for the evidence-strength terms |u_t| =
# |tanh(delta_t / k)| shared by all Stan models; in 05_visualization.R it
# rebuilds the observed delta_t for the figures.
compute_delta_t_at_stop <- function(seg) {
  n_A_high <- n_A_low <- n_B_high <- n_B_low <- 0
  obs_A_high <- obs_A_low <- obs_B_high <- obs_B_low <- -1
  val_A_high <- val_A_low <- val_B_high <- val_B_low <- 0

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
  }

  n_A <- n_A_high + n_A_low
  n_B <- n_B_high + n_B_low
  p_high_A <- if (n_A > 0) n_A_high / n_A else 0.5
  p_high_B <- if (n_B > 0) n_B_high / n_B else 0.5
  ev_A <- p_high_A * val_A_high + (1 - p_high_A) * val_A_low
  ev_B <- p_high_B * val_B_high + (1 - p_high_B) * val_B_low
  var_A <- if (obs_A_high < 0 || obs_A_low < 0) {
    0.01
  } else {
    p_high_A * (ev_A - val_A_high)^2 +
      (1 - p_high_A) * (ev_A - val_A_low)^2
  }
  var_B <- if (obs_B_high < 0 || obs_B_low < 0) {
    0.01
  } else {
    p_high_B * (ev_B - val_B_high)^2 +
      (1 - p_high_B) * (ev_B - val_B_low)^2
  }
  se <- if (n_A == 0) {
    sqrt(var_B / n_B)
  } else if (n_B == 0) {
    sqrt(var_A / n_A)
  } else {
    sqrt(var_A / n_A + var_B / n_B)
  }
  (ev_B - ev_A) / se
}
