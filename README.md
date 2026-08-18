# Social influence and information cascades in decisions from experience

**Author:** Kiri Kuroda

## Table of contents

- [Task implementation and instructions](#task-implementation-and-instructions)
- [Environment setup](#environment-setup)
- [Project structure](#project-structure)
- [Preprocessed data](#preprocessed-data)

## Task implementation and instructions

The task was implemented using [oTree](https://www.otree.org/). The source codes are in the [oTree/](oTree/) directory.

Task instruction slides are available in the [instruction/](instruction/) directory.

---

## Environment setup

### Prerequisites

- R >= 4.5.1
- CmdStan (see below)

### 1. Clone the repository

```bash
git clone <repository-url>
cd social_dfe_project
```

### 2. Restore packages with renv

This project uses [renv](https://rstudio.github.io/renv/) for package management. Opening the project activates renv automatically via `.Rprofile`. On first use, restore all packages recorded in `renv.lock` by running:

```r
renv::restore()
```

### 3. Install CmdStan

The `cmdstanr` package (installed in step 2 via renv) requires a separate local installation of [CmdStan](https://mc-stan.org/users/interfaces/cmdstan). If CmdStan is not yet installed, run:

```r
cmdstanr::install_cmdstan()
```

To verify an existing installation:

```r
cmdstanr::cmdstan_version()
```

### 4. Run the analysis

You can run the scripts in order, from  `analysis/01_descriptive.R` to `analysis/07_ppc.R`.

> **Note on compute time:** The Bayesian parameter estimation in `analysis/02_modeling.R` is computationally intensive and can take several hours to complete. The estimation scripts assume **80 CPU cores** (4 chains × 20 threads per chain).

---

## Project structure

```
social_dfe_project/
├── data/
│   ├── data.rda          # Preprocessed data
│   └── codebook.yaml     # Variable descriptions (sidecar)
├── function/
│   ├── R/                # R functions
│   └── Stan/             # Stan models
├── analysis/
│   ├── 01_descriptive.R
│   ├── 02_modeling.R
│   ├── 03_simulation.R
│   ├── 04_postprocessing.R
│   ├── 05_visualization.R
│   ├── 06_tables.R
│   └── 07_ppc.R
├── output/
│   ├── fit/
│   ├── figure/
│   └── table/
├── instruction/          # Task instruction slides
└── oTree/                # oTree task implementation
```

## Preprocessed data

All preprocessed data are stored in `data/data.rda` and can be loaded with:

```r
load(here::here("data/data.rda"))
```

This loads six data frames: `df_participants`, `df_options`, `df_practice`, `df_trials`, `df_sampling`, and `df_questionnaire`.

Variable descriptions for all data frames are available in the sidecar file `data/codebook.yaml`:

```r
yaml::read_yaml(here::here("data/codebook.yaml"))
```

The variable tables below are excerpted from the codebook. **Only the variables used in the analysis are listed.** The data frames also carry columns that are not analysed — raw Qualtrics metadata, page timing and browser fields, and participant-level columns duplicated by joins.

---

### `df_participants`

Participant-level metadata. Unit of observation: participant.


| Variable               | Type      | Values          | Description                                    |
| ---------------------- | --------- | --------------- | ---------------------------------------------- |
| `subject_id`           | character | —               | Participant identifier                         |
| `condition`            | factor    | `solo`, `group` | Experimental condition                         |
| `session_id`           | character | —               | Session identifier                             |
| `datetime_start`       | datetime  | —               | Session start time                             |
| `datetime_finish`      | datetime  | —               | Session finish time                            |
| `completion_code`      | character | —               | Prolific completion code                       |
| `payoff_base`          | double    | —               | Base payment (GBP)                             |
| `payoff_task`          | double    | —               | Task-based payment (GBP)                       |
| `payoff_inconvenience` | double    | —               | Inconvenience payment (GBP)                    |
| `payoff_bonus`         | double    | —               | Bonus payment (GBP)                            |
| `is_bot`               | logical   | `TRUE`, `FALSE` | Whether flagged as bot by the system           |
| `count_quiz_failure`   | integer   | 0, 1, 2, ...    | Number of comprehension quiz failures          |
| `is_quiz_failed`       | logical   | `TRUE`, `FALSE` | Whether the participant failed the quiz        |
| `is_matching_timeout`  | logical   | `TRUE`, `FALSE` | Whether matching timed out                     |
| `is_dropout_self`      | logical   | `TRUE`, `FALSE` | Whether the participant dropped out themselves |
| `is_dropout_other`     | logical   | `TRUE`, `FALSE` | Whether dropped out due to another participant |


---

### `df_options`

Option definitions used in the task. Unit of observation: option.


| Variable       | Type      | Values                                         | Description                                                                                             |
| -------------- | --------- | ---------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| `option_id`    | integer   | 1–40                                           | Option identifier                                                                                       |
| `v_a_high`     | double    | in points                                      | High outcome of option A                                                                                |
| `v_a_low`      | double    | in points                                      | Low outcome of option A                                                                                 |
| `p_a_high`     | double    | 0.1, 0.5, 0.9                                  | Probability of high outcome of option A (0.5 for attention check trials)                                |
| `p_a_low`      | double    | 0.1, 0.5, 0.9                                  | Probability of low outcome of option A (0.5 for attention check trials)                                 |
| `v_b_high`     | double    | in points                                      | High outcome of option B                                                                                |
| `v_b_low`      | double    | in points                                      | Low outcome of option B                                                                                 |
| `p_b_high`     | double    | 0.1, 0.5, 1.0                                  | Probability of high outcome of option B (1.0 for risky_safe; 0.5 for attention check)                   |
| `p_b_low`      | double    | 0.0, 0.5, 0.9                                  | Probability of low outcome of option B (0.0 for risky_safe; 0.5 for attention check)                    |
| `trial_type`   | character | `risky_risky`, `risky_safe`, `attention_check` | Trial type                                                                                              |
| `rare_outcome` | character | `large`, `small`, `NA`                         | Whether the rare outcome is the high (`large`) or low (`small`) payoff; `NA` for attention check trials |


---

### `df_trials`

Trial-level choice data (excluding attention check trials). Unit of observation: participant × trial.


| Variable             | Type      | Values                      | Description                                                                         |
| -------------------- | --------- | --------------------------- | ----------------------------------------------------------------------------------- |
| `subject_id`         | character | —                           | Participant identifier                                                              |
| `condition`          | factor    | `solo`, `group`             | Experimental condition                                                              |
| `session_id`         | character | —                           | Session identifier                                                                  |
| `group_id`           | character | —                           | Group identifier (interaction of session and raw group ID)                          |
| `player_id`          | integer   | —                           | Player slot assigned at session start (not used in analysis)                        |
| `trial_number`       | integer   | 1–40                        | Trial number as presented; gaps where attention checks were dropped                 |
| `option_id`          | integer   | 1–36                        | Option identifier (links to `df_options`)                                           |
| `trial_type`         | factor    | `risky_safe`, `risky_risky` | Trial type (factor levels: `risky_safe`, `risky_risky`)                             |
| `rare_outcome`       | character | `large`, `small`            | Whether the rare outcome is large or small                                          |
| `is_real_round`      | logical   | `TRUE`, `FALSE`             | Whether this is a real (paid) trial                                                 |
| `position`           | integer   | 0, 1                        | Display position of option A (0 = A on right, 1 = A on left)                        |
| `choice_self`        | integer   | 0, 1                        | Own choice (0 = A, 1 = B)                                                           |
| `rt_self`            | double    | —                           | Reaction time for own choice (ms)                                                   |
| `n_samples_self`     | integer   | 2–40                        | Number of samples drawn by the participant                                          |
| `choice_other`       | character | list string, NA             | Choices of all group members (JSON-like list; group condition only)                 |
| `rt_other`           | character | list string, NA             | Reaction times of all group members (ms; JSON-like list; group condition only)      |
| `n_samples_other`    | character | list string, NA             | Number of samples drawn by all group members (JSON-like list; group condition only) |
| `is_round_completed` | logical   | `TRUE`, `FALSE`             | Whether the round was completed                                                     |
| `payoff_round`       | double    | —                           | Payoff earned in this trial                                                         |


---

### `df_sampling`

Sample-level data from the information sampling phase. Unit of observation: participant × trial × sample.


| Variable               | Type      | Values          | Description                                    |
| ---------------------- | --------- | --------------- | ---------------------------------------------- |
| `subject_id`           | character | —               | Participant identifier                         |
| `condition`            | factor    | `solo`, `group` | Experimental condition                         |
| `trial_number`         | integer   | 1–40            | Trial number within a session                  |
| `option_id`            | integer   | 1–36            | Option identifier (links to `df_options`)      |
| `sample_id`            | integer   | 1, 2, 3, ...    | Sample index within a trial                    |
| `sampled_option_label` | character | `a`, `b`        | Which option was sampled                       |
| `sampled_option`       | integer   | 0, 1            | Which option was sampled (0 = A, 1 = B)        |
| `sampled_outcome`      | double    | in points       | Outcome observed in this sample                |
| `sequence_a`           | double    | in points       | Outcome option A would have shown at this draw |
| `sequence_b`           | double    | in points       | Outcome option B would have shown at this draw |
| `rt_sampling`          | double    | —               | Reaction time for this sample (ms)             |


---

### `df_questionnaire`

Post-task questionnaire responses. Unit of observation: participant.


| Variable              | Description                                                             |
| --------------------- | ----------------------------------------------------------------------- |
| `subject_id`          | Participant identifier                                                  |
| `condition`           | Experimental condition (`solo`, `group`), joined from `df_participants` |
| `age`                 | Age (19–45)                                                             |
| `gender`              | Gender (`man`, `woman`, `nonbinary`, `noresponse`)                      |
| `social_info_use`     | Self-reported use of social information (1–6; group only)               |
| `choose_after_others` | Tendency to choose after observing others (1–7; group only)             |
| `delay_choice`        | Tendency to delay choice (1–7; group only)                              |
| `choice_timing`       | Self-reported choice timing (1–7; group only)                           |
| `suspicion_others`    | Suspicion that other participants are bots (1–7; group only)            |
| `social_DO`           | Qualtrics display order of the five social items (group only)           |
| `strategy`            | Open-ended decision strategy (also carries the bot honeypot)            |
| `comment`             | Open-ended comment (also carries the bot honeypot)                      |


