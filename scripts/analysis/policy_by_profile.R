library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file <- snakemake@input[["conjoint"]]
  profiles_file <- snakemake@input[["profiles"]]
  choice_out    <- snakemake@output[["choice"]]
} else {
  conjoint_file <- here("data", "conjoint_long.csv")
  profiles_file <- here("data", "lpa_conjoint_profiles.csv")
  choice_out    <- here("data", "policy_by_profile_choice.csv")
}

elapsed <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
ts_msg  <- function(...) message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
) |>
  filter(!is.na(policy_type)) |>
  mutate(policy_type = factor(policy_type, levels = policy_levels))

profiles <- read_csv(profiles_file, show_col_types = FALSE) |>
  select(id, profile) |>
  mutate(profile = factor(paste("Profile", profile), levels = paste("Profile", 1:3)))

df <- inner_join(conjoint, profiles, by = "id")

ts_msg("Policy × profile data: ", nrow(df), " rows, ",
       n_distinct(df$id), " respondents.")
ts_msg("Obs per cell:")
print(count(df, policy_type, profile))

ts_msg("Fitting policy × profile choice model (glmer)...")
t0 <- Sys.time()

choice_model <- suppressWarnings(
  glmer(
    binary_choice ~ policy_type * profile + (1 | id),
    data    = df,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

ts_msg("Model done (", elapsed(t0), "s). Computing emmeans...")

profile_n <- df |>
  distinct(id, profile) |>
  count(profile, name = "n_profile")

choice_emm <- emmeans(
  choice_model,
  ~ policy_type | profile,
  type = "response"
) |>
  as.data.frame() |>
  select(profile, policy_type, prob, SE, df, asymp.LCL, asymp.UCL) |>
  left_join(profile_n, by = "profile")

write_csv(choice_emm, choice_out)
ts_msg("Policy by profile emmeans written (", elapsed(t0), "s total).")
