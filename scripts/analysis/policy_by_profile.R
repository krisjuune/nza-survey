library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "conjoint_models.R"))

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
)

profiles <- read_csv(profiles_file, show_col_types = FALSE) |>
  select(id, profile) |>
  mutate(profile = factor(paste("Profile", profile), levels = paste("Profile", 1:3)))

ts_msg("Fitting policy x profile choice model (glmer)...")
t0 <- Sys.time()
choice_emm <- fit_policy_by_profile(conjoint, profiles, log = ts_msg)

write_csv(choice_emm, choice_out)
ts_msg("Policy by profile emmeans written (", elapsed(t0), "s total).")
