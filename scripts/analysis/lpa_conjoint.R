library(dplyr)
library(tidyr)
library(readr)
library(here)
library(mclust)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "lpa_models.R"))

if (exists("snakemake")) {
  conjoint_file <- snakemake@input[["conjoint"]]
  ic_out        <- snakemake@output[["ic"]]
  profiles_out  <- snakemake@output[["profiles"]]
  means_out     <- snakemake@output[["means"]]
} else {
  conjoint_file <- here("data", "conjoint_long.csv")
  ic_out        <- here("data", "lpa_conjoint_ic.csv")
  profiles_out  <- here("data", "lpa_conjoint_profiles.csv")
  means_out     <- here("data", "lpa_conjoint_means.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

# Per-person preference scores for every attribute level (see
# scripts/shared/lpa_models.R)
scores <- lpa_preference_scores(conjoint)
X <- lpa_matrix(scores$wide)

message("Fitting LPA models for G = 1 to 8...")
t0 <- Sys.time()
lpa_ic <- lpa_information_criteria(X, max_g = 8)
write_csv(lpa_ic, ic_out)
message("IC table written [", round(difftime(Sys.time(), t0, units = "secs")), "s]")

message("Extracting G = 3 profile solution...")
fit3 <- lpa_fit_profiles(X, scores$wide$id, G = 3)
write_csv(fit3$profiles, profiles_out)

profile_means <- lpa_profile_means(scores$wide, fit3$model$classification, scores$map)
write_csv(profile_means, means_out)
message("Profile solution written [", round(difftime(Sys.time(), t0, units = "secs")), "s total]")
