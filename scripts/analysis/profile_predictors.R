library(dplyr)
library(tidyr)
library(nnet)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "profile_models.R"))

if (exists("snakemake")) {
  profiles_file   <- snakemake@input[["profiles"]]
  groups_file     <- snakemake@input[["groups"]]
  covariates_file <- snakemake@input[["covariates"]]
  raw_file        <- snakemake@input[["raw"]]
  results_out     <- snakemake@output[["results"]]
  ame_out         <- snakemake@output[["ame"]]
} else {
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  raw_file        <- here("raw-data", "raw_data.csv")
  results_out     <- here("data", "profile_predictors.csv")
  ame_out         <- here("data", "profile_predictors_ame.csv")
}

df <- read_profile_data(profiles_file, groups_file, covariates_file, raw_file)

message("Respondents in model: ", nrow(df))
message("Profile distribution:")
print(table(df$profile))

formula <- profile_formula

tidy_multinom <- function(m, ref) {
  s  <- summary(m)
  z  <- s$coefficients / s$standard.errors
  p  <- 2 * (1 - pnorm(abs(z)))
  lapply(seq_len(nrow(s$coefficients)), function(i) {
    tibble(
      comparison = paste(rownames(s$coefficients)[i], "vs", ref),
      term       = colnames(s$coefficients),
      estimate   = s$coefficients[i, ],
      std_error  = s$standard.errors[i, ],
      z_value    = z[i, ],
      p_value    = p[i, ]
    )
  }) |>
    bind_rows() |>
    mutate(
      or    = exp(estimate),
      lower = exp(estimate - 1.96 * std_error),
      upper = exp(estimate + 1.96 * std_error)
    ) |>
    filter(term != "(Intercept)")
}

set.seed(random_seed)
m1 <- multinom(formula, data = mutate(df, profile = relevel(profile, ref = "Profile 1")),
               trace = FALSE)
set.seed(random_seed)
m2 <- multinom(formula, data = mutate(df, profile = relevel(profile, ref = "Profile 2")),
               trace = FALSE)

results <- bind_rows(
  tidy_multinom(m1, "Profile 1"),
  tidy_multinom(m2, "Profile 2") |> filter(comparison == "Profile 3 vs Profile 2")
)

write_csv(results, results_out)
message("Profile predictor results written: ", nrow(results), " rows, ",
        n_distinct(results$comparison), " comparisons")

# ── Average marginal effects on profile membership probability ───────────────
# (see profile_ame() in scripts/shared/profile_models.R)

# 10,000 draws so borderline intervals are stable across runs and model edits
ame <- profile_ame(formula, df, variables = profile_ame_variables, n_draws = 10000)

write_csv(ame, ame_out)
message("Profile membership AMEs written: ", ame_out, " (",
        n_distinct(paste(ame$variable, ame$level)), " predictors)")
