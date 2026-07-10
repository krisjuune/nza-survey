library(dplyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  covariates_file <- snakemake@input[["covariates"]]
  choice_out      <- snakemake@output[["choice"]]
  rating_out      <- snakemake@output[["rating"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  choice_out      <- here("data", "durability_cost_choice_emm.csv")
  rating_out      <- here("data", "durability_cost_rating_emm.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  select(id, country)

df <- conjoint |>
  left_join(covariates, by = "id") |>
  mutate(
    country = recode(as.character(country), !!!country_recode),
    country = factor(country, levels = country_levels),
    cost_code = factor(cost_code, levels = c("10", "30", "50"))
  )

elapsed <- function(t0) {
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

ts_msg <- function(...) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
}

# ----------------------------
# 1. CHOICE MODEL (logit)
# ----------------------------
# Other attributes are included as additive controls; the interaction of
# interest (durability x cost x country) is left fully crossed so the
# cost/durability trade-off can vary by country.
ts_msg("Fitting choice model (glmer)...")
t0 <- Sys.time()

choice_model <- suppressWarnings(
  glmer(
    binary_choice ~
      fuel_code + activity_code + responsibility_code +
      durability_code * cost_code * country +
      (1 | id),
    data = df,
    family = binomial,
    control = glmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
    )
  )
)

ts_msg("Choice model done (", elapsed(t0), "s). Computing emmeans...")

choice_emm <- emmeans(
  choice_model,
  ~ durability_code * cost_code | country,
  type = "response"
) |>
  as.data.frame() |>
  select(country, durability_code, cost_code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)
ts_msg("Choice emmeans written (", elapsed(t0), "s total).")

# ----------------------------
# 2. RATING MODEL (linear)
# ----------------------------
ts_msg("Fitting rating model (lmer)...")
t1 <- Sys.time()

rating_model <- suppressWarnings(
  lmer(
    support ~
      fuel_code + activity_code + responsibility_code +
      durability_code * cost_code * country +
      (1 | id),
    data = df,
    control = lmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
    )
  )
)

ts_msg("Rating model done (", elapsed(t1), "s). Computing emmeans...")

rating_emm <- emmeans(
  rating_model,
  ~ durability_code * cost_code | country
) |>
  as.data.frame() |>
  rename_with(
    ~ case_match(
      ., "asymp.LCL" ~ "lower.CL", "asymp.UCL" ~ "upper.CL",
      .default = .
    )
  ) |>
  select(country, durability_code, cost_code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)
ts_msg("Rating emmeans written (", elapsed(t1), "s total).")

message("Durability x cost interaction analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)
