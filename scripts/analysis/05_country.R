library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  covariates_file <- snakemake@input[["covariates"]]
  choice_out      <- snakemake@output[["choice"]]
  rating_out      <- snakemake@output[["rating"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  choice_out      <- here("data", "country_choice_emm.csv")
  rating_out      <- here("data", "country_rating_emm.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  select(id, country)

country_levels <- c("Australia", "Brazil", "Germany", "Kenya", "Vietnam", "UAE")

df <- conjoint |>
  left_join(covariates, by = "id") |>
  mutate(
    country = recode(as.character(country),
      "1" = "Australia",
      "2" = "Brazil",
      "3" = "Germany",
      "4" = "Kenya",
      "5" = "Vietnam",
      "6" = "UAE"
    ),
    country = factor(country, levels = country_levels)
  )

attributes <- c(
  "fuel_code",
  "activity_code",
  "durability_code",
  "responsibility_code",
  "cost_code"
)

# ----------------------------
# 1. CHOICE MODEL (logit)
# ----------------------------
choice_model <- suppressWarnings(
  glmer(
    binary_choice ~
      (fuel_code + activity_code + durability_code +
         responsibility_code + cost_code) * country +
      (1 | id),
    data = df,
    family = binomial
  )
)

choice_emm <- lapply(attributes, function(attr) {
  emmeans(choice_model, as.formula(paste0("~ ", attr, " | country")), type = "response") |>
    as.data.frame() |>
    mutate(attribute = attr)
}) |>
  bind_rows() |>
  pivot_longer(
    cols = ends_with("_code"),
    names_to = "tmp",
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  select(country, attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)

# ----------------------------
# 2. RATING MODEL (linear)
# ----------------------------
rating_model <- suppressWarnings(
  lmer(
    support ~
      (fuel_code + activity_code + durability_code +
         responsibility_code + cost_code) * country +
      (1 | id),
    data = df
  )
)

rating_emm <- lapply(attributes, function(attr) {
  emmeans(rating_model, as.formula(paste0("~ ", attr, " | country"))) |>
    as.data.frame() |>
    mutate(attribute = attr)
}) |>
  bind_rows() |>
  pivot_longer(
    cols = ends_with("_code"),
    names_to = "tmp",
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  select(country, attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)

message("Country subgroup analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)
