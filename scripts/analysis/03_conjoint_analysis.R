library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

if (exists("snakemake")) {
  input_file   <- snakemake@input[[1]]
  choice_out   <- snakemake@output[["choice"]]
  rating_out   <- snakemake@output[["rating"]]
} else {
  input_file   <- here("data", "conjoint_long.csv")
  choice_out   <- here("data", "overall_choice_emm.csv")
  rating_out   <- here("data", "overall_rating_emm.csv")
}

df <- read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = cols(
    cost_code = col_character()
  )
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
    binary_choice ~ fuel_code + activity_code + durability_code +
      responsibility_code + cost_code +
      (1 | id),
    data = df,
    family = binomial
  )
)

choice_emm <- lapply(attributes, function(attr) {
  emmeans(choice_model, as.formula(paste0("~ ", attr)), type = "response") |>
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
  select(attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)

# ----------------------------
# 2. RATING MODEL (linear)
# ----------------------------
rating_model <- suppressWarnings(
  lmer(
    support ~ fuel_code + activity_code + durability_code +
      responsibility_code + cost_code +
      (1 | id),
    data = df
  )
)

rating_emm <- lapply(attributes, function(attr) {
  emmeans(rating_model, as.formula(paste0("~ ", attr))) |>
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
  select(attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)

message("Conjoint analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)