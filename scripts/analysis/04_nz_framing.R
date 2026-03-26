library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

if (exists("snakemake")) {
  input_file  <- snakemake@input[[1]]
  framing_choice_out   <- snakemake@output[["framing_choice"]]
  framing_rating_out   <- snakemake@output[["framing_rating"]]
  nz_choice_out   <- snakemake@output[["nz_choice"]]
  nz_rating_out   <- snakemake@output[["nz_rating"]]
} else {
  input_file  <- here("data", "conjoint_long.csv")
  framing_choice_out   <- here("data", "framing_choice_emm.csv")
  framing_rating_out   <- here("data", "framing_rating_emm.csv")
  nz_choice_out   <- here("data", "nz_choice_emm.csv")
  nz_rating_out   <- here("data", "nz_rating_emm.csv")
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
    binary_choice ~ 
      (fuel_code + activity_code + durability_code +
       responsibility_code + cost_code) * framing +
      nz_binary * framing +
      (1 | id),
    data = df,
    family = binomial
  )
)

choice_emm <- lapply(attributes, function(attr) {
  emmeans(choice_model, as.formula(paste0("~ ", attr, " | framing")), type = "response") |>
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
  select(framing, attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

nz_choice_emm <- emmeans(
  choice_model,
  ~ nz_binary | framing,
  type = "response"
) |>
  as.data.frame() |>
  select(framing, nz_binary, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, framing_choice_out)
write_csv(nz_choice_emm, nz_choice_out)

# ----------------------------
# 2. RATING MODEL (linear)
# ----------------------------
rating_model <- suppressWarnings(
  lmer(
    support ~ 
      (fuel_code + activity_code + durability_code +
       responsibility_code + cost_code) * framing +
      nz_binary * framing +
      (1 | id),
    data = df
  )
)

rating_emm <- lapply(attributes, function(attr) {
  emmeans(rating_model, as.formula(paste0("~ ", attr, " | framing"))) |>
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
  select(framing, attribute, code, emmean, SE, df, lower.CL, upper.CL)

nz_rating_emm <- emmeans(
  rating_model,
  ~ nz_binary | framing
) |>
  as.data.frame() |>
  select(framing, nz_binary, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, framing_rating_out)
write_csv(nz_rating_emm, nz_rating_out)