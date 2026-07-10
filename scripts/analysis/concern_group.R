library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  groups_file     <- snakemake@input[["groups"]]
  covariates_file <- snakemake@input[["covariates"]]
  choice_out      <- snakemake@output[["choice"]]
  rating_out      <- snakemake@output[["rating"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  choice_out      <- here("data", "concern_choice_emm.csv")
  rating_out      <- here("data", "concern_rating_emm.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

groups <- read_csv(groups_file, show_col_types = FALSE) |>
  select(id, concern_group) |>
  filter(!is.na(concern_group)) |>
  mutate(concern_group = factor(concern_group, levels = concern_levels))

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  select(id, country) |>
  mutate(
    country = recode(as.character(country), !!!country_recode),
    country = factor(country, levels = country_levels)
  )

df <- conjoint |>
  inner_join(groups,     by = "id") |>
  inner_join(covariates, by = "id")

attributes <- c(
  "fuel_code",
  "activity_code",
  "durability_code",
  "responsibility_code",
  "cost_code"
)

elapsed <- function(t0) {
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

ts_msg <- function(...) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
}

emm_per_country <- function(model, attr, group_var, t0) {
  emmeans(
    model,
    as.formula(paste0("~ ", attr, " | ", group_var)),
    type = "response"
  ) |>
    as.data.frame() |>
    mutate(attribute = attr)
}

# ----------------------------
# 1. CHOICE MODEL (logit), fit separately per country
# ----------------------------
ts_msg("Fitting choice models (glmer) per country...")
t0 <- Sys.time()

choice_emm <- lapply(country_levels, function(ctry) {
  ts_msg("  Country: ", ctry, " (", elapsed(t0), "s elapsed)")
  df_ctry <- filter(df, country == ctry)

  model <- suppressWarnings(
    glmer(
      binary_choice ~
        (fuel_code + activity_code + durability_code +
           responsibility_code + cost_code) * concern_group +
        (1 | id),
      data    = df_ctry,
      family  = binomial,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  )

  lapply(seq_along(attributes), function(i) {
    attr <- attributes[[i]]
    message(
      "    [", i, "/", length(attributes),
      "] emmeans for ", attr, " (", elapsed(t0), "s elapsed)"
    )
    emm_per_country(model, attr, "concern_group", t0) |>
      mutate(country = ctry)
  }) |> bind_rows()
}) |>
  bind_rows() |>
  pivot_longer(
    cols      = ends_with("_code"),
    names_to  = "tmp",
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  mutate(
    concern_group = factor(concern_group, levels = concern_levels),
    country       = factor(country, levels = country_levels)
  ) |>
  select(country, concern_group, attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)
ts_msg("Choice emmeans written (", elapsed(t0), "s total).")

# ----------------------------
# 2. RATING MODEL (linear), fit separately per country
# ----------------------------
ts_msg("Fitting rating models (lmer) per country...")
t1 <- Sys.time()

rating_emm <- lapply(country_levels, function(ctry) {
  ts_msg("  Country: ", ctry, " (", elapsed(t1), "s elapsed)")
  df_ctry <- filter(df, country == ctry)

  model <- suppressWarnings(
    lmer(
      support ~
        (fuel_code + activity_code + durability_code +
           responsibility_code + cost_code) * concern_group +
        (1 | id),
      data    = df_ctry,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  )

  lapply(seq_along(attributes), function(i) {
    attr <- attributes[[i]]
    message(
      "    [", i, "/", length(attributes),
      "] emmeans for ", attr, " (", elapsed(t1), "s elapsed)"
    )
    emmeans(model, as.formula(paste0("~ ", attr, " | concern_group"))) |>
      as.data.frame() |>
      mutate(attribute = attr, country = ctry)
  }) |> bind_rows()
}) |>
  bind_rows() |>
  pivot_longer(
    cols      = ends_with("_code"),
    names_to  = "tmp",
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  rename_with(
    ~ case_match(
      ., "asymp.LCL" ~ "lower.CL", "asymp.UCL" ~ "upper.CL",
      .default = .
    )
  ) |>
  mutate(
    concern_group = factor(concern_group, levels = concern_levels),
    country       = factor(country, levels = country_levels)
  ) |>
  select(country, concern_group, attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)

message("Concern-group analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)
