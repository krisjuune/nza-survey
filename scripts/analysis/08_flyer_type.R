library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

if (exists("snakemake")) {
  conjoint_file <- snakemake@input[["conjoint"]]
  groups_file   <- snakemake@input[["groups"]]
  choice_out    <- snakemake@output[["choice"]]
  rating_out    <- snakemake@output[["rating"]]
} else {
  conjoint_file <- here("data", "conjoint_long.csv")
  groups_file   <- here("data", "respondent_groups.csv")
  choice_out    <- here("data", "flyer_type_choice_emm.csv")
  rating_out    <- here("data", "flyer_type_rating_emm.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

flyer_levels <- c(
  "Never-flyer", "Infrequent/lapsed flyer", "Occasional flyer", "Frequent flyer"
)

# 1 respondent has a missing flyer_type and is dropped here only (kept in
# every other analysis that doesn't condition on flyer_type).
groups <- read_csv(groups_file, show_col_types = FALSE) |>
  select(id, flyer_type) |>
  filter(!is.na(flyer_type)) |>
  mutate(flyer_type = factor(flyer_type, levels = flyer_levels))

df <- conjoint |>
  inner_join(groups, by = "id")

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

# ----------------------------
# 1. CHOICE MODEL (logit)
# ----------------------------
ts_msg("Fitting choice model (glmer)...")
t0 <- Sys.time()

choice_model <- suppressWarnings(
  glmer(
    binary_choice ~
      (fuel_code + activity_code + durability_code +
         responsibility_code + cost_code) * flyer_type +
      (1 | id),
    data = df,
    family = binomial,
    control = glmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
    )
  )
)

ts_msg("Choice model done (", elapsed(t0), "s). Computing emmeans...")

choice_emm <- lapply(seq_along(attributes), function(i) {
  attr <- attributes[[i]]
  message(
    "  [", i, "/", length(attributes),
    "] emmeans for ", attr, " (", elapsed(t0), "s elapsed)"
  )
  emmeans(
    choice_model,
    as.formula(paste0("~ ", attr, " | flyer_type")),
    type = "response"
  ) |>
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
  select(flyer_type, attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

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
      (fuel_code + activity_code + durability_code +
         responsibility_code + cost_code) * flyer_type +
      (1 | id),
    data = df,
    control = lmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
    )
  )
)

ts_msg("Rating model done (", elapsed(t1), "s). Computing emmeans...")

rating_emm <- lapply(seq_along(attributes), function(i) {
  attr <- attributes[[i]]
  message(
    "  [", i, "/", length(attributes),
    "] emmeans for ", attr, " (", elapsed(t1), "s elapsed)"
  )
  emmeans(
    rating_model,
    as.formula(paste0("~ ", attr, " | flyer_type"))
  ) |>
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
  rename_with(
    ~ case_match(
      ., "asymp.LCL" ~ "lower.CL", "asymp.UCL" ~ "upper.CL",
      .default = .
    )
  ) |>
  select(flyer_type, attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)

message("Flyer-type subgroup analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)
