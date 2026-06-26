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
    binary_choice ~ fuel_code + activity_code + durability_code +
      responsibility_code + cost_code +
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
    as.formula(paste0("~ ", attr)),
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
  select(attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)
ts_msg("Choice emmeans written (", elapsed(t0), "s total).")

# ----------------------------
# 2. RATING MODEL (linear)
# ----------------------------
ts_msg("Fitting rating model (lmer)...")
t1 <- Sys.time()

rating_model <- suppressWarnings(
  lmer(
    support ~ fuel_code + activity_code + durability_code +
      responsibility_code + cost_code +
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
  rename_with(
    ~ case_match(
      ., "asymp.LCL" ~ "lower.CL", "asymp.UCL" ~ "upper.CL",
      .default = .
    )
  ) |>
  select(attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(rating_emm, rating_out)
ts_msg("Rating emmeans written (", elapsed(t1), "s total).")

message("Conjoint analysis completed:")
message("- Choice results: ", choice_out)
message("- Rating results: ", rating_out)
