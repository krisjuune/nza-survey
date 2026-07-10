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
  covariates_file <- snakemake@input[["covariates"]]
  choice_out      <- snakemake@output[["choice"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  choice_out      <- here("data", "framing_effect_choice.csv")
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
    country = factor(country, levels = country_levels)
  )

elapsed <- function(t0) {
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

ts_msg <- function(...) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
}

# ----------------------------
# Choice model (logit), framing x country fully crossed with every attribute
# so the framing effect on each attribute level can vary by country.
# ----------------------------
ts_msg("Fitting choice model (glmer)...")
t0 <- Sys.time()

choice_model <- suppressWarnings(
  glmer(
    binary_choice ~
      (fuel_code + activity_code + durability_code +
         responsibility_code + cost_code) * framing * country +
      (1 | id),
    data = df,
    family = binomial,
    control = glmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
    )
  )
)

ts_msg("Choice model done (", elapsed(t0), "s). Computing per-framing-arm emmeans...")

# Conditional probability for each framing arm (rather than their contrast),
# so the choice plot can overlay both arms directly - same model, so both
# arms are on a like-for-like footing.
framing_effect <- lapply(seq_along(attributes), function(i) {
  attr <- attributes[[i]]
  message(
    "  [", i, "/", length(attributes),
    "] emmeans for ", attr, " (", elapsed(t0), "s elapsed)"
  )
  emmeans(
    choice_model,
    as.formula(paste0("~ framing | ", attr, " * country")),
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
  select(country, attribute, code, framing, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(framing_effect, choice_out)
ts_msg("Framing-effect emmeans written (", elapsed(t0), "s total).")

message("Framing-effect analysis completed:")
message("- Choice results: ", choice_out)
