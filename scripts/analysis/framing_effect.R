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

country_levels <- c(
  "Australia", "Brazil", "Germany", "Kenya", "UAE", "Vietnam"
)

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

ts_msg("Choice model done (", elapsed(t0), "s). Computing framing-effect contrasts...")

# Each contrast is the net-zero-framing minus no-information shift in choice
# probability for one attribute level, within one country.
framing_effect <- lapply(seq_along(attributes), function(i) {
  attr <- attributes[[i]]
  message(
    "  [", i, "/", length(attributes),
    "] contrast for ", attr, " (", elapsed(t0), "s elapsed)"
  )
  emm <- emmeans(choice_model, as.formula(paste0("~ framing | ", attr, " * country")))
  regrid(emm, transform = "response") |>
    contrast(method = "revpairwise", by = c(attr, "country")) |>
    confint() |>
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
  select(country, attribute, code, estimate, SE, df, asymp.LCL, asymp.UCL)

write_csv(framing_effect, choice_out)
ts_msg("Framing-effect contrasts written (", elapsed(t0), "s total).")

message("Framing-effect analysis completed:")
message("- Choice results: ", choice_out)
