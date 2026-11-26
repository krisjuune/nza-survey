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
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  choice_out      <- here("data", "activity_cost_choice_emm.csv")
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
    cost_code = factor(cost_code, levels = c("10", "30", "50")),
    # Engineered capture (direct air or point-source capture) vs traditional
    # or nature-based offsets - a binary cut of the 4-level activity
    # attribute, matching the activity premium in the WTP analysis.
    activity_binary = case_when(
      activity_code %in% c("direct_air", "factory_ccs") ~ "Capture",
      activity_code %in% c("cookstoves", "trees")       ~ "Offsets",
      TRUE ~ NA_character_
    ),
    activity_binary = factor(activity_binary, levels = c("Offsets", "Capture"))
  )

ts_msg <- function(...) message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)

# Choice model: other attributes as additive controls; activity x cost x
# country fully crossed so the trade-off can vary by country (as in
# fuel_cost.R and durability_cost.R).
ts_msg("Fitting choice model (glmer)...")
t0 <- Sys.time()

choice_model <- suppressWarnings(
  glmer(
    binary_choice ~
      fuel_code + durability_code + responsibility_code +
      activity_binary * cost_code * country +
      (1 | id),
    data = df,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

choice_emm <- emmeans(choice_model, ~ activity_binary * cost_code | country,
                      type = "response") |>
  as.data.frame() |>
  select(country, activity_binary, cost_code, prob, SE, df, asymp.LCL, asymp.UCL)

write_csv(choice_emm, choice_out)
ts_msg("Activity x cost choice emmeans written (",
       round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s): ", choice_out)
