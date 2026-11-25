library(dplyr)
library(tidyr)
library(nnet)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "profile_models.R"))

if (exists("snakemake")) {
  profiles_file   <- snakemake@input[["profiles"]]
  groups_file     <- snakemake@input[["groups"]]
  covariates_file <- snakemake@input[["covariates"]]
  raw_file        <- snakemake@input[["raw"]]
  txt_out         <- snakemake@output[["txt"]]
} else {
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  raw_file        <- here("raw-data", "raw_data.csv")
  txt_out         <- here("output", "text-files", "lpa_additional_predictors.txt")
}

# Likelihood-ratio tests of groups of predictors of profile membership:
#   - household habits, within the main profile model (lpa_profile_predictors)
#   - stated pathway preferences and perceived achievability of each pathway,
#     added on top of the main profile model (not in any figure)

pref_items <- c("pref_path_trad_offset", "pref_path_geol_offset", "pref_path_safs",
                "pref_path_synfuels", "pref_path_electricplanes",
                "pref_path_efficiency")
achiev_items <- c("measure_achiev_trad_offset", "measure_achiev_geol_offset",
                  "measure_achiev_safs", "measure_achiev_synfuels",
                  "measure_achiev_electrification")

extra <- read_csv(raw_file, show_col_types = FALSE,
                  col_select = c(id, all_of(c(pref_items, achiev_items)))) |>
  mutate(across(everything(), as.numeric))

df <- read_profile_data(profiles_file, groups_file, covariates_file, raw_file) |>
  inner_join(extra, by = "id") |>
  filter(if_all(all_of(c(pref_items, achiev_items)), Negate(is.na)))

message("Respondents in model: ", nrow(df))

fit <- function(f) multinom(f, data = df, trace = FALSE, maxit = 500)
add_terms  <- function(f, vars) update(f, as.formula(paste(". ~ . +", paste(vars, collapse = " + "))))
drop_terms <- function(f, vars) update(f, as.formula(paste(". ~ . -", paste(vars, collapse = " - "))))

lr <- function(m0, m1) {
  stat <- as.numeric(2 * (logLik(m1) - logLik(m0)))
  k    <- m1$edf - m0$edf
  tibble(chisq = round(stat, 1), df = k,
         p = signif(pchisq(stat, k, lower.tail = FALSE), 3))
}

m_main <- fit(profile_formula)
m_null <- fit(profile ~ 1)
pseudo_r2 <- function(m) 1 - as.numeric(logLik(m) / logLik(m_null))

habits <- list(
  "Household habits (6 items + not-applicable dummies)" = c(habit_items, habit_na),
  "Electric car use (3 categories)"                     = "ev_use",
  "Household habits and electric car use"               = c(habit_items, habit_na, "ev_use")
)
blocks <- list(
  "Preferred pathway"                  = pref_items,
  "Perceived achievability of pathway" = achiev_items,
  "Both"                               = c(pref_items, achiev_items)
)

tests <- bind_rows(
  bind_rows(lapply(names(habits), function(b)
    bind_cols(tibble(predictors = b, test = "dropped from main model"),
              lr(fit(drop_terms(profile_formula, habits[[b]])), m_main)))),
  bind_rows(lapply(names(blocks), function(b)
    bind_cols(tibble(predictors = b, test = "added to main model"),
              lr(m_main, fit(add_terms(profile_formula, blocks[[b]]))))))
)

m_both <- fit(add_terms(profile_formula, blocks$Both))

txt <- c(
  "=== Likelihood-ratio tests: predictors of LPA profile membership ===",
  "",
  paste0("Multinomial logit of profile membership; n = ", nrow(df), " respondents."),
  "Main model: all predictors of lpa_profile_predictors.png, including household",
  "habits (99 = not applicable set to the item mean, with a not-applicable dummy",
  "per item as a control) and electric car use (rarely/never, at least",
  "sometimes, no car / not applicable).",
  "Preferred pathway: pref_path_* (1-5); perceived achievability: measure_achiev_*",
  "(1-5). Neither is in a figure.",
  "",
  capture.output(print(as.data.frame(tests), row.names = FALSE)),
  "",
  sprintf("McFadden pseudo-R2: main model %.3f; + pathway items %.3f",
          pseudo_r2(m_main), pseudo_r2(m_both))
)
writeLines(txt, txt_out)
message("Summary written: ", txt_out)
