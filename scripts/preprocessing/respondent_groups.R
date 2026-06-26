library(dplyr)
library(readr)
library(here)
library(mclust)

if (exists("snakemake")) {
  covariates_file <- snakemake@input[[1]]
  output_file     <- snakemake@output[["groups"]]
  summary_file    <- snakemake@output[["lpa_summary"]]
} else {
  covariates_file <- here("data", "covariates.csv")
  output_file     <- here("data", "respondent_groups.csv")
  summary_file    <- here("output", "lpa_profile_summary.txt")
}

covariates <- read_csv(covariates_file, show_col_types = FALSE)

# -------------------
# Preregistered flying-frequency groups, from the FlyerType screening
# variable (not derived from num_flying - see num_flying_6_TEXT for why).
# -------------------

flyer_levels <- c(
  "Never-flyer", "Infrequent/lapsed flyer", "Occasional flyer", "Frequent flyer"
)

covariates <- covariates |>
  mutate(
    flyer_type = recode(flyer_type,
      "Non- Flyer"       = "Never-flyer",
      "Infrequent Flyer" = "Infrequent/lapsed flyer",
      "Occasional Flyer" = "Occasional flyer",
      "Frequent Flyer"   = "Frequent flyer"
    ),
    flyer_type = factor(flyer_type, levels = flyer_levels)
  )

# -------------------
# Aviation climate concern sum score (4 items), grouped at the 33rd/67th
# percentiles of the observed distribution.
# -------------------

covariates <- covariates |>
  mutate(
    concern_score = concern_flying + statements_flying_1 +
      statements_flying_2 + statements_flying_3
  )

concern_cuts <- quantile(covariates$concern_score, probs = c(1 / 3, 2 / 3))

covariates <- covariates |>
  mutate(
    concern_group = case_when(
      concern_score <= concern_cuts[1] ~ "Low",
      concern_score <= concern_cuts[2] ~ "Mid",
      TRUE ~ "High"
    ),
    concern_group = factor(concern_group, levels = c("Low", "Mid", "High"))
  )

# -------------------
# Preferred decarbonisation pathway, via latent profile analysis (mclust)
# on the 10 pref_path_* / measure_achiev_* items (5 logics x 2 items each).
# Number of profiles and covariance structure both chosen by BIC.
# -------------------

lpa_items <- c(
  "pref_path_trad_offset", "measure_achiev_trad_offset",
  "pref_path_geol_offset", "measure_achiev_geol_offset",
  "pref_path_safs", "measure_achiev_safs",
  "pref_path_synfuels", "measure_achiev_synfuels",
  "pref_path_electricplanes", "measure_achiev_electrification"
)

lpa_data <- covariates |> select(all_of(lpa_items))
lpa_scaled <- scale(lpa_data)

set.seed(2026)
lpa_model <- Mclust(lpa_scaled, G = 1:6)

covariates <- covariates |>
  mutate(pathway_class = factor(paste0("Profile ", lpa_model$classification)))

respondent_groups <- covariates |>
  select(id, flyer_type, concern_score, concern_group, pathway_class)

write_csv(respondent_groups, output_file)
message("Respondent groups written to: ", output_file)

# -------------------
# LPA summary: model selection and per-class item means, for interpreting
# what each profile represents.
# -------------------

profile_sizes <- covariates |>
  dplyr::count(pathway_class, name = "n")

profile_means <- covariates |>
  select(pathway_class, all_of(lpa_items)) |>
  group_by(pathway_class) |>
  summarise(across(everything(), mean), .groups = "drop")

lpa_text <- c(
  "=== LPA MODEL SELECTION ===",
  "",
  paste0("Selected model: ", lpa_model$modelName,
         " with ", lpa_model$G, " profiles (BIC-based selection over G = 1:6)"),
  paste0("BIC: ", round(lpa_model$bic, 1)),
  "",
  "=== PROFILE SIZES ===",
  "",
  capture.output(print(profile_sizes)),
  "",
  "=== PROFILE MEANS (raw 1-5 item scale) ===",
  "",
  capture.output(print(profile_means))
)

writeLines(lpa_text, con = summary_file)
message("LPA summary written to: ", summary_file)
