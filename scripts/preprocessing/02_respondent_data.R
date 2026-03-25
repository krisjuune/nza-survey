library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(here)
library(janitor)

if (exists("snakemake")) {
  input_file  <- snakemake@input[[1]]
  output_file <- snakemake@output[[1]]
} else {
  input_file  <- here("raw-data", "GBF_250326.csv")
  output_file <- here("data", "covariates.csv")
}

df <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  slice(-(1:2)) |>
  mutate(id = row_number()) |>
  clean_names() |>
  mutate(
    duration_min = as.numeric(duration_in_seconds) / 60,
    statements_flying_3 = 6 - suppressWarnings(as.numeric(statements_flying_3_reverse))
  )

covariates <- df |>
  select(
    id,
    distribution_channel,
    q_language,
    q_terminate_flag,
    q_r_del,
    start_date,
    finished,
    duration_min,
    age,
    gender,
    country,
    num_flying,
    purpose_flying,
    income_decile,
    income_subjective,
    education_years,
    education_level,
    concern_flying,
    statements_flying_1,
    statements_flying_2,
    statements_flying_3,
    reduce_flight,
    wtp_flying_likert,
    wtp_flying_amount,
    govspend_flying,

    # LPA columnes
    pref_path_trad_offset,
    measure_achiev_trad_offset,

    pref_path_geol_offset,
    measure_achiev_geol_offset,

    pref_path_safs,
    measure_achiev_safs,

    pref_path_synfuels,
    measure_achiev_synfuels,

    pref_path_electricplanes,
    measure_achiev_electrification,

    pref_path_efficiency
  )

if (nrow(covariates) != nrow(df)) {
  stop("Row mismatch in covariates extraction")
}

if (any(duplicated(covariates$id))) {
  stop("Duplicate IDs detected in covariates")
}

message("Covariates extraction successful: ", nrow(covariates), " rows")

write_csv(covariates, output_file)