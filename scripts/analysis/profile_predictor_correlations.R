library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "profile_models.R"))

if (exists("snakemake")) {
  profiles_file   <- snakemake@input[["profiles"]]
  groups_file     <- snakemake@input[["groups"]]
  covariates_file <- snakemake@input[["covariates"]]
  raw_file        <- snakemake@input[["raw"]]
  cor_out         <- snakemake@output[["correlations"]]
} else {
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  raw_file        <- here("raw-data", "raw_data.csv")
  cor_out         <- here("data", "profile_predictor_correlations.csv")
}

# Spearman correlations between the predictors of profile membership (the
# respondents and variables of the main profile model), pooled and within
# each country - a collinearity check for the supplementary material.
# Ordered categories enter as their rank (flyer type, concern, income,
# education); gender as a dummy; electric car use as a dummy for driving one
# at least sometimes (vs rarely/never or no car); household habits with
# "not applicable" left missing (pairwise-complete), not mean-imputed as in
# the model.

df <- read_profile_data(profiles_file, groups_file, covariates_file, raw_file)
for (v in habit_items) df[[v]][df[[paste0(v, "_na")]] == 1] <- NA

vars <- df |>
  transmute(
    country,
    flyer_type     = as.numeric(flyer_type),
    concern_group  = as.numeric(concern_group),
    male           = as.numeric(gender == "Male"),
    income_cat     = as.numeric(income_cat),
    education_cat  = as.numeric(education_cat),
    age,
    publicinput_co2na,
    across(starts_with("actor_role_")),
    across(all_of(habit_items)),
    ev_sometimes   = as.numeric(ev_use == "At least sometimes")
  )

cor_long <- function(d, label) {
  m <- cor(select(d, -country), method = "spearman", use = "pairwise.complete.obs")
  as_tibble(m, rownames = "var1") |>
    pivot_longer(-var1, names_to = "var2", values_to = "rho") |>
    mutate(country = label, n = nrow(d))
}

cors <- bind_rows(
  cor_long(vars, "All countries"),
  lapply(country_levels, function(cn) cor_long(filter(vars, country == cn), cn))
)

write_csv(cors, cor_out)
off <- filter(cors, var1 < var2)
message("Predictor correlations written: ", cor_out)
message("Largest |rho| (pooled): ")
print(off |> filter(country == "All countries") |> arrange(desc(abs(rho))) |> head(8))
message("Largest |rho| (any country): ")
print(off |> arrange(desc(abs(rho))) |> head(8))
