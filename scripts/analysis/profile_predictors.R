library(dplyr)
library(tidyr)
library(nnet)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  profiles_file   <- snakemake@input[["profiles"]]
  groups_file     <- snakemake@input[["groups"]]
  covariates_file <- snakemake@input[["covariates"]]
  results_out     <- snakemake@output[["results"]]
} else {
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  results_out     <- here("data", "profile_predictors.csv")
}

profiles <- read_csv(profiles_file, show_col_types = FALSE) |>
  select(id, profile) |>
  mutate(profile = factor(paste("Profile", profile), levels = paste("Profile", 1:3)))

groups <- read_csv(groups_file, show_col_types = FALSE) |>
  select(id, flyer_type, concern_group) |>
  filter(!is.na(flyer_type), !is.na(concern_group)) |>
  mutate(
    flyer_type    = factor(flyer_type,    levels = flyer_levels),
    concern_group = factor(concern_group, levels = concern_levels)
  )

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  select(id, country, age, gender, income_decile, education_years,
         publicinput_co2na,
         actor_role_airlines, actor_role_fuelsuppliers, actor_role_government,
         actor_role_manufacturers, actor_role_nonprofit,
         actor_role_passengers, actor_role_researchers) |>
  mutate(
    country   = recode(as.character(country), !!!country_recode),
    country   = factor(country, levels = country_levels),
    gender    = factor(gender, levels = c(1, 2), labels = c("Female", "Male")),
    income_cat = case_when(
      income_decile %in% 1:3   ~ "Low",
      income_decile %in% 4:7   ~ "Middle",
      income_decile %in% 8:10  ~ "High",
      TRUE                      ~ NA_character_
    ),
    income_cat = factor(income_cat, levels = c("Low", "Middle", "High")),
    education_cat = case_when(
      education_years <= 12 ~ "Secondary or below",
      education_years <= 16 ~ "Post-secondary",
      education_years >= 17 ~ "Tertiary or above",
      TRUE                  ~ NA_character_
    ),
    education_cat = factor(education_cat,
                           levels = c("Secondary or below", "Post-secondary", "Tertiary or above")),
    across(c(publicinput_co2na,
             starts_with("actor_role_")), as.numeric)
  )

df <- profiles |>
  inner_join(groups,     by = "id") |>
  inner_join(covariates, by = "id") |>
  filter(if_all(everything(), Negate(is.na)))

message("Respondents in model: ", nrow(df))
message("Profile distribution:")
print(table(df$profile))

formula <- profile ~
  country + flyer_type + concern_group + age + gender +
  income_cat + education_cat +
  publicinput_co2na +
  actor_role_airlines + actor_role_fuelsuppliers + actor_role_government +
  actor_role_manufacturers + actor_role_nonprofit +
  actor_role_passengers + actor_role_researchers

tidy_multinom <- function(m, ref) {
  s  <- summary(m)
  z  <- s$coefficients / s$standard.errors
  p  <- 2 * (1 - pnorm(abs(z)))
  lapply(seq_len(nrow(s$coefficients)), function(i) {
    tibble(
      comparison = paste(rownames(s$coefficients)[i], "vs", ref),
      term       = colnames(s$coefficients),
      estimate   = s$coefficients[i, ],
      std_error  = s$standard.errors[i, ],
      z_value    = z[i, ],
      p_value    = p[i, ]
    )
  }) |>
    bind_rows() |>
    mutate(
      or    = exp(estimate),
      lower = exp(estimate - 1.96 * std_error),
      upper = exp(estimate + 1.96 * std_error)
    ) |>
    filter(term != "(Intercept)")
}

set.seed(random_seed)
m1 <- multinom(formula, data = mutate(df, profile = relevel(profile, ref = "Profile 1")),
               trace = FALSE)
set.seed(random_seed)
m2 <- multinom(formula, data = mutate(df, profile = relevel(profile, ref = "Profile 2")),
               trace = FALSE)

results <- bind_rows(
  tidy_multinom(m1, "Profile 1"),
  tidy_multinom(m2, "Profile 2") |> filter(comparison == "Profile 3 vs Profile 2")
)

write_csv(results, results_out)
message("Profile predictor results written: ", nrow(results), " rows, ",
        n_distinct(results$comparison), " comparisons")
