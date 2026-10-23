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
  ame_out         <- snakemake@output[["ame"]]
} else {
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  covariates_file <- here("data", "covariates.csv")
  results_out     <- here("data", "profile_predictors.csv")
  ame_out         <- here("data", "profile_predictors_ame.csv")
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

# ── Average marginal effects on profile membership probability ───────────────
# For each predictor, the average change in every respondent's predicted
# probability of belonging to each profile (percentage points):
#   categorical: predictor set to the level vs set to its reference level
#   continuous:  +1 SD from each respondent's observed value
# Effects for one predictor sum to zero across the three profiles. 95% CIs
# from parametric draws of the multinomial coefficients.

n_draws <- 1000

set.seed(random_seed)
m_ame <- multinom(formula, data = df, trace = FALSE, Hess = TRUE)

profile_levels_ame <- levels(df$profile)
rhs   <- delete.response(terms(m_ame))
coefs <- coef(m_ame)                        # (K-1) x p, reference = Profile 1
beta_names <- as.vector(t(outer(rownames(coefs), colnames(coefs), paste, sep = ":")))
beta_hat   <- as.vector(t(coefs))
beta_draws <- MASS::mvrnorm(n_draws, mu = beta_hat,
                            Sigma = vcov(m_ame)[beta_names, beta_names])

# Mean predicted membership probabilities for design matrix X, for each
# coefficient vector in B (rows); returns n_B x K.
mean_probs <- function(X, B) {
  t(apply(B, 1, function(b) {
    eta <- cbind(0, X %*% t(matrix(b, nrow = nrow(coefs), byrow = TRUE)))
    p   <- exp(eta - apply(eta, 1, max))
    colMeans(p / rowSums(p))
  }))
}

design_for <- function(newdata) {
  model.matrix(rhs, model.frame(rhs, newdata, xlev = m_ame$xlevels))
}

contrasts_list <- c(
  # categorical predictors: every non-reference level
  unlist(lapply(names(m_ame$xlevels), function(v) {
    lv <- m_ame$xlevels[[v]]
    lapply(lv[-1], function(l) list(variable = v, level = l, ref = lv[1]))
  }), recursive = FALSE),
  # continuous predictors: +1 SD
  lapply(setdiff(all.vars(rhs), names(m_ame$xlevels)),
         function(v) list(variable = v, level = v, ref = NA))
)

ame <- lapply(contrasts_list, function(cn) {
  d1 <- df; d0 <- df
  if (is.na(cn$ref)) {
    d1[[cn$variable]] <- d1[[cn$variable]] + sd(df[[cn$variable]])
  } else {
    d1[[cn$variable]] <- factor(cn$level, levels = m_ame$xlevels[[cn$variable]])
    d0[[cn$variable]] <- factor(cn$ref,   levels = m_ame$xlevels[[cn$variable]])
  }
  X1 <- design_for(d1); X0 <- design_for(d0)
  est   <- mean_probs(X1, rbind(beta_hat))   - mean_probs(X0, rbind(beta_hat))
  draws <- mean_probs(X1, beta_draws)        - mean_probs(X0, beta_draws)
  tibble(
    variable  = cn$variable,
    level     = cn$level,
    reference = cn$ref,
    profile   = profile_levels_ame,
    ame       = 100 * est[1, ],
    lower     = 100 * apply(draws, 2, quantile, 0.025),
    upper     = 100 * apply(draws, 2, quantile, 0.975)
  )
}) |>
  bind_rows() |>
  mutate(significant = lower > 0 | upper < 0)

write_csv(ame, ame_out)
message("Profile membership AMEs written: ", ame_out, " (",
        n_distinct(paste(ame$variable, ame$level)), " predictors)")
