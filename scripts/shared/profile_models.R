# Profile-membership models shared by the main predictor analysis and the
# additional (attitude / daily-life) predictor analysis.
# Sourced after scripts/shared/constants.R.

# Household habits (dailylife_actions_1-6, 1 = never ... 5 = always;
# 99 = not applicable). Missing-indicator method: a not-applicable answer is
# set to the item mean and flagged in a dummy that enters the model as a
# control (not reported), so no respondent is lost.
habit_items <- paste0("dailylife_actions_", 1:6)
habit_na    <- paste0(habit_items, "_na")

# Electric car use (dailylife_actions_7) as a category instead: 30% answered
# "not applicable", mostly respondents without a car. Reference (first level):
# no car / not applicable.
ev_levels <- c("No car / not applicable", "Rarely or never", "At least sometimes")

read_household_habits <- function(raw_file) {
  d <- readr::read_csv(raw_file, show_col_types = FALSE,
                       col_select = c(id, dplyr::all_of(habit_items),
                                      dailylife_actions_7)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  for (v in habit_items) {
    na <- is.na(d[[v]]) | d[[v]] == 99
    d[[paste0(v, "_na")]] <- as.numeric(na)
    d[[v]][na] <- mean(d[[v]][!na])
  }
  d |>
    dplyr::mutate(ev_use = factor(dplyr::case_when(
      dailylife_actions_7 == 99 ~ ev_levels[1],
      dailylife_actions_7 >= 3  ~ ev_levels[3],
      dailylife_actions_7 >= 1  ~ ev_levels[2]
    ), levels = ev_levels)) |>
    dplyr::select(-dailylife_actions_7)
}

# Respondent-level data for the profile model: LPA profile, flyer type,
# concern group, socio-demographic / responsibility covariates and household
# habits. Complete cases on these variables only.
read_profile_data <- function(profiles_file, groups_file, covariates_file,
                              raw_file) {
  profiles <- readr::read_csv(profiles_file, show_col_types = FALSE) |>
    dplyr::select(id, profile) |>
    dplyr::mutate(profile = factor(paste("Profile", profile),
                                   levels = paste("Profile", 1:3)))

  groups <- readr::read_csv(groups_file, show_col_types = FALSE) |>
    dplyr::select(id, flyer_type, concern_group) |>
    dplyr::filter(!is.na(flyer_type), !is.na(concern_group)) |>
    dplyr::mutate(
      flyer_type    = factor(flyer_type,    levels = flyer_levels),
      concern_group = factor(concern_group, levels = concern_levels)
    )

  covariates <- readr::read_csv(covariates_file, show_col_types = FALSE) |>
    dplyr::select(id, country, age, gender, income_decile, education_years,
                  publicinput_co2na,
                  actor_role_airlines, actor_role_fuelsuppliers, actor_role_government,
                  actor_role_manufacturers, actor_role_nonprofit,
                  actor_role_passengers, actor_role_researchers) |>
    dplyr::mutate(
      country   = dplyr::recode(as.character(country), !!!country_recode),
      country   = factor(country, levels = country_levels),
      gender    = factor(gender, levels = c(1, 2), labels = c("Female", "Male")),
      income_cat = dplyr::case_when(
        income_decile %in% 1:3   ~ "Low",
        income_decile %in% 4:7   ~ "Middle",
        income_decile %in% 8:10  ~ "High",
        TRUE                      ~ NA_character_
      ),
      income_cat = factor(income_cat, levels = c("Low", "Middle", "High")),
      education_cat = dplyr::case_when(
        education_years <= 12 ~ "Secondary or below",
        education_years <= 16 ~ "Post-secondary",
        education_years >= 17 ~ "Tertiary or above",
        TRUE                  ~ NA_character_
      ),
      education_cat = factor(education_cat,
                             levels = c("Secondary or below", "Post-secondary",
                                        "Tertiary or above")),
      dplyr::across(c(publicinput_co2na,
                      dplyr::starts_with("actor_role_")), as.numeric)
    )

  profiles |>
    dplyr::inner_join(groups,     by = "id") |>
    dplyr::inner_join(covariates, by = "id") |>
    dplyr::inner_join(read_household_habits(raw_file), by = "id") |>
    dplyr::filter(dplyr::if_all(dplyr::everything(), Negate(is.na)))
}

profile_formula <- profile ~
  country + flyer_type + concern_group + age + gender +
  income_cat + education_cat +
  publicinput_co2na +
  actor_role_airlines + actor_role_fuelsuppliers + actor_role_government +
  actor_role_manufacturers + actor_role_nonprofit +
  actor_role_passengers + actor_role_researchers +
  dailylife_actions_1 + dailylife_actions_2 + dailylife_actions_3 +
  dailylife_actions_4 + dailylife_actions_5 + dailylife_actions_6 +
  ev_use +
  dailylife_actions_1_na + dailylife_actions_2_na + dailylife_actions_3_na +
  dailylife_actions_4_na + dailylife_actions_5_na + dailylife_actions_6_na

# Reported in the AME output: every predictor except the not-applicable dummies
profile_ame_variables <- setdiff(all.vars(profile_formula)[-1], habit_na)

# Average marginal effects on profile membership probability (percentage
# points). For each predictor, the average change in every respondent's
# predicted probability of belonging to each profile:
#   categorical: predictor set to the level vs set to its reference level
#   continuous:  +1 SD from each respondent's observed value
# Effects for one predictor sum to zero across the three profiles. 95% CIs
# from parametric draws of the multinomial coefficients.
#   variables  predictors to report (default: all in the formula)
profile_ame <- function(formula, df, variables = NULL, n_draws = 1000,
                        seed = random_seed) {
  set.seed(seed)
  m_ame <- nnet::multinom(formula, data = df, trace = FALSE, Hess = TRUE)

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
  if (!is.null(variables)) {
    contrasts_list <- Filter(function(cn) cn$variable %in% variables, contrasts_list)
  }

  lapply(contrasts_list, function(cn) {
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
    tibble::tibble(
      variable  = cn$variable,
      level     = cn$level,
      reference = cn$ref,
      profile   = profile_levels_ame,
      ame       = 100 * est[1, ],
      lower     = 100 * apply(draws, 2, quantile, 0.025),
      upper     = 100 * apply(draws, 2, quantile, 0.975)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(significant = lower > 0 | upper < 0)
}
