# Conjoint models shared by the main analysis and the robustness checks.
# Sourced after scripts/shared/constants.R (uses `attributes`).

# Country choice model: logit of choosing a profile on all attributes, each
# interacted with country, with a random intercept per respondent. Returns
# the marginal choice probabilities (emmeans) per attribute level and country.
fit_country_choice <- function(df, log = message) {
  t0 <- Sys.time()
  elapsed <- function() round(as.numeric(difftime(Sys.time(), t0, units = "secs")))

  log("Fitting choice model (glmer) on ", dplyr::n_distinct(df$id), " respondents...")
  model <- suppressWarnings(
    lme4::glmer(
      binary_choice ~
        (fuel_code + activity_code + durability_code +
           responsibility_code + cost_code) * country +
        (1 | id),
      data = df,
      family = binomial,
      control = lme4::glmerControl(
        optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
      )
    )
  )
  log("Choice model done (", elapsed(), "s). Computing emmeans...")

  lapply(seq_along(attributes), function(i) {
    attr <- attributes[[i]]
    log("  [", i, "/", length(attributes), "] emmeans for ", attr,
        " (", elapsed(), "s elapsed)")
    emmeans::emmeans(model, as.formula(paste0("~ ", attr, " | country")),
                     type = "response") |>
      as.data.frame() |>
      dplyr::mutate(attribute = attr)
  }) |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(cols = dplyr::ends_with("_code"), names_to = "tmp",
                        values_to = "code") |>
    dplyr::filter(!is.na(code)) |>
    dplyr::select(country, attribute, code, prob, SE, df, asymp.LCL, asymp.UCL)
}

# Design-based marginal means: because attribute levels are fully randomised,
# the marginal mean of a level is the (optionally weighted) share of profiles
# with that level that were chosen. 95% CIs by bootstrapping respondents
# within each country (each respondent contributes 12 profiles). Unlike glmer,
# this estimator accepts survey weights.
#   df       long conjoint data with id, country, binary_choice, *_code columns
#   weight   name of a per-respondent weight column, or NULL
design_mm <- function(df, weight = NULL, B = 500, seed = 1) {
  long <- df |>
    dplyr::mutate(w = if (is.null(weight)) 1 else .data[[weight]]) |>
    dplyr::select(id, country, binary_choice, w, dplyr::all_of(attributes)) |>
    tidyr::pivot_longer(dplyr::all_of(attributes), names_to = "attribute",
                        values_to = "code")

  # Per respondent x level: weighted choice sum and weight sum, so a bootstrap
  # replicate only needs respondent-level resampling of these totals.
  per_resp <- long |>
    dplyr::group_by(country, id, attribute, code) |>
    dplyr::summarise(yw = sum(w * binary_choice), ww = sum(w), .groups = "drop")

  est <- per_resp |>
    dplyr::group_by(country, attribute, code) |>
    dplyr::summarise(prob = sum(yw) / sum(ww), .groups = "drop")

  set.seed(seed)
  ids <- dplyr::distinct(per_resp, country, id)
  boot <- lapply(seq_len(B), function(b) {
    draw <- ids |>
      dplyr::group_by(country) |>
      dplyr::slice_sample(prop = 1, replace = TRUE) |>
      dplyr::ungroup() |>
      dplyr::count(country, id, name = "k")
    per_resp |>
      dplyr::inner_join(draw, by = c("country", "id")) |>
      dplyr::group_by(country, attribute, code) |>
      dplyr::summarise(prob = sum(k * yw) / sum(k * ww), .groups = "drop") |>
      dplyr::mutate(b = b)
  }) |>
    dplyr::bind_rows()

  boot |>
    dplyr::group_by(country, attribute, code) |>
    dplyr::summarise(asymp.LCL = stats::quantile(prob, 0.025),
                     asymp.UCL = stats::quantile(prob, 0.975), .groups = "drop") |>
    dplyr::right_join(est, by = c("country", "attribute", "code")) |>
    dplyr::select(country, attribute, code, prob, asymp.LCL, asymp.UCL)
}
