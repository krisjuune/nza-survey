# Latent profile analysis of conjoint preferences, shared by the main LPA
# (scripts/analysis/lpa_conjoint.R) and the attention-check robustness check
# (scripts/analysis/robustness_lca.R). Sourced after scripts/shared/constants.R
# (uses `attributes`, `random_seed`).

lpa_model_names <- c("EEI", "VEI", "EVI", "VVI")   # diagonal covariance only

# Per-person, per-attribute-level preference score.
#
# Within each choice task, the chosen alternative's levels receive +1 and the
# rejected alternative's levels receive -1. Summing across all 12 alternatives
# (6 tasks x 2) and dividing by the number of times each level appeared gives
# a normalised preference rate in [-1, +1], where +1 = always chose when
# present, -1 = always rejected.
# Returns list(wide = id x attr_level scores, map = attr_level lookup).
lpa_preference_scores <- function(conjoint) {
  conjoint <- dplyr::mutate(conjoint, signed = 2L * binary_choice - 1L)

  # Lookup from column name to (attribute, raw level), so display labels can
  # be joined without string-splitting on underscores.
  attr_level_map <- lapply(attributes, function(attr) {
    tibble::tibble(
      attr_level = paste(attr, unique(conjoint[[attr]]), sep = "__"),
      attribute  = attr,
      level      = unique(conjoint[[attr]])
    )
  }) |>
    dplyr::bind_rows()

  pref_long <- lapply(attributes, function(attr) {
    conjoint |>
      dplyr::group_by(id, level = .data[[attr]]) |>
      dplyr::summarise(pref = sum(signed) / dplyr::n(), .groups = "drop") |>
      dplyr::mutate(attr_level = paste(attr, level, sep = "__")) |>
      dplyr::select(id, attr_level, pref)
  }) |>
    dplyr::bind_rows()

  wide <- pref_long |>
    tidyr::pivot_wider(names_from = attr_level, values_from = pref, values_fill = 0)

  list(wide = wide, map = attr_level_map)
}

# Scale before clustering so attributes with more levels don't dominate.
lpa_matrix <- function(wide) wide |> dplyr::select(-id) |> as.matrix() |> scale()

# Information criteria for G = 1..max_g profiles (larger = better, mclust sign)
lpa_information_criteria <- function(X, max_g = 8, progress = message) {
  set.seed(random_seed)
  t0 <- Sys.time()
  lapply(seq_len(max_g), function(g) {
    progress("  G = ", g, " [", round(difftime(Sys.time(), t0, units = "secs")), "s]")

    m <- mclust::Mclust(X, G = g, modelNames = lpa_model_names, verbose = FALSE)

    if (is.null(m)) {
      progress("  G = ", g, ": no model converged")
      return(tibble::tibble(G = g, model = NA_character_, AIC = NA, BIC = NA,
                            SABIC = NA, AWE = NA, ICL = NA))
    }

    n  <- m$n
    k  <- m$df
    ll <- m$loglik
    z  <- m$z
    entropy <- -sum(z * log(z + .Machine$double.eps))

    tibble::tibble(
      G     = g,
      model = m$modelName,
      AIC   = 2 * ll - 2 * k,
      BIC   = 2 * ll - k * log(n),
      SABIC = 2 * ll - k * log((n + 2) / 24),
      AWE   = 2 * ll - k * (log(n) + 1.5),
      ICL   = (2 * ll - k * log(n)) - 2 * entropy
    )
  }) |>
    dplyr::bind_rows()
}

# G-profile solution: hard classification + posterior probabilities per id
lpa_fit_profiles <- function(X, ids, G = 3) {
  set.seed(random_seed)
  m <- mclust::Mclust(X, G = G, modelNames = lpa_model_names, verbose = FALSE)
  z <- m$z
  colnames(z) <- paste0("prob_", seq_len(ncol(z)))
  list(model = m,
       profiles = tibble::tibble(id = ids, profile = m$classification) |>
         dplyr::bind_cols(tibble::as_tibble(z)))
}

# Mean (unscaled) preference per attribute level per profile, on the
# interpretable [-1, +1] preference-rate scale
lpa_profile_means <- function(wide, classification, attr_level_map) {
  wide |>
    dplyr::mutate(profile = classification) |>
    tidyr::pivot_longer(cols = -c(id, profile), names_to = "attr_level",
                        values_to = "pref") |>
    dplyr::group_by(profile, attr_level) |>
    dplyr::summarise(
      mean_pref = mean(pref),
      sd_pref   = sd(pref),
      n         = dplyr::n(),
      .groups   = "drop"
    ) |>
    dplyr::left_join(attr_level_map, by = "attr_level")
}
