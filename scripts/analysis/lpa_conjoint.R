library(dplyr)
library(tidyr)
library(readr)
library(here)
library(mclust)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file <- snakemake@input[["conjoint"]]
  ic_out        <- snakemake@output[["ic"]]
  profiles_out  <- snakemake@output[["profiles"]]
  means_out     <- snakemake@output[["means"]]
} else {
  conjoint_file <- here("data", "conjoint_long.csv")
  ic_out        <- here("data", "lpa_conjoint_ic.csv")
  profiles_out  <- here("data", "lpa_conjoint_profiles.csv")
  means_out     <- here("data", "lpa_conjoint_means.csv")
}

conjoint <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
)

# -------------------------------------------------------------------
# Feature extraction: per-person, per-attribute-level preference score.
#
# Within each choice task, the chosen alternative's levels receive +1
# and the rejected alternative's levels receive -1. Summing across all
# 12 alternatives (6 tasks × 2) and dividing by the number of times
# each level appeared gives a normalised preference rate in [-1, +1],
# where +1 = always chose when present, -1 = always rejected.
# -------------------------------------------------------------------
conjoint <- mutate(conjoint, signed = 2L * binary_choice - 1L)

# Build a lookup from column name to (attribute, raw level) so we can
# join display labels without string-splitting on underscores.
attr_level_map <- lapply(attributes, function(attr) {
  tibble(
    attr_level = paste(attr, unique(conjoint[[attr]]), sep = "__"),
    attribute  = attr,
    level      = unique(conjoint[[attr]])
  )
}) |>
  bind_rows()

pref_long <- lapply(attributes, function(attr) {
  conjoint |>
    group_by(id, level = .data[[attr]]) |>
    summarise(pref = sum(signed) / n(), .groups = "drop") |>
    mutate(attr_level = paste(attr, level, sep = "__")) |>
    select(id, attr_level, pref)
}) |>
  bind_rows()

pref_wide <- pref_long |>
  pivot_wider(names_from = attr_level, values_from = pref, values_fill = 0)

ids <- pref_wide$id

# Scale before clustering so attributes with more levels don't dominate.
X <- pref_wide |> select(-id) |> as.matrix() |> scale()

# -------------------------------------------------------------------
# LPA model selection: G = 2:8 profiles, diagonal covariance only.
# -------------------------------------------------------------------
set.seed(random_seed)
message("Fitting LPA models for G = 2 to 8...")
t0 <- Sys.time()

lpa_ic <- lapply(2:8, function(g) {
  message("  G = ", g, " [", round(difftime(Sys.time(), t0, units = "secs")), "s]")

  m <- Mclust(X, G = g, modelNames = c("EEI", "VEI", "EVI", "VVI"),
              verbose = FALSE)

  if (is.null(m)) {
    message("  G = ", g, ": no model converged")
    return(tibble(G = g, model = NA_character_, AIC = NA, BIC = NA,
                  SABIC = NA, AWE = NA, ICL = NA))
  }

  n  <- m$n
  k  <- m$df
  ll <- m$loglik
  z  <- m$z
  entropy <- -sum(z * log(z + .Machine$double.eps))

  tibble(
    G     = g,
    model = m$modelName,
    AIC   = 2 * ll - 2 * k,
    BIC   = 2 * ll - k * log(n),
    SABIC = 2 * ll - k * log((n + 2) / 24),
    AWE   = 2 * ll - k * (log(n) + 1.5),
    ICL   = (2 * ll - k * log(n)) - 2 * entropy
  )
}) |>
  bind_rows()

write_csv(lpa_ic, ic_out)
message("IC table written [", round(difftime(Sys.time(), t0, units = "secs")), "s]")

# -------------------------------------------------------------------
# G = 3 profile solution
# -------------------------------------------------------------------
message("Extracting G = 3 profile solution...")
set.seed(random_seed)
m3 <- Mclust(X, G = 3, modelNames = c("EEI", "VEI", "EVI", "VVI"),
             verbose = FALSE)

# Respondent-level: hard classification + posterior probabilities
z_named           <- m3$z
colnames(z_named) <- paste0("prob_", seq_len(ncol(z_named)))
profiles <- tibble(id = ids, profile = m3$classification) |>
  bind_cols(as_tibble(z_named))

write_csv(profiles, profiles_out)

# Profile-level: mean (unscaled) preference per attribute level per profile.
# Using the original pref_wide (before scaling) so means are on the
# interpretable [-1, +1] preference-rate scale.
profile_means <- pref_wide |>
  mutate(profile = m3$classification) |>
  pivot_longer(
    cols      = -c(id, profile),
    names_to  = "attr_level",
    values_to = "pref"
  ) |>
  group_by(profile, attr_level) |>
  summarise(
    mean_pref = mean(pref),
    sd_pref   = sd(pref),
    n         = n(),
    .groups   = "drop"
  ) |>
  left_join(attr_level_map, by = "attr_level")

write_csv(profile_means, means_out)
message("Profile solution written [", round(difftime(Sys.time(), t0, units = "secs")), "s total]")
