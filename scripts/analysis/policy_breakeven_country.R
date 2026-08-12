library(dplyr)
library(lme4)
library(readr)
library(tidyr)
library(purrr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file  <- snakemake@input[["conjoint"]]
  covariate_file <- snakemake@input[["covariates"]]
  grid_file      <- snakemake@output[["grid"]]
} else {
  conjoint_file  <- here("data", "conjoint_long.csv")
  covariate_file <- here("data", "covariates.csv")
  grid_file      <- here("data", "policy_breakeven_country_grid.csv")
}

covariates <- read_csv(covariate_file, show_col_types = FALSE) |>
  select(id, country) |>
  mutate(
    id      = as.character(id),
    country = recode(as.character(country), !!!country_recode),
    country = factor(country, levels = country_levels)
  )

df <- read_csv(
  conjoint_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
) |>
  filter(!is.na(policy_type), policy_type %in% c("GBF", "SAF_ETS")) |>
  mutate(
    id          = as.character(id),
    cost_pct    = as.numeric(cost_code),
    policy_type = factor(policy_type, levels = c("GBF", "SAF_ETS"))
  ) |>
  left_join(covariates, by = "id") |>
  filter(!is.na(country))

# Country × policy type × price interaction model.
# GBF × Australia is the reference cell.
model <- suppressWarnings(
  glmer(
    binary_choice ~ policy_type * cost_pct * country + (1 | id),
    data    = df,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

message("Model fitted. Fixed effects: ", length(fixef(model)))

betas    <- fixef(model)
vcov_mat <- as.matrix(vcov(model))

# Predict choice probabilities for both pathways at the same absolute cost level.
# Both curves vary along cost_seq; their crossing = breakeven.
predict_probs_country <- function(cost_seq, ctry) {
  n <- length(cost_seq)

  new_s <- tibble(
    policy_type = factor("SAF_ETS", levels = levels(df$policy_type)),
    cost_pct    = cost_seq,
    country     = factor(ctry, levels = levels(df$country))
  )
  new_g <- tibble(
    policy_type = factor("GBF", levels = levels(df$policy_type)),
    cost_pct    = cost_seq,
    country     = factor(ctry, levels = levels(df$country))
  )

  Xs <- model.matrix(~ policy_type * cost_pct * country, data = new_s)
  Xg <- model.matrix(~ policy_type * cost_pct * country, data = new_g)

  eta_s <- as.numeric(Xs %*% betas)
  eta_g <- as.numeric(Xg %*% betas)
  ps    <- plogis(eta_s);  dps <- ps * (1 - ps)
  pg    <- plogis(eta_g);  dpg <- pg * (1 - pg)

  v_s    <- diag(Xs %*% vcov_mat %*% t(Xs))
  v_g    <- diag(Xg %*% vcov_mat %*% t(Xg))
  cov_sg <- rowSums((Xs %*% vcov_mat) * Xg)
  se_gap <- sqrt(pmax(dps^2 * v_s + dpg^2 * v_g - 2 * dps * dpg * cov_sg, 0))

  tibble(
    country        = ctry,
    cost_pct       = cost_seq,
    prob_saf       = ps,
    prob_saf_lower = plogis(eta_s - 1.96 * sqrt(pmax(v_s, 0))),
    prob_saf_upper = plogis(eta_s + 1.96 * sqrt(pmax(v_s, 0))),
    prob_gbf       = pg,
    prob_gbf_lower = plogis(eta_g - 1.96 * sqrt(pmax(v_g, 0))),
    prob_gbf_upper = plogis(eta_g + 1.96 * sqrt(pmax(v_g, 0))),
    prob_gap       = ps - pg,
    prob_gap_lower = ps - pg - 1.96 * se_gap,
    prob_gap_upper = ps - pg + 1.96 * se_gap
  )
}

# Extend to 90pp to cover realistic eSAF scenario range (up to ~81pp).
countries_to_plot <- country_levels
cost_seq          <- seq(0, 90, by = 0.5)

prob_grid <- map_dfr(countries_to_plot, function(ctry) {
  predict_probs_country(cost_seq, ctry)
})

write_csv(prob_grid, grid_file)
message("Country-level probability grid saved: ", grid_file)

# Log breakeven per country (zero-crossing of prob_gap on absolute cost axis)
for (ctry in countries_to_plot) {
  g <- filter(prob_grid, country == ctry)
  pos <- filter(g, prob_gap >= 0) |> slice_tail(n = 1)
  neg <- filter(g, prob_gap <  0) |> slice_head(n = 1)
  if (nrow(pos) > 0 && nrow(neg) > 0) {
    bev <- pos$cost_pct +
      (0 - pos$prob_gap) / (neg$prob_gap - pos$prob_gap) * (neg$cost_pct - pos$cost_pct)
    message(sprintf("  %s breakeven: %.1f pp", ctry, bev))
  } else {
    message(sprintf("  %s: no breakeven in [0, 90] range", ctry))
  }
}
