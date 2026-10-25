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
  cost_file      <- snakemake@input[["costs"]]
  grid_file      <- snakemake@output[["grid"]]
  summary_file   <- snakemake@output[["summary"]]
  txt_file       <- snakemake@output[["txt"]]
} else {
  conjoint_file  <- here("data", "conjoint_long.csv")
  covariate_file <- here("data", "covariates.csv")
  cost_file      <- here("data", "airfare_costs.csv")
  grid_file      <- here("data", "policy_breakeven_country_grid.csv")
  summary_file   <- here("data", "policy_breakeven_country_summary.csv")
  txt_file       <- here("output", "text-files", "policy_breakeven_country.txt")
}

n_draws <- 4000

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

costs <- read_csv(cost_file, show_col_types = FALSE)

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

# Parametric bootstrap draws of the fixed effects, used to propagate model
# uncertainty into quantities that are non-linear in the coefficients
# (breakeven premium, probability gap at projected costs).
set.seed(random_seed)
beta_draws <- MASS::mvrnorm(n_draws, mu = betas, Sigma = vcov_mat)

# Linear predictor for one pathway in one country is eta(c) = a + b * c.
# Returns the design rows giving a (intercept) and b (price slope).
design_ab <- function(policy, ctry) {
  rows <- tibble(
    policy_type = factor(policy, levels = levels(df$policy_type)),
    cost_pct    = c(0, 1),
    country     = factor(ctry, levels = levels(df$country))
  )
  X <- model.matrix(~ policy_type * cost_pct * country, data = rows)
  list(a = X[1, ], b = X[2, ] - X[1, ])
}

ci <- function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE, names = FALSE)

# ── Prediction grid (panel a curves) ─────────────────────────────────────────

# Extends to 120% to cover every projected eSAF fare (up to 117%); the survey
# only tested 10-50%, so `in_tested_range` marks the extrapolated part.
cost_seq <- seq(0, 120, by = 0.5)

predict_curve <- function(policy, ctry) {
  d   <- design_ab(policy, ctry)
  X   <- outer(rep(1, length(cost_seq)), d$a) + outer(cost_seq, d$b)
  eta <- as.numeric(X %*% betas)
  se  <- sqrt(pmax(rowSums((X %*% vcov_mat) * X), 0))
  tibble(
    country    = ctry,
    policy     = policy,
    cost_pct   = cost_seq,
    prob       = plogis(eta),
    prob_lower = plogis(eta - 1.96 * se),
    prob_upper = plogis(eta + 1.96 * se)
  )
}

prob_grid <- expand_grid(policy = c("SAF_ETS", "GBF"), country = country_levels) |>
  pmap_dfr(\(policy, country) predict_curve(policy, country)) |>
  mutate(in_tested_range = cost_pct >= tested_cost_range[1] &
                           cost_pct <= tested_cost_range[2])

write_csv(prob_grid, grid_file)
message("Country-level probability grid saved: ", grid_file)

# ── Projected costs per country and fuel family ──────────────────────────────

# central = median of the pathway medians; path_* = range of pathway medians;
# full_* = range including each source's scenario bounds.
summarise_family <- function(d) {
  tibble(
    central = median(d$fare_change_median),
    path_lo = min(d$fare_change_median),
    path_hi = max(d$fare_change_median),
    full_lo = min(d$fare_change_low),
    full_hi = max(d$fare_change_high)
  )
}

cost_summary <- costs |>
  filter(fuel_family %in% c("eSAF", "GBF")) |>
  group_by(country, fuel_family) |>
  group_modify(\(d, k) summarise_family(d)) |>
  ungroup() |>
  pivot_wider(names_from = fuel_family,
              values_from = c(central, path_lo, path_hi, full_lo, full_hi),
              names_glue = "{tolower(fuel_family)}_{.value}")

# Projected eSAF premium over GBF, from every eSAF x GBF pathway pair.
# Scenario bounds are matched (low with low, high with high) because both
# come from the same kerosene price scenario.
projected_premium <- costs |>
  filter(fuel_family == "eSAF") |>
  inner_join(filter(costs, fuel_family == "GBF"), by = "country",
             suffix = c("_s", "_g"), relationship = "many-to-many") |>
  mutate(
    x      = fare_change_median_s - fare_change_median_g,
    x_low  = fare_change_low_s    - fare_change_low_g,
    x_high = fare_change_high_s   - fare_change_high_g
  ) |>
  group_by(country) |>
  summarise(
    projected_path_lo = min(x),
    projected_path_hi = max(x),
    projected_full_lo = min(x, x_low, x_high),
    projected_full_hi = max(x, x_low, x_high),
    .groups = "drop"
  ) |>
  left_join(cost_summary, by = "country") |>
  mutate(projected_central = esaf_central - gbf_central)

# ── Per-country summary ──────────────────────────────────────────────────────

summarise_country <- function(ctry) {
  s <- design_ab("SAF_ETS", ctry)
  g <- design_ab("GBF", ctry)
  a_s <- sum(s$a * betas); b_s <- sum(s$b * betas)
  a_g <- sum(g$a * betas); b_g <- sum(g$b * betas)
  a_s_d <- as.numeric(beta_draws %*% s$a); b_s_d <- as.numeric(beta_draws %*% s$b)
  a_g_d <- as.numeric(beta_draws %*% g$a); b_g_d <- as.numeric(beta_draws %*% g$b)

  out <- tibble(
    country     = ctry,
    saf_slope   = b_s,
    gbf_slope   = b_g,
    # share of draws where the SAF pathway is not price-sensitive (premium undefined)
    share_saf_slope_nonneg = mean(b_s_d >= 0)
  )

  pp <- filter(projected_premium, country == ctry)
  if (nrow(pp) == 0) return(out)

  c_s <- pp$esaf_central
  c_g <- pp$gbf_central

  # Accepted premium: the eSAF fare c* at which the SAF pathway is exactly as
  # popular as GBF at its projected fare, i.e. a_s + b_s c* = a_g + b_g c_g.
  premium   <- (a_g + b_g * c_g - a_s) / b_s - c_g
  premium_d <- (a_g_d + b_g_d * c_g - a_s_d) / b_s_d - c_g
  premium_d[b_s_d >= 0] <- NA

  p_s   <- plogis(a_s + b_s * c_s);       p_g   <- plogis(a_g + b_g * c_g)
  p_s_d <- plogis(a_s_d + b_s_d * c_s);   p_g_d <- plogis(a_g_d + b_g_d * c_g)

  # Reference: both pathways at GBF's projected fare (no eSAF cost premium)
  p_s_eq   <- plogis(a_s + b_s * c_g)
  p_s_eq_d <- plogis(a_s_d + b_s_d * c_g)

  # Cost uncertainty: gap at point estimates for every eSAF x GBF pathway
  # pair (pathway medians), and additionally at the matched scenario bounds.
  gap_at <- \(cs, cg) plogis(a_s + b_s * cs) - plogis(a_g + b_g * cg)
  pairs <- costs |>
    filter(country == ctry, fuel_family == "eSAF") |>
    inner_join(filter(costs, country == ctry, fuel_family == "GBF"),
               by = "country", suffix = c("_s", "_g"), relationship = "many-to-many")
  gap_path <- gap_at(pairs$fare_change_median_s, pairs$fare_change_median_g)
  gap_full <- c(gap_path,
                gap_at(pairs$fare_change_low_s,  pairs$fare_change_low_g),
                gap_at(pairs$fare_change_high_s, pairs$fare_change_high_g))

  bind_cols(out, pp |> select(-country), tibble(
    prob_saf_equal_price       = p_s_eq,
    prob_saf_equal_price_lower = ci(p_s_eq_d)[1],
    prob_saf_equal_price_upper = ci(p_s_eq_d)[2],
    prob_gap_equal_price       = p_s_eq - p_g,
    prob_gap_equal_price_lower = ci(p_s_eq_d - p_g_d)[1],
    prob_gap_equal_price_upper = ci(p_s_eq_d - p_g_d)[2],
    prob_gap_path_lo = min(gap_path), prob_gap_path_hi = max(gap_path),
    prob_gap_full_lo = min(gap_full), prob_gap_full_hi = max(gap_full),
    accepted_premium       = premium,
    accepted_premium_lower = ci(premium_d)[1],
    accepted_premium_upper = ci(premium_d)[2],
    accepted_saf_cost      = c_g + premium,
    prob_saf_at_projected       = p_s,
    prob_saf_at_projected_lower = ci(p_s_d)[1],
    prob_saf_at_projected_upper = ci(p_s_d)[2],
    prob_gbf_at_projected       = p_g,
    prob_gbf_at_projected_lower = ci(p_g_d)[1],
    prob_gbf_at_projected_upper = ci(p_g_d)[2],
    prob_gap_at_projected       = p_s - p_g,
    prob_gap_at_projected_lower = ci(p_s_d - p_g_d)[1],
    prob_gap_at_projected_upper = ci(p_s_d - p_g_d)[2],
    # TRUE where a quantity relies on prices outside the tested range
    extrapolated_projection = pp$esaf_full_hi > tested_cost_range[2] |
                              pp$gbf_full_lo  < tested_cost_range[1],
    extrapolated_accepted   = (c_g + premium) > tested_cost_range[2] |
                              (c_g + premium) < tested_cost_range[1]
  ))
}

summary_tbl <- map_dfr(country_levels, summarise_country)

# ── Pooled ("All countries") summary ─────────────────────────────────────────
# Per-country pathway samples are small and noisy, so we also fit a pooled
# model with no country term, evaluated at the median of the per-country
# projected fares among the countries already shown in panel b (i.e. those
# with both an eSAF and a GBF cost projection).

model_pooled <- suppressWarnings(
  glmer(
    binary_choice ~ policy_type * cost_pct + (1 | id),
    data    = df,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

betas_pooled <- fixef(model_pooled)
vcov_pooled  <- as.matrix(vcov(model_pooled))

set.seed(random_seed)
beta_draws_pooled <- MASS::mvrnorm(n_draws, mu = betas_pooled, Sigma = vcov_pooled)

design_ab_pooled <- function(policy) {
  rows <- tibble(
    policy_type = factor(policy, levels = levels(df$policy_type)),
    cost_pct    = c(0, 1)
  )
  X <- model.matrix(~ policy_type * cost_pct, data = rows)
  list(a = X[1, ], b = X[2, ] - X[1, ])
}

pooled_fare <- projected_premium |>
  summarise(esaf_central = median(esaf_central), gbf_central = median(gbf_central))

summarise_pooled <- function() {
  s <- design_ab_pooled("SAF_ETS")
  g <- design_ab_pooled("GBF")
  a_s <- sum(s$a * betas_pooled); b_s <- sum(s$b * betas_pooled)
  a_g <- sum(g$a * betas_pooled); b_g <- sum(g$b * betas_pooled)
  a_s_d <- as.numeric(beta_draws_pooled %*% s$a); b_s_d <- as.numeric(beta_draws_pooled %*% s$b)
  a_g_d <- as.numeric(beta_draws_pooled %*% g$a); b_g_d <- as.numeric(beta_draws_pooled %*% g$b)

  c_s <- pooled_fare$esaf_central
  c_g <- pooled_fare$gbf_central

  premium   <- (a_g + b_g * c_g - a_s) / b_s - c_g
  premium_d <- (a_g_d + b_g_d * c_g - a_s_d) / b_s_d - c_g
  premium_d[b_s_d >= 0] <- NA

  p_s   <- plogis(a_s + b_s * c_s);       p_g   <- plogis(a_g + b_g * c_g)
  p_s_d <- plogis(a_s_d + b_s_d * c_s);   p_g_d <- plogis(a_g_d + b_g_d * c_g)

  p_s_eq   <- plogis(a_s + b_s * c_g)
  p_s_eq_d <- plogis(a_s_d + b_s_d * c_g)

  tibble(
    country                    = "All countries",
    saf_slope                  = b_s,
    gbf_slope                  = b_g,
    share_saf_slope_nonneg      = mean(b_s_d >= 0),
    esaf_central                = c_s,
    gbf_central                 = c_g,
    projected_central           = c_s - c_g,
    prob_saf_equal_price        = p_s_eq,
    prob_saf_equal_price_lower  = ci(p_s_eq_d)[1],
    prob_saf_equal_price_upper  = ci(p_s_eq_d)[2],
    prob_gap_equal_price        = p_s_eq - p_g,
    prob_gap_equal_price_lower  = ci(p_s_eq_d - p_g_d)[1],
    prob_gap_equal_price_upper  = ci(p_s_eq_d - p_g_d)[2],
    accepted_premium            = premium,
    accepted_premium_lower      = ci(premium_d)[1],
    accepted_premium_upper      = ci(premium_d)[2],
    accepted_saf_cost           = c_g + premium,
    prob_saf_at_projected       = p_s,
    prob_saf_at_projected_lower = ci(p_s_d)[1],
    prob_saf_at_projected_upper = ci(p_s_d)[2],
    prob_gbf_at_projected       = p_g,
    prob_gbf_at_projected_lower = ci(p_g_d)[1],
    prob_gbf_at_projected_upper = ci(p_g_d)[2],
    prob_gap_at_projected       = p_s - p_g,
    prob_gap_at_projected_lower = ci(p_s_d - p_g_d)[1],
    prob_gap_at_projected_upper = ci(p_s_d - p_g_d)[2]
  )
}

pooled_row <- summarise_pooled()
message(sprintf(
  "Pooled (all countries) estimate: median fares eSAF +%.0f%%, GBF +%.0f%%.",
  pooled_fare$esaf_central, pooled_fare$gbf_central
))

write_csv(bind_rows(summary_tbl, pooled_row), summary_file)
message("Country breakeven summary saved: ", summary_file)

# ── Text summary ─────────────────────────────────────────────────────────────

fmt_row <- function(r) {
  if (is.na(r$gbf_central)) {
    return(sprintf("%-10s no 2050 cost projection", r$country))
  }
  sprintf(paste0(
    "%-10s GBF fare +%.0f%%, eSAF fare +%.0f%% | projected eSAF premium %.0f pp ",
    "(pathways %.0f to %.0f; incl. scenarios %.0f to %.0f) | accepted premium ",
    "%.0f pp [%.0f, %.0f]\n           P(SAF) - P(GBF): at equal price %+.2f [%+.2f, %+.2f]; ",
    "at projected fares %+.2f [%+.2f, %+.2f] (pathways %+.2f to %+.2f)"),
    r$country, r$gbf_central, r$esaf_central, r$projected_central,
    r$projected_path_lo, r$projected_path_hi, r$projected_full_lo, r$projected_full_hi,
    r$accepted_premium, r$accepted_premium_lower, r$accepted_premium_upper,
    r$prob_gap_equal_price, r$prob_gap_equal_price_lower, r$prob_gap_equal_price_upper,
    r$prob_gap_at_projected, r$prob_gap_at_projected_lower, r$prob_gap_at_projected_upper,
    r$prob_gap_path_lo, r$prob_gap_path_hi)
}

saf_long  <- policy_labels$SAF_ETS$long
saf_short <- policy_labels$SAF_ETS$short
gbf_short <- policy_labels$GBF$short

txt <- c(
  sprintf("=== Country breakeven: %s vs %s ===", saf_long, gbf_short),
  "Model: binary_choice ~ policy_type * cost_pct * country + (1|id)",
  sprintf("Cost source: %s. Tested price range: %d-%d%%. CIs: %d parametric draws.",
          paste(unique(costs$source), collapse = ", "),
          tested_cost_range[1], tested_cost_range[2], n_draws),
  "",
  "Accepted premium = how much higher the eSAF fare increase can be (pp) before",
  sprintf("%s loses its preference advantage over %s priced at its projected fare.",
          saf_short, gbf_short),
  sprintf("%s is preferred at projected fares where projected premium > accepted premium.",
          gbf_short),
  sprintf("The premium is a ratio with the %s price slope in the denominator; where",
          saf_short),
  "that slope is close to zero its CI is very wide, so the probability gap at",
  "projected fares is the quantity plotted.",
  "",
  map_chr(seq_len(nrow(summary_tbl)), \(i) fmt_row(summary_tbl[i, ])),
  "",
  sprintf("Share of draws with non-negative %s price slope: %s", saf_short,
          paste(sprintf("%s %.3f", summary_tbl$country,
                        summary_tbl$share_saf_slope_nonneg), collapse = ", "))
)
writeLines(txt, txt_file)
message(paste(txt, collapse = "\n"))
