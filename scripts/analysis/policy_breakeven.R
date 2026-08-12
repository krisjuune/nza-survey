library(dplyr)
library(lme4)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  input_file        <- snakemake@input[["conjoint"]]
  out_file          <- snakemake@output[["results"]]
  grid_file         <- snakemake@output[["grid"]]
  interaction_file  <- snakemake@output[["interaction"]]
} else {
  input_file        <- here("data", "conjoint_long.csv")
  out_file          <- here("data", "policy_breakeven.csv")
  grid_file         <- here("data", "policy_breakeven_grid.csv")
  interaction_file  <- here("output", "supp_figs", "policy_breakeven_interaction.txt")
}

# Policy-type trials only; treat cost as continuous
df <- read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = cols(cost_code = col_character())
) |>
  filter(!is.na(policy_type)) |>
  mutate(
    cost_pct    = as.numeric(cost_code),
    policy_type = factor(policy_type, levels = c("GBF", "CORSIA", "SAF_ETS",
                                                  "VCM_SBTi", "VCM_status_quo"))
  )

# Model: preference ~ policy type + continuous price sensitivity
# GBF is the reference level so all policy_type coefficients are gaps vs GBF.
model <- suppressWarnings(
  glmer(
    binary_choice ~ policy_type + cost_pct + (1 | id),
    data    = df,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

# Fixed-effect coefficients + Wald CIs
coef_mat <- coef(summary(model))
ci_mat   <- confint(model, parm = "beta_", method = "Wald")

coef_tbl <- as.data.frame(coef_mat) |>
  tibble::rownames_to_column("term") |>
  rename(estimate = Estimate, se = `Std. Error`, z = `z value`, p_value = `Pr(>|z|)`) |>
  left_join(
    as.data.frame(ci_mat) |>
      tibble::rownames_to_column("term") |>
      rename(ci_lower = `2.5 %`, ci_upper = `97.5 %`),
    by = "term"
  )

# Price coefficient (log-odds per %-point ticket price increase; negative)
price_row  <- filter(coef_tbl, term == "cost_pct")
price_coef <- price_row$estimate

# For each policy type vs GBF:
#   breakeven_pct = preference_log_odds / abs(price_coef)
#   i.e. how many %-points cheaper GBF would need to be at equal policy popularity
results <- coef_tbl |>
  filter(grepl("^policy_type", term)) |>
  mutate(
    policy_type    = sub("^policy_type", "", term),
    breakeven_pct  = estimate / abs(price_coef),
    price_coef     = price_coef,
    price_ci_lower = price_row$ci_lower,
    price_ci_upper = price_row$ci_upper
  ) |>
  select(
    policy_type,
    log_odds_vs_gbf = estimate,
    lo_ci_lower     = ci_lower,
    lo_ci_upper     = ci_upper,
    p_value,
    price_coef,
    price_ci_lower,
    price_ci_upper,
    breakeven_pct
  )

write_csv(results, out_file)
message("Policy breakeven results written: ", out_file)

# ---- Gap-curve CI from main effects model ----
# Both pathways share the same price sensitivity in this model, so CI reflects
# uncertainty in the intercept gap (log-odds vs GBF) and price coefficient.
predict_gap_ci <- function(saf_prices, gbf_price) {
  n      <- length(saf_prices)
  new_s  <- tibble(policy_type = factor("SAF_ETS", levels = levels(df$policy_type)),
                   cost_pct    = saf_prices)
  new_g  <- tibble(policy_type = factor("GBF",     levels = levels(df$policy_type)),
                   cost_pct    = rep(gbf_price, n))
  Xs     <- model.matrix(~ policy_type + cost_pct, data = new_s)
  Xg     <- model.matrix(~ policy_type + cost_pct, data = new_g)
  eta_s  <- as.numeric(Xs %*% betas)
  eta_g  <- as.numeric(Xg %*% betas)
  ps     <- plogis(eta_s);  dps <- ps * (1 - ps)
  pg     <- plogis(eta_g);  dpg <- pg * (1 - pg)
  v_s    <- diag(Xs %*% vcov_mat %*% t(Xs))
  v_g    <- diag(Xg %*% vcov_mat %*% t(Xg))
  cov_sg <- rowSums((Xs %*% vcov_mat) * Xg)
  se_gap <- sqrt(pmax(dps^2 * v_s + dpg^2 * v_g - 2 * dps * dpg * cov_sg, 0))
  tibble(
    x          = saf_prices - gbf_price,
    prob       = ps - pg,
    prob_lower = ps - pg - 1.96 * se_gap,
    prob_upper = ps - pg + 1.96 * se_gap
  )
}

# ---- Interaction model: price sensitivity by policy type ----
model_int <- suppressWarnings(
  glmer(
    binary_choice ~ policy_type * cost_pct + (1 | id),
    data    = df,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
)

coef_int  <- coef(summary(model_int))
ci_int    <- confint(model_int, parm = "beta_", method = "Wald")

# Base (GBF) price sensitivity + per-policy-type deviations
base_cost   <- coef_int["cost_pct", "Estimate"]
policy_lvls <- c("GBF", "CORSIA", "SAF_ETS", "VCM_SBTi", "VCM_status_quo")

price_sens <- tibble(policy_type = policy_lvls) |>
  mutate(
    interaction_term = c(NA_character_, paste0("policy_type", policy_lvls[-1], ":cost_pct")),
    interaction_est  = c(0, sapply(interaction_term[-1], function(t) coef_int[t, "Estimate"])),
    interaction_p    = c(NA, sapply(interaction_term[-1], function(t) coef_int[t, "Pr(>|z|)"])),
    price_sensitivity = base_cost + interaction_est
  )

# Write text summary
txt_lines <- c(
  "=== Interaction model: price sensitivity by policy type ===",
  "Model: binary_choice ~ policy_type * cost_pct + (1|id)",
  "",
  "--- Full model summary ---",
  capture.output(print(coef_int, digits = 4)),
  "",
  "--- Price sensitivity per policy type (log-odds per %-pt ticket price increase) ---",
  capture.output(print(
    price_sens |>
      select(policy_type, price_sensitivity, interaction_est, interaction_p) |>
      as.data.frame(),
    digits = 4, row.names = FALSE
  )),
  "",
  paste0("Note: GBF base cost_pct coefficient = ", round(base_cost, 5),
         ". Interaction terms indicate deviation from GBF's price sensitivity."),
  "A positive interaction means that policy type is LESS price-sensitive than GBF.",
  paste0("SAF_ETS interaction p-value = ",
         round(price_sens$interaction_p[price_sens$policy_type == "SAF_ETS"], 4))
)
writeLines(txt_lines, interaction_file)
message("Interaction model summary written: ", interaction_file)

# ---- Prediction grids for visualisation ----
vcov_mat <- as.matrix(vcov(model))
betas    <- fixef(model)

predict_prob_ci <- function(policy_val, cost_vals) {
  new_df  <- tibble(
    policy_type = factor(policy_val, levels = levels(df$policy_type)),
    cost_pct    = cost_vals
  )
  X      <- model.matrix(~ policy_type + cost_pct, data = new_df)
  eta    <- as.numeric(X %*% betas)
  se_eta <- sqrt(diag(X %*% vcov_mat %*% t(X)))
  tibble(
    pathway    = policy_val,
    x          = cost_vals,
    prob       = plogis(eta),
    prob_lower = plogis(eta - 1.96 * se_eta),
    prob_upper = plogis(eta + 1.96 * se_eta)
  )
}

# Option A: GBF declining curve + SAF_ETS at three reference prices
gbf_curve <- predict_prob_ci("GBF", seq(0, 60, by = 0.5)) |>
  mutate(plot_type = "crossing", saf_ref = NA_real_)

saf_refs <- bind_rows(lapply(c(10, 30, 50), function(sp) {
  predict_prob_ci("SAF_ETS", sp) |>
    mutate(plot_type = "crossing", saf_ref = sp)
}))

# Option C: Preference gap curve with delta-method CI from interaction model
# GBF fixed at 10% (approximate lower bound); SAF price = 10 + differential
gbf_ref  <- 10
diff_seq <- seq(-20, 60, by = 0.5)
gap_curve <- predict_gap_ci(gbf_ref + diff_seq, gbf_ref) |>
  mutate(plot_type = "gap", pathway = "gap", saf_ref = NA_real_)

pred_grid <- bind_rows(gbf_curve, saf_refs, gap_curve)
write_csv(pred_grid, grid_file)
message("Prediction grid saved: ", grid_file)
message(
  sprintf(
    "Price coefficient: %.4f log-odds per %%-pt (95%% CI: %.4f to %.4f)",
    price_coef, price_row$ci_lower, price_row$ci_upper
  )
)
for (i in seq_len(nrow(results))) {
  message(sprintf(
    "  %s vs GBF: log-odds gap = %.3f, breakeven = %.1f %%-pts cheaper for GBF",
    results$policy_type[i], results$log_odds_vs_gbf[i], results$breakeven_pct[i]
  ))
}
