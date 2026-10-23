library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  covariates_file <- snakemake@input[["covariates"]]
  wtp_out         <- snakemake@output[["wtp"]]
  txt_out         <- snakemake@output[["txt"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  wtp_out         <- here("data", "wtp_country.csv")
  txt_out         <- here("output", "supp_figs", "wtp_summary.txt")
}

n_draws <- 5000

# Willingness to pay (WTP) in percentage points of ticket price increase,
# WTP_k = beta_k / -beta_price, from a conditional logit with price entered
# linearly. Each task is a forced choice between two profiles (a, b), so the
# conditional logit is exactly a logit of "a chosen" on the attribute
# differences x_a - x_b; the intercept is an alternative-specific constant for
# the left-hand profile (position bias). Attributes are effects-coded, so every
# level - including the omitted one - has its own part-worth relative to the
# average profile. SEs are clustered by respondent (6 tasks each).

conjoint <- read_csv(conjoint_file, show_col_types = FALSE,
                     col_types = cols(cost_code = col_character()))

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  select(id, country) |>
  mutate(country = recode(as.character(country), !!!country_recode))

non_price_attrs <- setdiff(attributes, "cost_code")
attr_levels <- lapply(attribute_display[non_price_attrs],
                      function(a) names(a$levels))

# Effects coding: for an attribute with levels l_1..l_L, column j (j < L) is
# 1 for l_j, -1 for l_L and 0 otherwise.
effects_code <- function(x, lv) {
  m <- sapply(lv[-length(lv)], function(l) (x == l) - (x == lv[length(lv)]))
  matrix(m, ncol = length(lv) - 1,
         dimnames = list(NULL, lv[-length(lv)]))
}

design <- function(d) {
  cols <- imap(attr_levels, function(lv, a) {
    m <- effects_code(d[[a]], lv)
    colnames(m) <- paste0(a, ":", colnames(m))
    m
  })
  do.call(cbind, cols)
}

# Price coding: linear (pp of ticket price) or categorical (effects-coded)
price_columns <- function(cost, linear) {
  if (linear) return(matrix(as.numeric(cost), dimnames = list(NULL, "price")))
  m <- effects_code(cost, c("10", "30", "50"))
  colnames(m) <- paste0("cost_code:", colnames(m))
  m
}

# One row per task: y = 1 if profile a chosen, X = x_a - x_b
build_pairs <- function(d, linear = TRUE) {
  d <- arrange(d, id, task, alternative)
  X <- cbind(design(d), price_columns(d$cost_code, linear))
  a <- d$alternative == "a"
  list(
    y  = d$binary_choice[a],
    X  = cbind(asc_a = 1, X[a, , drop = FALSE] - X[!a, , drop = FALSE]),
    id = d$id[a]
  )
}

# Logit fit with respondent-clustered (sandwich) covariance
fit_clogit <- function(p) {
  fit  <- glm.fit(p$X, p$y, family = binomial())
  beta <- fit$coefficients
  mu   <- as.numeric(plogis(p$X %*% beta))
  bread  <- solve(crossprod(p$X * sqrt(mu * (1 - mu))))
  scores <- rowsum(p$X * (p$y - mu), p$id)
  G <- nrow(scores)
  meat <- crossprod(scores) * G / (G - 1)
  list(beta = beta, vcov = bread %*% meat %*% bread, n_tasks = length(p$y),
       n_resp = G, loglik = -fit$deviance / 2)
}

# Part-worths for every level (omitted level = minus the sum of the others),
# for a vector or a draws matrix of coefficients.
part_worths <- function(b) {
  b <- rbind(b)
  out <- imap(attr_levels, function(lv, a) {
    est <- b[, paste0(a, ":", lv[-length(lv)]), drop = FALSE]
    est <- cbind(est, -rowSums(est))
    colnames(est) <- paste0(a, ":", lv)
    est
  })
  do.call(cbind, out)
}

summarise_wtp <- function(fit, label) {
  b <- fit$beta
  draws <- MASS::mvrnorm(n_draws, mu = b, Sigma = fit$vcov)
  price   <- b[["price"]]
  price_d <- draws[, "price"]

  wtp   <- part_worths(b) / -price
  wtp_d <- part_worths(draws) / -price_d

  # Pooled SAF (biofuels or synthetic fuels), matching the survey's SAF package
  saf   <- unname(rowMeans(wtp[, c("fuel_code:plants", "fuel_code:electric"),
                                 drop = FALSE]))
  saf_d <- rowMeans(wtp_d[, c("fuel_code:plants", "fuel_code:electric")])
  wtp   <- cbind(wtp, "fuel_code:saf" = saf)
  wtp_d <- cbind(wtp_d, "fuel_code:saf" = saf_d)

  # Premiums used in the text: SAF over fossil, permanent over temporary
  prem <- c("fuel_code:saf_vs_fossil" = saf - unname(wtp[, "fuel_code:fossil"]),
            "durability_code:permanent_vs_temporary" =
              unname(wtp[, "durability_code:permanent"] - wtp[, "durability_code:temporary"]))
  prem_d <- cbind(saf_d - wtp_d[, "fuel_code:fossil"],
                  wtp_d[, "durability_code:permanent"] - wtp_d[, "durability_code:temporary"])

  all_est <- c(wtp[1, ], prem)
  all_d   <- cbind(wtp_d, prem_d)
  tibble(
    country   = label,
    term      = names(all_est),
    wtp       = unname(all_est),
    lower     = apply(all_d, 2, quantile, 0.025),
    upper     = apply(all_d, 2, quantile, 0.975),
    price_coef = price,
    price_se   = sqrt(fit$vcov["price", "price"]),
    share_price_nonneg = mean(price_d >= 0),
    n_resp    = fit$n_resp
  ) |>
    separate(term, into = c("attribute", "code"), sep = ":")
}

# Linearity of price: is the +30% effect midway between +10% and +50%?
# With effects coding, beta_50 = -(beta_10 + beta_30), so the deviation from
# linearity is beta_30 - (beta_10 + beta_50) / 2 = 1.5 * beta_30.
linearity_test <- function(fit) {
  est <- 1.5 * fit$beta[["cost_code:30"]]
  se  <- 1.5 * sqrt(fit$vcov["cost_code:30", "cost_code:30"])
  c(deviation = est, se = se, p = 2 * pnorm(-abs(est / se)))
}

df <- conjoint |>
  inner_join(covariates, by = "id") |>
  filter(country %in% country_levels)

set.seed(random_seed)
groups <- c(list(Pooled = df), split(df, df$country)[country_levels])

wtp_tbl <- imap_dfr(groups, function(d, label) {
  message("Fitting conditional logit: ", label)
  summarise_wtp(fit_clogit(build_pairs(d, linear = TRUE)), label)
})

lin_tbl <- imap_dfr(groups, function(d, label) {
  t <- linearity_test(fit_clogit(build_pairs(d, linear = FALSE)))
  tibble(country = label, deviation = t[["deviation"]], se = t[["se"]], p = t[["p"]])
})

write_csv(wtp_tbl, wtp_out)
message("WTP estimates written: ", wtp_out)

# ── Text summary ─────────────────────────────────────────────────────────────

fmt <- function(x, lo, hi) sprintf("%+.1f [%+.1f, %+.1f]", x, lo, hi)

prem_lines <- wtp_tbl |>
  filter(code %in% c("saf_vs_fossil", "permanent_vs_temporary")) |>
  mutate(txt = fmt(wtp, lower, upper)) |>
  select(country, code, txt) |>
  pivot_wider(names_from = code, values_from = txt)

price_lines <- wtp_tbl |>
  distinct(country, price_coef, price_se, share_price_nonneg, n_resp)

txt <- c(
  "=== Willingness to pay (percentage points of ticket price increase) ===",
  "Conditional logit (logit on a - b attribute differences + left-alternative constant),",
  "effects-coded attributes, linear price, respondent-clustered SEs,",
  sprintf("95%% Krinsky-Robb intervals from %d draws. WTP_k = beta_k / -beta_price.", n_draws),
  "",
  "--- Price coefficient (log-odds per pp of ticket price) ---",
  capture.output(print(as.data.frame(price_lines), digits = 3, row.names = FALSE)),
  "",
  "--- Premiums (pp of ticket price): SAF over fossil; permanent over temporary ---",
  capture.output(print(as.data.frame(prem_lines), row.names = FALSE)),
  "",
  "--- Linearity of price (categorical-price model) ---",
  "Deviation = log-odds by which +30% departs from the straight line between +10% and +50%.",
  capture.output(print(as.data.frame(lin_tbl), digits = 3, row.names = FALSE))
)
writeLines(txt, txt_out)
message(paste(txt, collapse = "\n"))
