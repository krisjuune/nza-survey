library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(here)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "conjoint_models.R"))

if (exists("snakemake")) {
  conjoint_file       <- snakemake@input[["conjoint"]]
  covariates_file     <- snakemake@input[["covariates"]]
  country_choice_file <- snakemake@input[["country_choice"]]
  country_rating_file <- snakemake@input[["country_rating"]]
  overall_choice_file <- snakemake@input[["overall_choice"]]
  overall_rating_file <- snakemake@input[["overall_rating"]]
  policy_choice_file  <- snakemake@input[["policy_choice"]]
  policy_rating_file  <- snakemake@input[["policy_rating"]]
  variants_out        <- snakemake@output[["variants"]]
  validation_out      <- snakemake@output[["validation"]]
  txt_out             <- snakemake@output[["txt"]]
} else {
  conjoint_file       <- here("data", "conjoint_long.csv")
  covariates_file     <- here("data", "covariates.csv")
  country_choice_file <- here("data", "country_choice_emm.csv")
  country_rating_file <- here("data", "country_rating_emm.csv")
  overall_choice_file <- here("data", "overall_choice_emm.csv")
  overall_rating_file <- here("data", "overall_rating_emm.csv")
  policy_choice_file  <- here("data", "policy_choice_emm.csv")
  policy_rating_file  <- here("data", "policy_rating_emm.csv")
  variants_out        <- here("data", "robustness_country_choice.csv")
  validation_out      <- here("data", "robustness_validation.csv")
  txt_out             <- here("output", "supp_figs", "robustness_summary.txt")
}

speeder_share  <- 0.05
attention_pass <- c(1, 99)   # 1 = correct answer, 99 = honest "I don't know"
n_boot         <- 500

ts_msg <- function(...) message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)

covariates <- read_csv(covariates_file, show_col_types = FALSE) |>
  mutate(country = factor(recode(as.character(country), !!!country_recode),
                          levels = country_levels))

df <- read_csv(conjoint_file, show_col_types = FALSE,
               col_types = cols(cost_code = col_character())) |>
  inner_join(select(covariates, id, country), by = "id")

resp <- covariates |> filter(id %in% df$id)

# ── 1. Sample variants: same glmer model as the main country choice figure ───

# Fastest 5% within each country (durations differ strongly between countries)
speeders <- resp |>
  group_by(country) |>
  filter(duration_min < quantile(duration_min, speeder_share)) |>
  pull(id)
inattentive <- resp |> filter(!attention_check %in% attention_pass) |> pull(id)

sample_variants <- list(
  no_speeders      = setdiff(unique(df$id), speeders),
  attention_passed = setdiff(unique(df$id), inattentive)
)

variant_mm <- imap_dfr(sample_variants, function(keep, name) {
  ts_msg("Variant ", name, ": ", length(keep), " respondents")
  fit_country_choice(filter(df, id %in% keep), log = ts_msg) |> mutate(variant = name)
})

full_mm <- read_csv(country_choice_file, show_col_types = FALSE) |>
  mutate(variant = "full")

# ── 2. Income weighting (design-based marginal means) ────────────────────────

# Self-placed national income deciles: the population share of each decile is
# 10% by definition. Weight = 0.10 / sample share within country; respondents
# who did not report income (99) keep weight 1; weights normalised to mean 1.
weights <- resp |>
  mutate(decile = if_else(income_decile %in% 1:10, income_decile, NA_real_)) |>
  add_count(country, decile, name = "n_decile") |>
  group_by(country) |>
  mutate(share = n_decile / sum(!is.na(decile)),
         w = if_else(is.na(decile), 1, 0.10 / share),
         w = w / mean(w)) |>
  ungroup() |>
  select(id, country, decile, w)

weight_diag <- weights |>
  group_by(country) |>
  summarise(n = n(), w_min = min(w), w_max = max(w),
            n_eff = sum(w)^2 / sum(w^2), .groups = "drop")

weight_check <- weights |>
  filter(!is.na(decile)) |>
  group_by(country, decile) |>
  summarise(w = sum(w), .groups = "drop_last") |>
  mutate(share = w / sum(w)) |>
  summarise(max_dev_from_10pct = max(abs(share - 0.10)), .groups = "drop")

dfw <- df |> inner_join(select(weights, id, w), by = "id")

ts_msg("Design-based marginal means (unweighted, weighted)...")
design_mm_tbl <- bind_rows(
  design_mm(dfw, weight = NULL, B = n_boot, seed = random_seed) |>
    mutate(variant = "design_unweighted"),
  design_mm(dfw, weight = "w", B = n_boot, seed = random_seed) |>
    mutate(variant = "design_weighted")
)

variants <- bind_rows(full_mm, variant_mm, design_mm_tbl) |>
  mutate(country = as.character(country), code = as.character(code)) |>
  select(variant, country, attribute, code, prob, asymp.LCL, asymp.UCL)

write_csv(variants, variants_out)

compare_variants <- function(a, b) {
  x <- inner_join(filter(variants, variant == a), filter(variants, variant == b),
                  by = c("country", "attribute", "code"))
  side <- function(lo, hi) ifelse(lo > 0.5, 1, ifelse(hi < 0.5, -1, 0))
  tibble(
    comparison   = paste(b, "vs", a),
    max_abs_diff_pp  = 100 * max(abs(x$prob.y - x$prob.x)),
    mean_abs_diff_pp = 100 * mean(abs(x$prob.y - x$prob.x)),
    correlation  = cor(x$prob.x, x$prob.y),
    levels_changing_significance = sum(side(x$asymp.LCL.x, x$asymp.UCL.x) !=
                                         side(x$asymp.LCL.y, x$asymp.UCL.y)),
    n_levels     = nrow(x)
  )
}

variant_summary <- bind_rows(
  compare_variants("full", "no_speeders"),
  compare_variants("full", "attention_passed"),
  compare_variants("full", "design_unweighted"),
  compare_variants("design_unweighted", "design_weighted")
)

# ── 3. Internal validation (i): conjoint ratings vs choices ──────────────────

# Policy packages: rank by choice probability vs by mean support rating
pkg <- df |> filter(!is.na(policy_type))
pkg_means <- bind_rows(
  pkg |> mutate(country = "Pooled"),
  pkg |> mutate(country = as.character(country))
) |>
  group_by(country, policy_type) |>
  summarise(choice = mean(binary_choice), rating = mean(support, na.rm = TRUE),
            n_profiles = n(), .groups = "drop")

pkg_rank <- pkg_means |>
  group_by(country) |>
  summarise(spearman_rho = cor(choice, rating, method = "spearman"),
            same_order = identical(policy_type[order(-choice)],
                                   policy_type[order(-rating)]),
            choice_order = paste(policy_type[order(-choice)], collapse = " > "),
            rating_order = paste(policy_type[order(-rating)], collapse = " > "),
            .groups = "drop")

# Pooled model-based package estimates (the policy choice figure) for reference
pkg_model <- read_csv(policy_choice_file, show_col_types = FALSE) |>
  select(policy_type, choice = prob) |>
  inner_join(read_csv(policy_rating_file, show_col_types = FALSE) |>
               select(policy_type, rating = emmean), by = "policy_type")
pkg_model_rho <- cor(pkg_model$choice, pkg_model$rating, method = "spearman")

# Attribute levels: correlation of rating and choice marginal means
level_cor <- bind_rows(
  inner_join(read_csv(overall_choice_file, show_col_types = FALSE),
             read_csv(overall_rating_file, show_col_types = FALSE),
             by = c("attribute", "code")) |> mutate(country = "Pooled"),
  inner_join(read_csv(country_choice_file, show_col_types = FALSE),
             read_csv(country_rating_file, show_col_types = FALSE),
             by = c("country", "attribute", "code"))
) |>
  group_by(country) |>
  summarise(pearson_r = cor(prob, emmean), n_levels = n(), .groups = "drop")

# ── 4. Internal validation (ii): stated pathway ratings vs conjoint choices ──

# Each stated pathway rating (1-5, asked separately from the conjoint) is
# matched to the conjoint level it describes. Validity check: respondents who
# rate the pathway favourably (4-5) should choose profiles with the matching
# level more often than respondents who do not (1-3).
stated_items <- tribble(
  ~item,                   ~attribute,        ~code,
  "pref_path_safs",        "fuel_code",       "plants",
  "pref_path_synfuels",    "fuel_code",       "electric",
  "pref_path_geol_offset", "durability_code", "permanent",
  "pref_path_trad_offset", "activity_code",   "cookstoves"
)

stated_diff <- pmap_dfr(stated_items, function(item, attribute, code) {
  grp <- resp |>
    transmute(id, favourable = case_when(.data[[item]] %in% 4:5 ~ TRUE,
                                         .data[[item]] %in% 1:3 ~ FALSE))
  # per respondent: choices and number of profiles with the matching level
  per_resp <- df |>
    filter(.data[[attribute]] == code) |>
    group_by(id, country) |>
    summarise(y = sum(binary_choice), n = n(), .groups = "drop") |>
    inner_join(grp, by = "id") |>
    filter(!is.na(favourable))

  one <- function(d, label) {
    diff_of <- function(k) {
      f <- d$favourable
      sum(k * d$y * f) / sum(k * d$n * f) - sum(k * d$y * !f) / sum(k * d$n * !f)
    }
    est <- diff_of(rep(1, nrow(d)))
    # bootstrap respondents (within country for the pooled estimate)
    boot <- replicate(n_boot, {
      idx <- unlist(lapply(split(seq_len(nrow(d)), d$country),
                           function(i) sample(i, length(i), replace = TRUE)))
      diff_of(tabulate(idx, nbins = nrow(d)))
    })
    tibble(item = item, attribute = attribute, code = code, country = label,
           diff = 100 * est, lower = 100 * quantile(boot, 0.025),
           upper = 100 * quantile(boot, 0.975),
           n_favourable = sum(d$favourable), n_other = sum(!d$favourable))
  }
  set.seed(random_seed)
  bind_rows(one(per_resp, "Pooled"),
            map_dfr(country_levels, \(c) one(filter(per_resp, country == c), c)))
})

# ── 5. Left-right position bias ──────────────────────────────────────────────

position <- bind_rows(
  df |> mutate(country = "Pooled"),
  df |> mutate(country = as.character(country))
) |>
  filter(alternative == "a") |>
  group_by(country) |>
  summarise(k = sum(binary_choice), n = n(), .groups = "drop") |>
  rowwise() |>
  mutate(share_left = k / n,
         lower = binom.test(k, n)$conf.int[1],
         upper = binom.test(k, n)$conf.int[2]) |>
  ungroup()

# ── Outputs ──────────────────────────────────────────────────────────────────

validation <- bind_rows(
  stated_diff |> transmute(check = "stated_vs_choice", country, item, attribute,
                           code, estimate = diff, lower, upper),
  pkg_rank |> transmute(check = "package_rank_rating_vs_choice", country,
                        estimate = spearman_rho),
  pkg_means |> transmute(check = "package_choice", country, item = policy_type,
                         estimate = choice),
  pkg_means |> transmute(check = "package_rating", country, item = policy_type,
                         estimate = rating),
  level_cor |> transmute(check = "level_mm_rating_vs_choice", country,
                         estimate = pearson_r),
  position |> transmute(check = "position_left_share", country,
                        estimate = share_left, lower, upper)
)
write_csv(validation, validation_out)

show <- function(x, digits = 3) capture.output(print(as.data.frame(x), digits = digits,
                                                       row.names = FALSE))
txt <- c(
  "=== Robustness checks: country choice conjoint ===",
  "",
  "--- Samples ---",
  sprintf("Full sample: %d respondents", n_distinct(df$id)),
  sprintf("Excluding fastest %d%% per country: %d removed (%d kept)",
          100 * speeder_share, length(speeders), length(sample_variants$no_speeders)),
  sprintf("Attention-check passers (answer 1 or 99): %d removed (%d kept)",
          length(inattentive), length(sample_variants$attention_passed)),
  "",
  "--- Income weights (uniform national deciles within country) ---",
  show(weight_diag),
  "Max deviation of weighted decile shares from 10%:",
  show(weight_check, 2),
  "",
  "--- Marginal means: variant vs reference (pp of choice probability) ---",
  "full = glmer on full sample (main figure); design_* = design-based means.",
  "levels_changing_significance = levels whose 95% CI moves to/from the other",
  "side of 0.5 (out of 96 country x level estimates).",
  show(variant_summary),
  "",
  "--- Internal validation (i): conjoint ratings vs choices ---",
  sprintf("Policy packages, pooled model estimates (policy choice figure): Spearman rho = %.2f",
          pkg_model_rho),
  "Policy packages, design-based means by country:",
  show(pkg_rank |> select(country, spearman_rho, same_order)),
  "Orders (highest first):",
  show(pkg_rank |> select(country, choice_order, rating_order)),
  "Attribute-level marginal means, rating vs choice (Pearson r):",
  show(level_cor),
  "",
  "--- Internal validation (ii): stated pathway ratings vs conjoint choices ---",
  "Difference in choice probability of the matching level (pp), respondents rating the",
  "pathway favourably (4-5) minus others (1-3); positive = consistent.",
  show(stated_diff |> select(item, code, country, diff, lower, upper, n_favourable, n_other), 2),
  "",
  "--- Left-right position bias: share of tasks where the left profile was chosen ---",
  show(position |> select(country, share_left, lower, upper, n))
)
writeLines(txt, txt_out)
message(paste(txt, collapse = "\n"))
