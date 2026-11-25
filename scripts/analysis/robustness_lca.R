library(dplyr)
library(tidyr)
library(readr)
library(here)
library(mclust)
library(nnet)

source(here("scripts", "shared", "constants.R"))
source(here("scripts", "shared", "lpa_models.R"))
source(here("scripts", "shared", "conjoint_models.R"))
source(here("scripts", "shared", "profile_models.R"))

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  covariates_file <- snakemake@input[["covariates"]]
  groups_file     <- snakemake@input[["groups"]]
  raw_file        <- snakemake@input[["raw"]]
  full_ic_file    <- snakemake@input[["full_ic"]]
  full_prof_file  <- snakemake@input[["full_profiles"]]
  profiles_out    <- snakemake@output[["profiles"]]
  policy_out      <- snakemake@output[["policy"]]
  ame_out         <- snakemake@output[["ame"]]
  txt_out         <- snakemake@output[["txt"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  covariates_file <- here("data", "covariates.csv")
  groups_file     <- here("data", "respondent_groups.csv")
  raw_file        <- here("raw-data", "raw_data.csv")
  full_ic_file    <- here("data", "lpa_conjoint_ic.csv")
  full_prof_file  <- here("data", "lpa_conjoint_profiles.csv")
  profiles_out    <- here("data", "robustness_lca_profiles.csv")
  policy_out      <- here("data", "robustness_lca_policy.csv")
  ame_out         <- here("data", "robustness_lca_ame.csv")
  txt_out         <- here("output", "text-files", "robustness_lca.txt")
}

ts_msg <- function(...) message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)

# Robustness of the preference profiles to inattentive respondents: the whole
# profile pipeline (preference scores -> LPA model selection -> 3-profile
# solution -> policy choice by profile -> predictors of membership) is re-run
# on respondents who passed the attention check (1 = correct, 99 = honest
# "I don't know"; as in scripts/analysis/robustness.R).

attention_pass <- c(1, 99)

conjoint <- read_csv(conjoint_file, show_col_types = FALSE,
                     col_types = cols(cost_code = col_character()))
attention <- read_csv(covariates_file, show_col_types = FALSE) |>
  transmute(id, attention_fail = as.numeric(!attention_check %in% attention_pass))
attentive_ids <- attention$id[attention$attention_fail == 0]
conjoint_att  <- filter(conjoint, id %in% attentive_ids)
ts_msg("Attentive respondents: ", length(attentive_ids), " of ", nrow(attention))

# ── 1. LPA on attentive respondents ──────────────────────────────────────────

scores <- lpa_preference_scores(conjoint_att)
X      <- lpa_matrix(scores$wide)

ts_msg("Fitting LPA models for G = 1 to 8 (attentive sample)...")
ic_att  <- lpa_information_criteria(X, max_g = 8)
ic_full <- read_csv(full_ic_file, show_col_types = FALSE)
best_g  <- function(ic) sapply(c("AIC", "BIC", "SABIC", "AWE", "ICL"),
                               function(cr) ic$G[which.max(ic[[cr]])])

fit3 <- lpa_fit_profiles(X, scores$wide$id, G = 3)

# Profile numbers are arbitrary: relabel so each attentive profile carries the
# number of the full-sample profile it overlaps most with (best permutation).
full_prof <- read_csv(full_prof_file, show_col_types = FALSE) |> select(id, profile)
overlap <- fit3$profiles |>
  select(id, profile_att = profile) |>
  inner_join(full_prof, by = "id") |>
  with(table(profile_att, profile))
perms <- list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
best  <- perms[[which.max(sapply(perms, function(p) sum(overlap[cbind(1:3, p)])))]]
relabel <- setNames(best, rownames(overlap))

att_profiles <- fit3$profiles |>
  mutate(profile = relabel[as.character(profile)])
# posterior columns follow the relabelled profile numbers
names(att_profiles)[match(paste0("prob_", 1:3), names(att_profiles))] <-
  paste0("prob_", relabel[as.character(1:3)])
att_profiles <- select(att_profiles, id, profile, prob_1, prob_2, prob_3)
write_csv(att_profiles, profiles_out)

agreement <- full_prof |>
  inner_join(select(att_profiles, id, profile_att = profile), by = "id")
pct_same <- 100 * mean(agreement$profile == agreement$profile_att)
ari      <- adjustedRandIndex(agreement$profile, agreement$profile_att)

# Profile means: full vs attentive solution, correlation across attribute levels
means_att  <- lpa_profile_means(scores$wide, att_profiles$profile[match(scores$wide$id, att_profiles$id)],
                                scores$map)
scores_all <- lpa_preference_scores(conjoint)
means_full <- lpa_profile_means(scores_all$wide,
                                full_prof$profile[match(scores_all$wide$id, full_prof$id)],
                                scores_all$map)
means_r <- inner_join(means_full, means_att, by = c("profile", "attr_level"),
                      suffix = c("_full", "_att")) |>
  group_by(profile) |>
  summarise(r = cor(mean_pref_full, mean_pref_att),
            max_abs_diff = max(abs(mean_pref_full - mean_pref_att)), .groups = "drop")

# ── 2. Policy choice by profile (attentive) ──────────────────────────────────

ts_msg("Fitting policy x profile model (attentive sample)...")
policy_att <- fit_policy_by_profile(
  conjoint_att,
  att_profiles |> transmute(id, profile = factor(paste("Profile", profile),
                                                 levels = paste("Profile", 1:3))),
  log = ts_msg
)
write_csv(policy_att, policy_out)

# ── 3. Predictors of profile membership (attentive) ──────────────────────────

ts_msg("Profile membership AMEs (attentive sample)...")
df_att <- read_profile_data(profiles_out, groups_file, covariates_file, raw_file)
ame_att <- profile_ame(profile_formula, df_att, variables = profile_ame_variables,
                       n_draws = 2000)
write_csv(ame_att, ame_out)

# ── 4. Attention-check failure as a predictor (full sample, full model) ─────

df_full <- read_profile_data(full_prof_file, groups_file, covariates_file, raw_file) |>
  inner_join(attention, by = "id") |>
  mutate(attention = factor(if_else(attention_fail == 1, "Failed", "Passed"),
                            levels = c("Passed", "Failed")))
m0 <- multinom(profile_formula, data = df_full, trace = FALSE, maxit = 500)
m1 <- multinom(update(profile_formula, . ~ . + attention), data = df_full,
               trace = FALSE, maxit = 500)
lr_stat <- as.numeric(2 * (logLik(m1) - logLik(m0)))
lr_df   <- m1$edf - m0$edf
ame_attention <- profile_ame(update(profile_formula, . ~ . + attention), df_full,
                             variables = "attention", n_draws = 2000)

full_post <- read_csv(full_prof_file, show_col_types = FALSE) |>
  mutate(max_prob = pmax(prob_1, prob_2, prob_3)) |>
  inner_join(attention, by = "id")

# ── Text summary ─────────────────────────────────────────────────────────────

profile_names <- c(`1` = "Permanence sceptics", `2` = "Moderate demanders",
                   `3` = "High-integrity demanders")
shares <- bind_rows(
  count(full_prof, profile) |> mutate(sample = "Full"),
  count(att_profiles, profile) |> mutate(sample = "Attentive")
) |>
  group_by(sample) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup() |>
  mutate(profile = profile_names[as.character(profile)]) |>
  select(sample, profile, n, pct)

fmt <- function(x) capture.output(print(as.data.frame(x), row.names = FALSE))

txt <- c(
  "=== Robustness of the preference profiles to inattentive respondents ===",
  "",
  sprintf("Attentive sample (attention check 1 or 99): %d of %d respondents.",
          length(attentive_ids), nrow(attention)),
  "The full profile pipeline was re-run on the attentive sample: preference",
  "scores, LPA model selection (G = 1-8), 3-profile solution, policy choice by",
  "profile (glmer) and predictors of membership (multinomial AMEs, 2,000 draws).",
  "",
  "--- Model selection: number of profiles preferred by each criterion ---",
  fmt(rbind(Full = best_g(ic_full), Attentive = best_g(ic_att))),
  "",
  "--- Profile sizes ---",
  fmt(shares),
  "",
  sprintf("Same profile in both solutions: %.1f%% of attentive respondents; adjusted Rand index %.3f.",
          pct_same, ari),
  "Profile means (mean preference score per attribute level), full vs attentive:",
  fmt(mutate(means_r, profile = profile_names[as.character(profile)],
             across(c(r, max_abs_diff), ~ round(.x, 3)))),
  "",
  "--- Attention-check failure as a predictor of membership (full sample) ---",
  sprintf("Added to the main profile model: LR chisq(%d) = %.1f, p = %.2g.",
          lr_df, lr_stat, pchisq(lr_stat, lr_df, lower.tail = FALSE)),
  "AME of failing vs passing (pp):",
  fmt(ame_attention |>
        transmute(profile = profile_names[sub("Profile ", "", profile)],
                  across(c(ame, lower, upper), ~ round(.x, 2)), significant)),
  sprintf("Mean highest posterior membership probability: passed %.3f, failed %.3f.",
          mean(full_post$max_prob[full_post$attention_fail == 0]),
          mean(full_post$max_prob[full_post$attention_fail == 1]))
)
writeLines(txt, txt_out)
ts_msg("Robustness LCA outputs written: ", txt_out)
