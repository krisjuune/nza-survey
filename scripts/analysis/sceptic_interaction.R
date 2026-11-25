library(dplyr)
library(tidyr)
library(readr)
library(lme4)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  conjoint_file   <- snakemake@input[["conjoint"]]
  profiles_file   <- snakemake@input[["profiles"]]
  covariates_file <- snakemake@input[["covariates"]]
  txt_out         <- snakemake@output[["txt"]]
} else {
  conjoint_file   <- here("data", "conjoint_long.csv")
  profiles_file   <- here("data", "lpa_conjoint_profiles.csv")
  covariates_file <- here("data", "covariates.csv")
  txt_out         <- here("output", "text-files", "sceptic_durability_activity.txt")
}

# Within each preference profile, does the preference for permanent vs
# temporary storage depend on the offsetting activity it comes with? In
# particular: do permanence sceptics reject "permanent" even with a
# nature-based offset? If the gap is the same for every activity, the
# durability label drives the choice, not the technology.

profile_names <- c(`1` = "Permanence sceptics", `2` = "Moderate demanders",
                   `3` = "High-integrity demanders")

conjoint <- read_csv(conjoint_file, show_col_types = FALSE,
                     col_types = cols(cost_code = col_character()))
profiles <- read_csv(profiles_file, show_col_types = FALSE) |> select(id, profile)
attentive <- read_csv(covariates_file, show_col_types = FALSE) |>
  transmute(id, attentive = attention_check %in% c(1, 99))

df <- conjoint |>
  inner_join(profiles, by = "id") |>
  inner_join(attentive, by = "id") |>
  mutate(activity = level_label("activity_code", activity_code))

# Design-based choice share of each activity x durability cell (attributes are
# fully randomised), with 95% CIs from a respondent bootstrap; also the
# permanent-minus-temporary gap within each activity.
cell_shares <- function(x, B = 1000, seed = random_seed) {
  per <- x |>
    group_by(id, activity, durability_code) |>
    summarise(y = sum(binary_choice), n = n(), .groups = "drop")
  share_of <- function(d) d |>
    group_by(activity, durability_code) |>
    summarise(share = sum(w * y) / sum(w * n), .groups = "drop")

  est <- share_of(mutate(per, w = 1))
  set.seed(seed)
  ids  <- unique(per$id)
  boot <- bind_rows(lapply(seq_len(B), function(b) {
    k <- count(tibble(id = sample(ids, replace = TRUE)), id, name = "w")
    share_of(inner_join(per, k, by = "id")) |> mutate(b = b)
  }))

  gap <- function(d) d |>
    pivot_wider(names_from = durability_code, values_from = share) |>
    mutate(gap = permanent - temporary)
  ci <- function(d, v, by) d |>
    group_by(across(all_of(by))) |>
    summarise(lower = quantile(.data[[v]], 0.025),
              upper = quantile(.data[[v]], 0.975), .groups = "drop")

  list(
    cells = left_join(est, ci(boot, "share", c("activity", "durability_code")),
                      by = c("activity", "durability_code")),
    gaps  = left_join(select(gap(est), activity, gap),
                      ci(gap(boot), "gap", "activity"), by = "activity")
  )
}

# Interaction test: logit with all attributes, activity x durability added,
# respondent random intercept; likelihood-ratio test of the interaction
interaction_test <- function(x) {
  m0 <- glmer(binary_choice ~ fuel_code + activity_code + durability_code +
                responsibility_code + cost_code + (1 | id),
              data = x, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
  m1 <- update(m0, . ~ . + activity_code:durability_code)
  a  <- anova(m0, m1)
  sprintf("activity x durability: LR chisq(%d) = %.2f, p = %.3g",
          a$Df[2], a$Chisq[2], a$`Pr(>Chisq)`[2])
}

pp  <- function(d) mutate(d, across(where(is.double), ~ round(100 * .x, 1)))
fmt <- function(d) capture.output(print(as.data.frame(d), row.names = FALSE))

out <- c(
  "=== Durability x offsetting activity within preference profiles ===",
  "",
  "Choice share (%) of profiles with each activity x durability combination",
  "(design-based; 95% CI from 1,000 respondent bootstrap draws), and the",
  "permanent-minus-temporary gap (pp) within each activity. Attentive =",
  "attention check 1 or 99.",
  ""
)

for (p in names(profile_names)) {
  for (subset_name in c("All respondents", "Attentive only")) {
    x <- df |> filter(profile == as.integer(p),
                      subset_name == "All respondents" | attentive)
    r <- cell_shares(x)
    out <- c(out,
             sprintf("--- %s: %s (n = %d) ---", profile_names[[p]], subset_name,
                     n_distinct(x$id)),
             if (p == "1") c("Choice share by cell:",
                             fmt(pp(arrange(r$cells, activity, durability_code)))),
             "Permanent minus temporary, by activity:",
             fmt(pp(r$gaps)),
             interaction_test(x),
             "")
  }
}

writeLines(out, txt_out)
message("Sceptic interaction summary written: ", txt_out)
