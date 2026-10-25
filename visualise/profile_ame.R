# Forest plot of average marginal effects (AMEs) on LPA profile membership,
# shared by the profiles main figure (significant predictors only) and the
# supplementary predictor figure (all predictors). Sourced after theme_clean.R.

profile_names <- c(
  "Profile 1" = "Permanence sceptics",
  "Profile 2" = "Moderate demanders",
  "Profile 3" = "High-integrity demanders"
)
profile_display_order <- c("High-integrity demanders", "Moderate demanders",
                           "Permanence sceptics")

# Row structure: one group per predictor, the reference level in the header.
# key = "variable|level" as written by scripts/analysis/profile_predictors.R
ame_rows <- tibble::tribble(
  ~group,                                         ~key,                                     ~label,
  "Country (vs Australia)",                       "country|Brazil",                         "Brazil",
  "Country (vs Australia)",                       "country|Germany",                        "Germany",
  "Country (vs Australia)",                       "country|Kenya",                          "Kenya",
  "Country (vs Australia)",                       "country|UAE",                            "UAE",
  "Country (vs Australia)",                       "country|Vietnam",                        "Vietnam",
  "Flying behaviour (vs never-flyer)",            "flyer_type|Infrequent/lapsed flyer",     "Infrequent/lapsed flyer",
  "Flying behaviour (vs never-flyer)",            "flyer_type|Occasional flyer",            "Occasional flyer",
  "Flying behaviour (vs never-flyer)",            "flyer_type|Frequent flyer",              "Frequent flyer",
  "Aviation climate concern (vs low)",            "concern_group|Mid",                      "Mid",
  "Aviation climate concern (vs low)",            "concern_group|High",                     "High",
  "Gender (vs female)",                           "gender|Male",                            "Male",
  "Income (vs low)",                              "income_cat|Middle",                      "Middle",
  "Income (vs low)",                              "income_cat|High",                        "High",
  "Education (vs secondary or below)",            "education_cat|Post-secondary",           "Post-secondary",
  "Education (vs secondary or below)",            "education_cat|Tertiary or above",        "Tertiary or above",
  "Age (per SD)",                                 "age|age",                                "Age",
  "Decision-making (per SD)",                     "publicinput_co2na|publicinput_co2na",    "Public input in decisions",
  "Responsibility ratings (per SD)",              "actor_role_airlines|actor_role_airlines", "Airlines",
  "Responsibility ratings (per SD)",              "actor_role_fuelsuppliers|actor_role_fuelsuppliers", "Fuel suppliers",
  "Responsibility ratings (per SD)",              "actor_role_government|actor_role_government", "Government",
  "Responsibility ratings (per SD)",              "actor_role_manufacturers|actor_role_manufacturers", "Manufacturers",
  "Responsibility ratings (per SD)",              "actor_role_nonprofit|actor_role_nonprofit", "Nonprofits",
  "Responsibility ratings (per SD)",              "actor_role_passengers|actor_role_passengers", "Passengers",
  "Responsibility ratings (per SD)",              "actor_role_researchers|actor_role_researchers", "Researchers"
)

read_profile_ame <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      key     = paste(variable, level, sep = "|"),
      profile = factor(profile_names[profile], levels = profile_display_order)
    )
}

# significant_only = TRUE keeps predictors with at least one profile whose
# 95% CI excludes zero (and drops groups left empty).
plot_profile_ame <- function(ame, significant_only = FALSE) {
  keep <- if (significant_only) {
    unique(ame$key[ame$significant])
  } else {
    unique(ame$key)
  }
  rows <- dplyr::filter(ame_rows, key %in% keep)

  # Header row per group, then its items; bold headers with no data or tick
  lv  <- unlist(lapply(unique(rows$group), function(g)
    c(g, rows$label[rows$group == g])))
  ids <- unlist(lapply(unique(rows$group), function(g)
    c(g, rows$key[rows$group == g])))
  pos <- setNames(make_row_positions(ids, unique(rows$group)), ids)
  axis_labels <- ifelse(ids %in% rows$group, paste0("<b>", lv, "</b>"), lv)

  d <- ame |>
    dplyr::filter(key %in% rows$key) |>
    dplyr::mutate(y = pos[key])

  ggplot(d, aes(x = ame, y = y)) +
    geom_vline(xintercept = 0, color = reference_line_color, linetype = "dashed",
               linewidth = 0.3) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = policy_color,
                   linewidth = paper_ci_width) +
    geom_point(color = policy_color, size = paper_point_size) +
    scale_y_continuous(breaks = pos, labels = axis_labels,
                       limits = c(min(pos) - 0.8, max(pos) + 0.8),
                       expand = expansion(add = 0)) +
    facet_wrap(~profile, nrow = 1) +
    theme_paper(legend = "none") +
    theme(axis.text.y  = ggtext::element_markdown(),
          axis.ticks.y = element_blank()) +
    labs(x = "Change in probability of profile membership (percentage points)",
         y = NULL)
}
