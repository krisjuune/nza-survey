library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  results_file <- snakemake@input[["results"]]
  plot_out     <- snakemake@output[["plot"]]
} else {
  results_file <- here("data", "profile_predictors.csv")
  plot_out     <- here("output", "supp_figs", "profile_predictors_plot.png")
}

comparison_recode <- c(
  "Profile 2 vs Profile 1" = "Moderate demanders vs Permanence sceptics",
  "Profile 3 vs Profile 1" = "High-integrity demanders vs Permanence sceptics",
  "Profile 3 vs Profile 2" = "High-integrity demanders vs Moderate demanders"
)
comparison_levels <- unname(comparison_recode)

# -------------------------------------------------------------------
# Y-axis structure: explicit row ordering, display labels, and flags.
# is_header = TRUE  → spacer label only, no geom plotted.
# is_ref    = TRUE  → open-circle point at OR = 1, no errorbar.
# For single-variable groups (Country, Flying, Concern) the ref row
# doubles as the group header (bold variable name + ref level).
# Demographics keeps a separate header since it spans multiple variables.
# -------------------------------------------------------------------
y_structure <- tribble(
  ~key,                      ~display,                                          ~is_header, ~is_ref,  ~group,
  "Australia",               "<b>Country</b> (Australia)",                      FALSE,      TRUE,     "Country",
  "Brazil",                  "  Brazil",                                        FALSE,      FALSE,    "Country",
  "Germany",                 "  Germany",                                       FALSE,      FALSE,    "Country",
  "Kenya",                   "  Kenya",                                         FALSE,      FALSE,    "Country",
  "Vietnam",                 "  Vietnam",                                       FALSE,      FALSE,    "Country",
  "UAE",                     "  UAE",                                           FALSE,      FALSE,    "Country",
  "Never-flyer",             "<b>Flying behaviour</b> (Never-flyer)",           FALSE,      TRUE,     "Flying behaviour",
  "Infrequent/lapsed flyer", "  Infrequent/lapsed flyer",                      FALSE,      FALSE,    "Flying behaviour",
  "Occasional flyer",        "  Occasional flyer",                              FALSE,      FALSE,    "Flying behaviour",
  "Frequent flyer",          "  Frequent flyer",                                FALSE,      FALSE,    "Flying behaviour",
  "Low concern",             "<b>Climate concern</b> (Low)",                    FALSE,      TRUE,     "Climate concern",
  "Mid concern",             "  Mid concern",                                   FALSE,      FALSE,    "Climate concern",
  "High concern",            "  High concern",                                  FALSE,      FALSE,    "Climate concern",
  "__Demographics",          "<b>Demographics</b>",                             TRUE,       FALSE,    "Demographics",
  "Female",                  "  <b>Gender</b> (Female)",                        FALSE,      TRUE,     "Demographics",
  "Male",                    "  Male",                                          FALSE,      FALSE,    "Demographics",
  "Low income",              "  <b>Income</b> (Low)",                           FALSE,      TRUE,     "Demographics",
  "Middle income",           "  Middle income",                                 FALSE,      FALSE,    "Demographics",
  "High income",             "  High income",                                   FALSE,      FALSE,    "Demographics",
  "Secondary or below",      "  <b>Education</b> (Secondary or below)",         FALSE,      TRUE,     "Demographics",
  "Post-secondary",          "  Post-secondary",                                FALSE,      FALSE,    "Demographics",
  "Tertiary or above",       "  Tertiary or above",                             FALSE,      FALSE,    "Demographics",
  "Age",                     "  Age (continuous)",                              FALSE,      FALSE,    "Demographics",
  "__Actors",                "<b>Actor responsibility</b> (continuous)",         TRUE,       FALSE,    "Actor responsibility",
  "Public input",            "  Public input in decisions",                     FALSE,      FALSE,    "Actor responsibility",
  "Airlines",                "  Airlines",                                      FALSE,      FALSE,    "Actor responsibility",
  "Fuel suppliers",          "  Fuel suppliers",                                FALSE,      FALSE,    "Actor responsibility",
  "Government",              "  Government",                                    FALSE,      FALSE,    "Actor responsibility",
  "Manufacturers",           "  Manufacturers",                                 FALSE,      FALSE,    "Actor responsibility",
  "Nonprofits",              "  Nonprofits",                                    FALSE,      FALSE,    "Actor responsibility",
  "Passengers",              "  Passengers",                                    FALSE,      FALSE,    "Actor responsibility",
  "Researchers",             "  Researchers",                                   FALSE,      FALSE,    "Actor responsibility"
)

# -------------------------------------------------------------------
# Load and label results
# -------------------------------------------------------------------
results <- read_csv(results_file, show_col_types = FALSE) |>
  mutate(
    variable = case_when(
      str_starts(term, "country")                  ~ "country",
      str_starts(term, "flyer_type")               ~ "flyer_type",
      str_starts(term, "concern_group")            ~ "concern_group",
      str_starts(term, "age")                      ~ "age",
      str_starts(term, "gender")                   ~ "gender",
      str_starts(term, "income_cat")               ~ "income_cat",
      str_starts(term, "education_cat")            ~ "education_cat",
      str_starts(term, "publicinput_co2na")        ~ "publicinput_co2na",
      str_starts(term, "actor_role_airlines")      ~ "actor_role_airlines",
      str_starts(term, "actor_role_fuelsuppliers") ~ "actor_role_fuelsuppliers",
      str_starts(term, "actor_role_government")    ~ "actor_role_government",
      str_starts(term, "actor_role_manufacturers") ~ "actor_role_manufacturers",
      str_starts(term, "actor_role_nonprofit")     ~ "actor_role_nonprofit",
      str_starts(term, "actor_role_passengers")    ~ "actor_role_passengers",
      str_starts(term, "actor_role_researchers")   ~ "actor_role_researchers",
      TRUE                                         ~ term
    ),
    level = str_remove(term, paste0(
      "^(country|flyer_type|concern_group|age|gender|income_cat|education_cat|",
      "publicinput_co2na|actor_role_airlines|actor_role_fuelsuppliers|",
      "actor_role_government|actor_role_manufacturers|actor_role_nonprofit|",
      "actor_role_passengers|actor_role_researchers)"
    )),
    level = if_else(level == "", variable, level),
    key = case_when(
      variable == "age"                      ~ "Age",
      variable == "gender"                   ~ level,
      variable == "income_cat"               ~ paste(level, "income"),
      variable == "education_cat"            ~ level,
      variable == "concern_group"            ~ paste(level, "concern"),
      variable == "flyer_type"               ~ level,
      variable == "country"                  ~ level,
      variable == "publicinput_co2na"        ~ "Public input",
      variable == "actor_role_airlines"      ~ "Airlines",
      variable == "actor_role_fuelsuppliers" ~ "Fuel suppliers",
      variable == "actor_role_government"    ~ "Government",
      variable == "actor_role_manufacturers" ~ "Manufacturers",
      variable == "actor_role_nonprofit"     ~ "Nonprofits",
      variable == "actor_role_passengers"    ~ "Passengers",
      variable == "actor_role_researchers"   ~ "Researchers",
      TRUE                                   ~ level
    ),
    comparison = recode(comparison, !!!comparison_recode),
    comparison = factor(comparison, levels = comparison_levels),
    is_header  = FALSE,
    is_ref     = FALSE
  )

# -------------------------------------------------------------------
# Reference rows (OR = 1.0, no CI), one per factor ref level × comparison
# -------------------------------------------------------------------
ref_rows <- crossing(
  y_structure |> filter(is_ref) |> select(key, is_header, is_ref, group),
  comparison = factor(comparison_levels, levels = comparison_levels)
) |>
  mutate(or = 1.0, lower = NA_real_, upper = NA_real_)

# -------------------------------------------------------------------
# Header rows (no point plotted — included in data so factor levels are retained)
# -------------------------------------------------------------------
header_rows <- crossing(
  y_structure |> filter(is_header) |> select(key, is_header, is_ref, group),
  comparison = factor(comparison_levels, levels = comparison_levels)
) |>
  mutate(or = NA_real_, lower = NA_real_, upper = NA_real_)

# -------------------------------------------------------------------
# Combine and attach display labels, set y factor
# -------------------------------------------------------------------
group_levels <- unique(y_structure$group[!y_structure$is_header])

group_colors <- setNames(
  unname(attribute_colors)[seq_along(group_levels)],
  group_levels
)

plot_df <- bind_rows(
  select(results,     comparison, key, or, lower, upper, is_header, is_ref),
  select(ref_rows,    comparison, key, or, lower, upper, is_header, is_ref),
  select(header_rows, comparison, key, or, lower, upper, is_header, is_ref)
) |>
  left_join(select(y_structure, key, display, group), by = "key") |>
  mutate(
    key      = factor(key, levels = rev(y_structure$key)),
    group    = factor(group, levels = group_levels),
    pt_shape = 16L
  )

# -------------------------------------------------------------------
# Plot — all rows passed to every geom (na.rm = TRUE drops header/CI
# rows with NA values), so all 32 factor levels train the x scale.
# -------------------------------------------------------------------
p <- ggplot(plot_df, aes(x = key, y = or, color = group)) +
  geom_hline(yintercept = 1, color = "grey50", linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, na.rm = TRUE) +
  geom_point(aes(shape = pt_shape), size = point_size, na.rm = TRUE) +
  scale_x_discrete(labels = setNames(y_structure$display, y_structure$key)) +
  scale_shape_identity(guide = "none") +
  scale_color_manual(values = group_colors, name = NULL) +
  scale_y_log10() +
  facet_wrap(~comparison, nrow = 1) +
  coord_flip() +
  theme_clean(base_size = 11) +
  theme(
    axis.text.y     = ggtext::element_markdown(),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = "Odds ratio (log scale)")

ggsave(plot_out, p,
       width  = plot_size$wide$width,
       height = plot_size$wide$height + 4)
message("Profile predictor plot saved: ", plot_out)
