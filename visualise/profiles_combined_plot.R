library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file  <- snakemake@input[["choice"]]
  results_file <- snakemake@input[["results"]]
  plot_out     <- snakemake@output[["plot"]]
} else {
  choice_file  <- here("data", "policy_by_profile_choice.csv")
  results_file <- here("data", "profile_predictors.csv")
  plot_out     <- here("output", "profiles_combined_plot.png")
}

profile_names <- c(
  "Profile 1" = "Permanence sceptics",
  "Profile 2" = "Moderate demanders",
  "Profile 3" = "High-integrity demanders"
)
profile_levels <- c("High-integrity demanders", "Moderate demanders", "Permanence sceptics")

comparison_recode <- c(
  "Profile 3 vs Profile 1" = "High-integrity vs\nPermanence sceptics",
  "Profile 3 vs Profile 2" = "Moderate vs\nHigh-integrity demanders",
  "Profile 2 vs Profile 1" = "Permanence sceptics vs\nModerate demanders"
)
comparison_levels <- unname(comparison_recode)
flipped_comparisons <- c(
  "Moderate vs\nHigh-integrity demanders",
  "Permanence sceptics vs\nModerate demanders"
)

# -------------------------------------------------------------------
# Panel a: policy preference by profile
# -------------------------------------------------------------------
policy_labels <- c(
  "GBF"            = "Supplier-mandated\ncarbon removal (GBF)",
  "CORSIA"         = "Mandatory airline\noffsetting (CORSIA)",
  "SAF_ETS"        = "Regulated clean fuel\ntransition (SAF + ETS)",
  "VCM_SBTi"       = "Voluntary passenger-funded\ncarbon removal (SBTi)",
  "VCM_status_quo" = "Voluntary passenger-funded\noffsetting (status quo)"
)
policy_order  <- c("SAF_ETS", "GBF", "CORSIA", "VCM_SBTi", "VCM_status_quo")
level_labels  <- policy_labels[rev(policy_order)]

raw_choice <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(
    profile = recode(profile, !!!profile_names),
    profile = factor(profile, levels = profile_levels)
  )

profile_label_map <- raw_choice |>
  distinct(profile, n_profile) |>
  arrange(profile) |>
  mutate(profile_label = paste0(profile, "\n(n = ", n_profile, ")"))

choice_df <- raw_choice |>
  left_join(select(profile_label_map, profile, profile_label), by = "profile") |>
  mutate(
    policy_label  = factor(policy_labels[policy_type], levels = level_labels),
    profile_label = factor(profile_label, levels = profile_label_map$profile_label)
  )

panel_a <- ggplot(choice_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_point(size = point_size + 1, color = policy_color) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = errorbar_width, color = policy_color
  ) +
  facet_wrap(~profile_label, nrow = 1) +
  coord_flip() +
  theme_clean(base_size = 11) +
  labs(x = NULL, y = "Choice probability")

# -------------------------------------------------------------------
# Panel b: profile predictor heatmap
# -------------------------------------------------------------------
y_structure <- tribble(
  ~key,              ~display,
  "Brazil",          "Country: Brazil",
  "Germany",         "Country: Germany",
  "Kenya",           "Country: Kenya",
  "Vietnam",         "Country: Vietnam",
  "UAE",             "Country: UAE",
  "Mid concern",     "Climate concern: Mid",
  "High concern",    "Climate concern: High",
  "Male",            "Gender: Male",
  "Middle income",   "Income: Middle",
  "High income",     "Income: High",
  "Post-secondary",  "Education: Post-secondary",
  "Tertiary or above", "Education: Tertiary or above",
  "Public input",    "Decision-making: Public input in decisions",
  "Airlines",        "Responsible actor: Airlines",
  "Fuel suppliers",  "Responsible actor: Fuel suppliers",
  "Government",      "Responsible actor: Government",
  "Manufacturers",   "Responsible actor: Manufacturers",
  "Nonprofits",      "Responsible actor: Nonprofits",
  "Passengers",      "Responsible actor: Passengers",
  "Researchers",     "Responsible actor: Researchers"
)

# Key → display label for scale
key_labels <- setNames(y_structure$display, y_structure$key)
y_levels   <- rev(y_structure$key)

# Load and key-map results
results_hm <- read_csv(results_file, show_col_types = FALSE) |>
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
    level      = if_else(level == "", variable, level),
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
    sig        = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    fill_val   = if_else(p_value < 0.05, log(or), NA_real_),
    fill_val   = if_else(comparison %in% flipped_comparisons, -fill_val, fill_val)
  )

# Keep only predictors with at least one significant effect across comparisons
keys_with_effects <- results_hm |>
  filter(key %in% y_structure$key, p_value < 0.05) |>
  pull(key) |>
  unique()

filtered_y_levels <- y_levels[y_levels %in% keys_with_effects]

hm_df <- results_hm |>
  filter(key %in% keys_with_effects) |>
  mutate(key = factor(key, levels = filtered_y_levels))

panel_b <- ggplot(hm_df, aes(x = comparison, y = key)) +
  geom_tile(aes(fill = fill_val), color = "white", linewidth = 0.3) +
  geom_text(
    data = hm_df |> filter(sig != ""),
    aes(label = sig), size = 3, color = "grey20"
  ) +
  scale_fill_gradient2(
    low      = policy_contrast_colors[["gbf"]],
    mid      = "white",
    high     = policy_contrast_colors[["saf"]],
    midpoint = 0,
    na.value = "grey92",
    name     = "log(OR)",
    guide    = guide_colorbar(barwidth = 0.5, barheight = 6, title.position = "top")
  ) +
  scale_y_discrete(labels = key_labels) +
  theme_clean(base_size = 11) +
  theme(
    axis.text.y      = element_markdown(),
    axis.text.x      = element_text(size = 9),
    legend.position  = "right",
    legend.title     = element_text(size = 9),
    panel.background = element_blank()
  ) +
  labs(x = NULL, y = NULL)

# -------------------------------------------------------------------
# Combine
# -------------------------------------------------------------------
combined <- panel_a / panel_b +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(plot_out, combined, width = 11, height = 8)
message("Profiles combined plot saved: ", plot_out)
