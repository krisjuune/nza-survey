library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  means_file <- snakemake@input[["means"]]
  plot_out   <- snakemake@output[["profile_plot"]]
} else {
  means_file <- here("data", "lpa_conjoint_means.csv")
  plot_out   <- here("output", "supp_figs", "lpa_conjoint_profiles.png")
}

# ColorBrewer Set2 — qualitatively distinct, not reused elsewhere in the theme.
profile_colors <- c("Profile 1" = "#66c2a5", "Profile 2" = "#fc8d62", "Profile 3" = "#8da0cb")

recode_codes <- function(code) {
  case_when(
    code == "trees"          ~ "Nature-based offsets",
    code == "factory_ccs"    ~ "Point source capture",
    code == "direct_air"     ~ "Direct air capture",
    code == "cookstoves"     ~ "Traditional offsets",
    code == "fossil"         ~ "Fossil fuels",
    code == "plants"         ~ "Biofuels",
    code == "electric"       ~ "Synthetic fuels",
    code == "temporary"      ~ "Temporary",
    code == "permanent"      ~ "Permanent",
    code == "fuel_suppliers" ~ "Fuel suppliers",
    code == "airline"        ~ "Airlines",
    code == "government"     ~ "Government",
    code == "passenger"      ~ "Passengers",
    code == "10"             ~ "10%",
    code == "30"             ~ "30%",
    code == "50"             ~ "50%",
    TRUE                     ~ as.character(code)
  )
}

recode_attributes <- function(attribute) {
  case_when(
    attribute == "activity_code"       ~ "Offsetting activity",
    attribute == "fuel_code"           ~ "Fuel",
    attribute == "durability_code"     ~ "Durability of offsets",
    attribute == "responsibility_code" ~ "Responsible actors",
    attribute == "cost_code"           ~ "Increase in ticket cost",
    TRUE ~ attribute
  )
}

df <- read_csv(means_file, show_col_types = FALSE) |>
  mutate(
    code      = recode_codes(level),
    attribute = recode_attributes(attribute),
    profile   = factor(profile, levels = c(1, 2, 3),
                       labels = paste("Profile", 1:3))
  )

empty_rows <- expand_grid(
  attribute = attribute_headers,
  code      = attribute_headers,
  profile   = paste("Profile", 1:3)
) |>
  mutate(mean_pref = NA_real_, sd_pref = NA_real_)

df <- bind_rows(df, empty_rows) |>
  mutate(
    code      = factor(code,      levels = rev(plot_levels)),
    attribute = factor(attribute, levels = attribute_headers)
  )

label_map <- tibble(code = levels(df$code)) |>
  mutate(
    code_label = if_else(
      code %in% attribute_headers,
      paste0("<b>", code, "</b>"),
      code
    )
  )

dodge <- position_dodge(width = 0.6)

profile_plot <- ggplot(df, aes(x = code, y = mean_pref,
                                color = profile, group = profile)) +
  geom_neutral_line(0) +
  geom_point(size = point_size, na.rm = TRUE, position = dodge) +
  geom_errorbar(
    aes(ymin = mean_pref - sd_pref, ymax = mean_pref + sd_pref),
    width = errorbar_width, na.rm = TRUE, position = dodge
  ) +
  scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
  scale_color_manual(values = profile_colors, name = "Preference profile") +
  coord_flip() +
  theme_clean(base_size = 11) +
  theme(
    axis.text.y     = ggtext::element_markdown(),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = "Mean preference score (−1 to +1)")

ggsave(plot_out, profile_plot,
       width  = plot_size$narrow$width,
       height = plot_size$narrow$height)
message("LPA profile plot saved: ", plot_out)
