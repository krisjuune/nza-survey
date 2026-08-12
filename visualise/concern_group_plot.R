library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file <- snakemake@input[["choice"]]
  rating_file <- snakemake@input[["rating"]]
  choice_out  <- snakemake@output[["choice_plot"]]
  rating_out  <- snakemake@output[["rating_plot"]]
} else {
  choice_file <- here("data", "concern_choice_emm.csv")
  rating_file <- here("data", "concern_rating_emm.csv")
  choice_out  <- here("output", "supp_figs", "concern_choice_plot.png")
  rating_out  <- here("output", "supp_figs", "concern_rating_plot.png")
}

read_and_factor <- function(path, rename_cols = NULL) {
  df <- read_csv(path, show_col_types = FALSE)
  if (!is.null(rename_cols)) df <- rename(df, !!!rename_cols)
  df |>
    mutate(
      concern_group = factor(concern_group, levels = concern_levels),
      country       = factor(country,       levels = country_levels)
    )
}

choice_df <- read_and_factor(choice_file)
rating_df <- read_and_factor(
  rating_file,
  rename_cols = c(prob = "emmean", asymp.LCL = "lower.CL", asymp.UCL = "upper.CL")
)

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

plot_emm_concern <- function(df, y_label = NULL, midline = NULL,
                              symmetric = FALSE, breaks = waiver()) {

  df <- df |>
    mutate(
      code      = recode_codes(code),
      attribute = recode_attributes(attribute)
    )

  empty_rows <- expand_grid(
    attribute     = attribute_headers,
    code          = attribute_headers,
    concern_group = concern_levels,
    country       = country_levels
  ) |>
    mutate(
      prob      = NA_real_,
      SE        = NA_real_,
      df        = NA_real_,
      asymp.LCL = NA_real_,
      asymp.UCL = NA_real_
    )

  df <- bind_rows(df, empty_rows) |>
    mutate(
      code          = factor(code,          levels = rev(plot_levels)),
      attribute     = factor(attribute,     levels = attribute_headers),
      concern_group = factor(concern_group, levels = concern_levels),
      country       = factor(country,       levels = country_levels)
    )

  label_map <- tibble(code = levels(df$code)) |>
    mutate(
      code_label = if_else(
        code %in% attribute_headers,
        paste0("<b>", code, "</b>"),
        code
      )
    )

  y_limits <- if (symmetric) {
    symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), midline)
  } else {
    c(min(df$asymp.LCL, na.rm = TRUE), max(df$asymp.UCL, na.rm = TRUE))
  }
  y_limits <- widen_limits_to_breaks(y_limits, breaks)

  dodge <- position_dodge(width = 0.6)

  ggplot(df, aes(x = code, y = prob, color = concern_group, group = concern_group)) +
    geom_neutral_line(midline) +
    geom_point(size = 1.8, na.rm = TRUE, position = dodge) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 0, na.rm = TRUE, position = dodge
    ) +
    scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
    scale_color_manual(
      values = concern_colors,
      name   = "Climate concern"
    ) +
    scale_y_continuous(breaks = breaks) +
    coord_flip(ylim = y_limits) +
    facet_wrap(~ country, nrow = 1) +
    theme_clean(base_size = 11) +
    theme(
      axis.text.y  = ggtext::element_markdown(),
      legend.position = "bottom"
    ) +
    labs(x = NULL, y = y_label)
}

choice_plot <- plot_emm_concern(
  choice_df,
  y_label = "Choice probability",
  midline = 0.5,
  breaks  = choice_breaks
)

rating_plot <- plot_emm_concern(
  rating_df,
  y_label   = "Estimated rating",
  midline   = 3,
  symmetric = TRUE
)

ggsave(choice_out, choice_plot, width = 11, height = 7)
ggsave(rating_out, rating_plot, width = 11, height = 7)

message("Concern-group plots saved: ", choice_out, " & ", rating_out)
