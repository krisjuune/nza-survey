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
  choice_file <- here("data", "flyer_type_choice_emm.csv")
  rating_file <- here("data", "flyer_type_rating_emm.csv")
  choice_out  <- here("output", "flyer_type_choice_plot.png")
  rating_out  <- here("output", "flyer_type_rating_plot.png")
}

choice_df <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(flyer_type = factor(flyer_type, levels = flyer_levels))

rating_df <- read_csv(rating_file, show_col_types = FALSE) |>
  rename(
    prob = emmean,
    asymp.LCL = lower.CL,
    asymp.UCL = upper.CL
  ) |>
  mutate(flyer_type = factor(flyer_type, levels = flyer_levels))

plot_emm_flyer_type <- function(df, y_label = NULL, midline = NULL,
                                 symmetric = FALSE, breaks = waiver()) {

  df <- df |>
    mutate(
      code = case_when(
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
        TRUE ~ as.character(code)
      ),
      attribute = case_when(
        attribute == "activity_code"       ~ "Offsetting activity",
        attribute == "fuel_code"           ~ "Fuel",
        attribute == "durability_code"     ~ "Durability of offsets",
        attribute == "responsibility_code" ~ "Responsible actors",
        attribute == "cost_code"           ~ "Increase in ticket cost",
        TRUE ~ attribute
      )
    )

  empty_rows <- expand_grid(
    attribute  = attribute_headers,
    code       = attribute_headers,
    flyer_type = levels(df$flyer_type)
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
      code = factor(code, levels = rev(plot_levels)),
      attribute = factor(attribute, levels = c(
        "Fuel",
        "Offsetting activity",
        "Durability of offsets",
        "Responsible actors",
        "Increase in ticket cost"
      )),
      flyer_type = factor(flyer_type, levels = flyer_levels)
    )

  label_map <- tibble(code = levels(df$code)) |>
    mutate(
      code_label = case_when(
        code %in% c(
          "Fuel",
          "Offsetting activity",
          "Durability of offsets",
          "Responsible actors",
          "Increase in ticket cost"
        ) ~ paste0("<b>", code, "</b>"),
        TRUE ~ code
      )
    )

  y_limits <- if (symmetric) {
    symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), midline)
  } else {
    c(min(df$asymp.LCL, na.rm = TRUE), max(df$asymp.UCL, na.rm = TRUE))
  }
  y_limits <- widen_limits_to_breaks(y_limits, breaks)

  ggplot(df, aes(x = code, y = prob, color = attribute)) +
    geom_neutral_line(midline) +
    geom_point(size = 2, na.rm = TRUE) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 0, na.rm = TRUE) +
    scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
    scale_color_attribute() +
    scale_y_continuous(breaks = breaks) +
    coord_flip(ylim = y_limits) +
    theme_clean(base_size = 11) +
    theme(
      axis.text.y = ggtext::element_markdown()
    ) +
    labs(
      x = NULL,
      y = y_label
    ) +
    facet_wrap(~ flyer_type, nrow = 1)
}

choice_plot <- plot_emm_flyer_type(
  choice_df,
  y_label = "Choice probability",
  midline = 0.5,
  breaks = choice_breaks
)

rating_plot <- plot_emm_flyer_type(
  rating_df,
  y_label = "Estimated rating",
  midline = 3,
  symmetric = TRUE
)

ggsave(choice_out, choice_plot, width = 11, height = 7)
ggsave(rating_out, rating_plot, width = 11, height = 7)

message("Flyer-type plots saved: ", choice_out, " & ", rating_out)
