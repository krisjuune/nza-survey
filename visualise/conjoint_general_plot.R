library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file         <- snakemake@input[["choice"]]
  rating_file         <- snakemake@input[["rating"]]
  country_choice_file <- snakemake@input[["country_choice"]]
  country_rating_file <- snakemake@input[["country_rating"]]
  choice_out          <- snakemake@output[["choice_plot"]]
  rating_out          <- snakemake@output[["rating_plot"]]
  country_choice_out  <- snakemake@output[["country_choice_plot"]]
  country_rating_out  <- snakemake@output[["country_rating_plot"]]
} else {
  choice_file         <- here("data", "overall_choice_emm.csv")
  rating_file         <- here("data", "overall_rating_emm.csv")
  country_choice_file <- here("data", "country_choice_emm.csv")
  country_rating_file <- here("data", "country_rating_emm.csv")
  choice_out          <- here("output", "general_choice_conjoint.png")
  rating_out          <- here("output", "general_rating_conjoint.png")
  country_choice_out  <- here("output", "country_choice_conjoint.png")
  country_rating_out  <- here("output", "country_rating_conjoint.png")
}

choice_df <- read_csv(choice_file, show_col_types = FALSE)

rating_df <- read_csv(rating_file, show_col_types = FALSE) |>
  rename(
    prob = emmean,
    asymp.LCL = lower.CL,
    asymp.UCL = upper.CL
  )

plot_emm <- function(df, y_label = NULL, midline = NULL, symmetric = FALSE,
                      breaks = waiver()) {

  df <- df |>
    mutate(
      code = case_when(
        code == "trees"       ~ "Nature-based offsets",
        code == "factory_ccs" ~ "Point source capture",
        code == "direct_air"  ~ "Direct air capture",
        code == "cookstoves"  ~ "Traditional offsets",
        code == "fossil"      ~ "Fossil fuels",
        code == "plants"      ~ "Biofuels",
        code == "electric"    ~ "Synthetic fuels",
        code == "temporary"   ~ "Temporary",
        code == "permanent"   ~ "Permanent",
        code == "fuel_suppliers" ~ "Fuel suppliers",
        code == "airline"       ~ "Airlines",
        code == "government"    ~ "Government",
        code == "passenger"     ~ "Passengers",
        code == "10"          ~ "10%",
        code == "30"          ~ "30%",
        code == "50"          ~ "50%",
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

  empty_rows <- tibble(
    attribute = attribute_headers,
    code      = attribute_headers,
    prob = NA_real_,
    SE = NA_real_,
    df = NA_real_,
    asymp.LCL = NA_real_,
    asymp.UCL = NA_real_
  )

  df <- bind_rows(df, empty_rows)

  df <- df |>
    mutate(
      code = factor(code, levels = rev(plot_levels)),
      attribute = factor(attribute, levels = c(
        "Fuel",
        "Offsetting activity",
        "Durability of offsets",
        "Responsible actors",
        "Increase in ticket cost"
      ))
    )

  # Create labels AFTER factorization
  label_map <- tibble(
    code = levels(df$code)
  ) |>
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
    geom_point(size = 3, na.rm = TRUE) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 0.2, na.rm = TRUE) +
    scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
    scale_color_attribute() +
    scale_y_continuous(breaks = breaks) +
    coord_flip(ylim = y_limits) +
    theme_clean() +
    theme(
      axis.text.y = ggtext::element_markdown()
    ) +
    labs(
      x = NULL,
      y = y_label
    )
}

choice_plot <- plot_emm(
  choice_df,
  y_label = "Choice probability",
  midline = 0.5,
  breaks = choice_breaks
)

rating_plot <- plot_emm(
  rating_df,
  y_label = "Estimated rating",
  midline = 3,
  symmetric = TRUE
)

ggsave(choice_out, choice_plot, width = 8, height = 9)
ggsave(rating_out, rating_plot, width = 8, height = 9)

# -------------------
# Country subgroup plots
# -------------------

country_choice_df <- read_csv(country_choice_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_levels))

country_rating_df <- read_csv(country_rating_file, show_col_types = FALSE) |>
  rename(
    prob      = emmean,
    asymp.LCL = lower.CL,
    asymp.UCL = upper.CL
  ) |>
  mutate(country = factor(country, levels = country_levels))

plot_emm_country <- function(df, y_label = NULL, midline = NULL, symmetric = FALSE,
                              breaks = waiver()) {

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
    attribute = attribute_headers,
    code      = attribute_headers,
    country   = levels(df$country)
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
      ))
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
                  width = 0.2, na.rm = TRUE) +
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
    facet_wrap(~ country, nrow = 1)
}

country_choice_plot <- plot_emm_country(
  country_choice_df,
  y_label = "Choice probability",
  midline = 0.5,
  breaks = choice_breaks
)

country_rating_plot <- plot_emm_country(
  country_rating_df,
  y_label = "Estimated rating",
  midline = 3,
  symmetric = TRUE
)

ggsave(country_choice_out, country_choice_plot, width = 11, height = 7)
ggsave(country_rating_out, country_rating_plot, width = 11, height = 7)

message("Plots saved: ", choice_out, " & ", rating_out)
message("Country plots saved: ", country_choice_out, " & ", country_rating_out)