library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file <- snakemake@input[["choice"]]
  plot_out    <- snakemake@output[["choice_plot"]]
} else {
  choice_file <- here("data", "framing_effect_choice.csv")
  plot_out    <- here("output", "framing_effect_choice_plot.png")
}

country_levels <- c(
  "Australia", "Brazil", "Germany", "Kenya", "UAE", "Vietnam"
)

choice_df <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_levels))

plot_levels <- c(
  "Fuel",
  "Fossil fuels", "Biofuels", "Synthetic fuels",
  "Offsetting activity",
  "Traditional offsets", "Direct air capture",
  "Point source capture", "Nature-based offsets",
  "Durability of offsets",
  "Temporary", "Permanent",
  "Responsible actors",
  "Fuel suppliers", "Airlines", "Government", "Passengers",
  "Increase in ticket cost",
  "10%", "30%", "50%"
)

df <- choice_df |>
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
  attribute = c("Fuel", "Offsetting activity", "Durability of offsets",
                "Responsible actors", "Increase in ticket cost"),
  code      = c("Fuel", "Offsetting activity", "Durability of offsets",
                "Responsible actors", "Increase in ticket cost"),
  country   = levels(df$country)
) |>
  mutate(
    estimate  = NA_real_,
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
    country = factor(country, levels = country_levels)
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

y_limits <- symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), 0)

plot <- ggplot(df, aes(x = code, y = estimate, color = attribute)) +
  geom_neutral_line(0) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, na.rm = TRUE) +
  scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
  scale_color_attribute() +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  coord_flip(ylim = y_limits) +
  theme_clean(base_size = 11) +
  theme(
    axis.text.y = ggtext::element_markdown()
  ) +
  labs(
    x = NULL,
    y = "Shift in choice probability"
  ) +
  facet_wrap(~ country, nrow = 1)

ggsave(plot_out, plot, width = 11, height = 7)

message("Framing-effect plot saved: ", plot_out)
