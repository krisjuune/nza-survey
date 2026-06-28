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
  plot_out    <- here("output", "country_framing_choice.png")
}

country_levels <- c(
  "Australia", "Brazil", "Germany", "Kenya", "UAE", "Vietnam"
)

framing_levels <- c("No information", "Net-zero information")

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

df <- read_csv(choice_file, show_col_types = FALSE) |>
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
    ),
    framing = recode(framing,
      `0` = "No information",
      `1` = "Net-zero information"
    )
  )

empty_rows <- expand_grid(
  attribute  = c("Fuel", "Offsetting activity", "Durability of offsets",
                 "Responsible actors", "Increase in ticket cost"),
  code       = c("Fuel", "Offsetting activity", "Durability of offsets",
                 "Responsible actors", "Increase in ticket cost"),
  country    = country_levels,
  framing    = framing_levels
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
    country = factor(country, levels = country_levels),
    framing = factor(framing, levels = framing_levels),
    # Literal lighter hex for the net-zero arm (not alpha), so the fill stays
    # fully opaque and the error bar drawn underneath can never show through.
    point_color = if_else(
      framing == "No information",
      attribute_colors[as.character(attribute)],
      attribute_colors_light[as.character(attribute)]
    )
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

y_limits <- widen_limits_to_breaks(
  c(min(df$asymp.LCL, na.rm = TRUE), max(df$asymp.UCL, na.rm = TRUE)),
  choice_breaks
)

# Both arms get a full error bar in their own (opaque) color, drawn before the
# points so the points - also fully opaque - sit on top and cover whatever
# error bar segment falls directly underneath them. No-framing is drawn last
# within the point layer so it's the one left visible wherever the two arms
# overlap.
# The real points are colored per attribute x framing via scale_color_identity
# (point_color is already a literal hex), so a generic black/grey dummy legend
# is added separately to explain what "faded" means without implying framing
# has its own attribute color.
legend_dummy <- tibble(framing = factor(framing_levels, levels = framing_levels))

plot <- ggplot(df, aes(x = code, y = prob)) +
  geom_neutral_line(0.5) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = point_color),
    width = 0.2, na.rm = TRUE
  ) +
  geom_point(
    data = df |> arrange(desc(framing)),
    aes(color = point_color), size = 2, na.rm = TRUE
  ) +
  scale_color_identity(guide = "none") +
  geom_point(
    data = legend_dummy,
    aes(x = NA, y = NA, shape = framing),
    color = "black", inherit.aes = FALSE, na.rm = TRUE
  ) +
  scale_shape_manual(
    values = c("No information" = 16, "Net-zero information" = 16),
    guide = guide_legend(
      title = NULL,
      override.aes = list(color = c("black", "grey60"))
    )
  ) +
  scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
  scale_y_continuous(breaks = choice_breaks) +
  coord_flip(ylim = y_limits) +
  theme_clean(base_size = 11) +
  theme(
    axis.text.y = ggtext::element_markdown(),
    legend.position = "bottom"
  ) +
  labs(
    x = NULL,
    y = "Choice probability"
  ) +
  facet_wrap(~ country, nrow = 1)

ggsave(plot_out, plot, width = 11, height = 7)

message("Country framing-effect plot saved: ", plot_out)
