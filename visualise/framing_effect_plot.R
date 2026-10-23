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
  plot_out    <- here("output", "supp_figs", "country_framing_choice.png")
}

framing_levels <- c("No information", "Net-zero information")

# No-information arm in dark ink, net-zero arm in the secondary grey
framing_values <- c("No information" = attribute_color,
                    "Net-zero information" = secondary_color)

df <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(
    framing = recode(framing, `0` = "No information", `1` = "Net-zero information"),
    country = factor(country, levels = country_levels)
  )

plot <- plot_attribute_emm(df, x_label = "Choice probability", midline = 0.5,
                           breaks = choice_breaks, facet = "country",
                           group = "framing", group_values = framing_values,
                           group_name = NULL)

save_paper_figure(plot, plot_out, height_mm = 125)

message("Country framing-effect plot saved: ", plot_out)
