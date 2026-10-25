library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file         <- snakemake@input[["choice"]]
  rating_file         <- snakemake@input[["rating"]]
  country_choice_file <- snakemake@input[["country_choice"]]
  country_rating_file <- snakemake@input[["country_rating"]]
  general_out         <- snakemake@output[["general_plot"]]
  country_choice_out  <- snakemake@output[["country_choice_plot"]]
  country_rating_out  <- snakemake@output[["country_rating_plot"]]
} else {
  choice_file         <- here("data", "overall_choice_emm.csv")
  rating_file         <- here("data", "overall_rating_emm.csv")
  country_choice_file <- here("data", "country_choice_emm.csv")
  country_rating_file <- here("data", "country_rating_emm.csv")
  general_out         <- here("output", "supp_figs", "general_conjoint.png")
  country_choice_out  <- here("output", "country_choice_conjoint.png")
  country_rating_out  <- here("output", "supp_figs", "country_rating_conjoint.png")
}

read_rating <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL)
}

# ── Pooled (all countries): supplementary, A = choice, B = rating ─────────────

choice_plot <- plot_attribute_emm(read_csv(choice_file, show_col_types = FALSE),
                        x_label = "Choice probability", midline = 0.5,
                        breaks = choice_breaks)

# All pooled ratings lie above the scale midpoint (3), so the axis runs from 3
# upwards instead of being centred on it; rows are shared with A, so no labels
rating_plot <- plot_attribute_emm(read_rating(rating_file),
                        x_label = "Estimated rating", midline = 3,
                        breaks = seq(3, 4, by = 0.25)) +
  theme(axis.text.y = element_blank())

general_plot <- (choice_plot | rating_plot) + panel_tags()

save_paper_figure(general_plot, general_out, height_mm = 110)

# ── By country: choice is a main figure, rating supplementary ───────────────

country_choice_df <- read_csv(country_choice_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_levels))

country_rating_df <- read_rating(country_rating_file) |>
  mutate(country = factor(country, levels = country_levels))

# Main figure: same indigo as the WTP figure
country_choice_plot <- plot_attribute_emm(country_choice_df,
                                x_label = "Choice probability", midline = 0.5,
                                breaks = choice_breaks, facet = "country",
                                color = highlight_color)

country_rating_plot <- plot_attribute_emm(country_rating_df,
                                x_label = "Estimated rating", midline = 3,
                                symmetric = TRUE, facet = "country")

save_paper_figure(country_choice_plot, country_choice_out, height_mm = 115)
save_paper_figure(country_rating_plot, country_rating_out, height_mm = 115)

message("Pooled plot saved: ", general_out)
message("Country plots saved: ", country_choice_out, " & ", country_rating_out)
