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
  choice_out          <- here("output", "supp_figs", "general_choice_conjoint.png")
  rating_out          <- here("output", "supp_figs", "general_rating_conjoint.png")
  country_choice_out  <- here("output", "country_choice_conjoint.png")
  country_rating_out  <- here("output", "supp_figs", "country_rating_conjoint.png")
}

read_rating <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL)
}

# ── Pooled (all countries): supplementary ───────────────────────────────────

choice_plot <- plot_attribute_emm(read_csv(choice_file, show_col_types = FALSE),
                        x_label = "Choice probability", midline = 0.5,
                        breaks = choice_breaks)

rating_plot <- plot_attribute_emm(read_rating(rating_file),
                        x_label = "Estimated rating", midline = 3,
                        symmetric = TRUE)

save_paper_figure(choice_plot, choice_out, width_mm = paper_width_single_mm,
                  height_mm = 110)
save_paper_figure(rating_plot, rating_out, width_mm = paper_width_single_mm,
                  height_mm = 110)

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

message("Plots saved: ", choice_out, " & ", rating_out)
message("Country plots saved: ", country_choice_out, " & ", country_rating_out)
