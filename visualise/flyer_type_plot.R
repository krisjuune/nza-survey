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
  choice_out  <- here("output", "supp-figs", "flyer_type_choice_plot.png")
  rating_out  <- here("output", "supp-figs", "flyer_type_rating_plot.png")
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

choice_plot <- plot_attribute_emm(choice_df, x_label = "Choice probability",
                                  midline = 0.5, breaks = choice_breaks,
                                  facet = "flyer_type")
rating_plot <- plot_attribute_emm(rating_df, x_label = "Estimated rating",
                                  midline = 3, symmetric = TRUE,
                                  facet = "flyer_type")

save_paper_figure(choice_plot, choice_out, height_mm = 115)
save_paper_figure(rating_plot, rating_out, height_mm = 115)

message("Flyer-type plots saved: ", choice_out, " & ", rating_out)
