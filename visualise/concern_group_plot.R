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

concern_values <- setNames(concern_colors[c("High", "Mid", "Low")],
                           c("High", "Mid", "Low"))

plot_concern <- function(df, x_label, midline, symmetric = FALSE,
                         breaks = waiver()) {
  plot_attribute_emm(df, x_label = x_label, midline = midline,
                     symmetric = symmetric, breaks = breaks, facet = "country",
                     group = "concern_group", group_values = concern_values,
                     group_name = "Aviation climate concern")
}

choice_plot <- plot_concern(choice_df, "Choice probability", 0.5,
                            breaks = choice_breaks)
rating_plot <- plot_concern(rating_df, "Estimated rating", 3, symmetric = TRUE)

save_paper_figure(choice_plot, choice_out, height_mm = 130)
save_paper_figure(rating_plot, rating_out, height_mm = 130)

message("Concern-group plots saved: ", choice_out, " & ", rating_out)
