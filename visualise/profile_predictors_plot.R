library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))
source(here("visualise", "profile_ame.R"))

if (exists("snakemake")) {
  ame_file <- snakemake@input[["ame"]]
  plot_out <- snakemake@output[["plot"]]
} else {
  ame_file <- here("data", "profile_predictors_ame.csv")
  plot_out <- here("output", "supp_figs", "lpa_profile_predictors.png")
}

# All predictors of profile membership (average marginal effects); the main
# profiles figure shows the subset with at least one significant effect.
p <- plot_profile_ame(read_profile_ame(ame_file), significant_only = FALSE)

save_paper_figure(p, plot_out, height_mm = 165)
message("Profile predictor plot saved: ", plot_out)
