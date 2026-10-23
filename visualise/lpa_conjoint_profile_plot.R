library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  means_file <- snakemake@input[["means"]]
  plot_out   <- snakemake@output[["profile_plot"]]
} else {
  means_file <- here("data", "lpa_conjoint_means.csv")
  plot_out   <- here("output", "supp_figs", "lpa_conjoint_profiles.png")
}

profile_names <- c("High-integrity demanders", "Moderate demanders",
                   "Permanence sceptics")
# Profile colours from config, keyed by LPA profile number
profile_values <- setNames(
  unname(profile_colors[c("Profile 3", "Profile 2", "Profile 1")]),
  profile_names
)

# Mean +/- 1 SD of individual preference scores, in the shared plot's columns
df <- read_csv(means_file, show_col_types = FALSE) |>
  transmute(
    attribute,
    code      = level,
    profile   = as.character(factor(profile, levels = c(1, 2, 3),
                                    labels = rev(profile_names))),
    prob      = mean_pref,
    asymp.LCL = mean_pref - sd_pref,
    asymp.UCL = mean_pref + sd_pref
  )

profile_plot <- plot_attribute_emm(df,
                                   x_label = "Mean preference score (\u22121 to +1)",
                                   midline = 0, group = "profile",
                                   group_values = profile_values,
                                   group_name = NULL) +
  # single-column width: stack the legend entries
  guides(color = guide_legend(ncol = 1))

save_paper_figure(profile_plot, plot_out, width_mm = paper_width_single_mm,
                  height_mm = 130)
message("LPA profile plot saved: ", plot_out)
