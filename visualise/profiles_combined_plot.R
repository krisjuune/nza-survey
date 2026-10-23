library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))
source(here("visualise", "profile_ame.R"))

if (exists("snakemake")) {
  choice_file  <- snakemake@input[["choice"]]
  ame_file     <- snakemake@input[["ame"]]
  plot_out     <- snakemake@output[["plot"]]
} else {
  choice_file  <- here("data", "policy_by_profile_choice.csv")
  ame_file     <- here("data", "profile_predictors_ame.csv")
  plot_out     <- here("output", "profiles_combined_plot.png")
}

profile_levels <- profile_display_order

# -------------------------------------------------------------------
# Panel a: policy preference by profile
# -------------------------------------------------------------------
policy_order   <- policy_display_order
policy_display <- setNames(policy_label(policy_order, wrap = TRUE), policy_order)
level_labels  <- policy_display[rev(policy_order)]

raw_choice <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(
    profile = recode(profile, !!!profile_names),
    profile = factor(profile, levels = profile_levels)
  )

profile_label_map <- raw_choice |>
  distinct(profile, n_profile) |>
  arrange(profile) |>
  mutate(profile_label = paste0(profile, "\n(n = ", n_profile, ")"))

choice_df <- raw_choice |>
  left_join(select(profile_label_map, profile, profile_label), by = "profile") |>
  mutate(
    policy_label  = factor(policy_display[policy_type], levels = level_labels),
    profile_label = factor(profile_label, levels = profile_label_map$profile_label)
  )

panel_a <- ggplot(choice_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                 color = policy_color, linewidth = paper_ci_width) +
  geom_point(color = policy_color, size = paper_point_size) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  facet_wrap(~profile_label, nrow = 1) +
  coord_flip(ylim = c(0.15, 0.85)) +
  theme_paper(legend = "none") +
  labs(x = NULL, y = "Choice probability")

# -------------------------------------------------------------------
# Panel b: predictors of profile membership (average marginal effects),
# predictors with at least one significant effect
# -------------------------------------------------------------------
panel_b <- plot_profile_ame(read_profile_ame(ame_file), significant_only = TRUE)

# -------------------------------------------------------------------
# Combine
# -------------------------------------------------------------------
combined <- panel_a / panel_b +
  plot_layout(heights = c(1, 1.2)) +
  panel_tags()

save_paper_figure(combined, plot_out, height_mm = 150)
message("Profiles combined plot saved: ", plot_out)
