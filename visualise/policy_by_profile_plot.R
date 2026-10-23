library(dplyr)
library(ggplot2)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file <- snakemake@input[["choice"]]
  plot_out    <- snakemake@output[["plot"]]
} else {
  choice_file <- here("data", "policy_by_profile_choice.csv")
  plot_out    <- here("output", "supp_figs", "policy_by_profile_plot.png")
}


policy_order   <- policy_display_order
policy_display <- setNames(policy_label(policy_order, wrap = TRUE), policy_order)
level_labels <- policy_display[rev(policy_order)]

profile_names <- c(
  "Profile 1" = "Permanence sceptics",
  "Profile 2" = "Moderate demanders",
  "Profile 3" = "High-integrity demanders"
)
profile_levels <- c("High-integrity demanders", "Moderate demanders", "Permanence sceptics")

raw <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(
    profile = recode(profile, !!!profile_names),
    profile = factor(profile, levels = profile_levels)
  )

profile_label_map <- raw |>
  distinct(profile, n_profile) |>
  arrange(profile) |>
  mutate(profile_label = paste0(profile, "\n(n = ", n_profile, ")"))

profile_label_levels <- profile_label_map$profile_label

df <- raw |>
  left_join(select(profile_label_map, profile, profile_label), by = "profile") |>
  mutate(
    policy_label  = factor(policy_display[policy_type], levels = level_labels),
    profile_label = factor(profile_label, levels = profile_label_levels)
  )

p <- ggplot(df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                 color = policy_color, linewidth = paper_ci_width) +
  geom_point(color = policy_color, size = paper_point_size) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  facet_wrap(~profile_label, nrow = 1) +
  coord_flip(ylim = c(0.15, 0.85)) +
  theme_paper(legend = "none") +
  labs(x = NULL, y = "Choice probability")

save_paper_figure(p, plot_out, height_mm = 65)
message("Policy by profile plot saved: ", plot_out)
