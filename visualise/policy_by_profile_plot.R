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

policy_labels <- c(
  "GBF"            = "Supplier-mandated\ncarbon removal (GBF)",
  "CORSIA"         = "Mandatory airline\noffsetting (CORSIA)",
  "SAF_ETS"        = "Regulated clean fuel\ntransition (SAF + ETS)",
  "VCM_SBTi"       = "Voluntary passenger-funded\ncarbon removal (SBTi)",
  "VCM_status_quo" = "Voluntary passenger-funded\noffsetting (status quo)"
)

policy_order <- c("SAF_ETS", "GBF", "CORSIA", "VCM_SBTi", "VCM_status_quo")
level_labels <- policy_labels[rev(policy_order)]

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
  mutate(profile_label = paste0(profile, " (n = ", n_profile, ")"))

profile_label_levels <- profile_label_map$profile_label

df <- raw |>
  left_join(select(profile_label_map, profile, profile_label), by = "profile") |>
  mutate(
    policy_label  = factor(policy_labels[policy_type], levels = level_labels),
    profile_label = factor(profile_label, levels = profile_label_levels)
  )

p <- ggplot(df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_point(size = point_size + 1, color = accent_color) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, color = accent_color
  ) +
  facet_wrap(~profile_label, nrow = 1) +
  coord_flip() +
  theme_clean() +
  labs(x = NULL, y = "Choice probability")

ggsave(plot_out, p,
       width  = plot_size$strip$width,
       height = plot_size$strip$height)
message("Policy by profile plot saved: ", plot_out)
