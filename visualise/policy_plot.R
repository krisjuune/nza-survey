library(dplyr)
library(ggplot2)
library(readr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  policy_choice_emm <- snakemake@input[["choice"]]
  policy_rating_emm <- snakemake@input[["rating"]]
  choice_out        <- snakemake@output[["choice_plot"]]
  rating_out        <- snakemake@output[["rating_plot"]]
} else {
  policy_choice_emm <- here("data", "policy_choice_emm.csv")
  policy_rating_emm <- here("data", "policy_rating_emm.csv")
  choice_out        <- here("output", "policy_choice_plot.png")
  rating_out        <- here("output", "policy_rating_plot.png")
}

# Top-to-bottom display order from config (highest choice probability first);
# reversed for the factor levels since coord_flip puts the first level at the
# bottom.
policy_order   <- policy_display_order
policy_display <- setNames(policy_label(policy_order, wrap = TRUE), policy_order)
policy_level_labels <- policy_display[rev(policy_order)]

choice_df <- read_csv(policy_choice_emm, show_col_types = FALSE) |>
  mutate(
    policy_label = factor(
      policy_display[policy_type],
      levels = policy_level_labels
    )
  )

choice_plot <- ggplot(choice_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                 color = policy_color, linewidth = paper_ci_width) +
  geom_point(color = policy_color, size = paper_point_size) +
  scale_y_continuous(breaks = choice_breaks) +
  coord_flip() +
  theme_paper(legend = "none") +
  labs(
    x = NULL,
    y = "Choice probability"
  )

# -------------------
# Attribute classification table, shown alongside the choice plot.
# Mirrors the policy_type rules in scripts/preprocessing/reshape_conjoint.R.
# -------------------

# Rows are listed explicitly by code (not tied to the display order), so each
# attribute row stays matched to its policy package.
policy_attrs <- tibble(
  policy_type = c("GBF", "CORSIA", "SAF_ETS", "VCM_SBTi", "VCM_status_quo"),
  fuel = c(
    "Fossil fuels",
    "Fossil fuels /\nBiofuels",
    "Biofuels /\nSynthetic fuels",
    "Fossil fuels",
    "Fossil fuels"
  ),
  offsetting = c(
    "Point source capture /\nDirect air capture",
    "Traditional offsets /\nNature-based offsets",
    "Point source capture /\nDirect air capture",
    "Direct air capture",
    "Traditional offsets /\nNature-based offsets"
  ),
  durability = c(
    "Permanent", "Temporary", "Permanent", "Permanent", "Temporary"
  ),
  actor = c(
    "Fuel suppliers",
    "Airlines",
    "Fuel suppliers /\nGovernment",
    "Passengers",
    "Passengers"
  )
) |>
  mutate(
    policy_label = factor(policy_display[policy_type], levels = policy_level_labels)
  )

col_x <- c(fuel = 1, offsetting = 2.5, durability = 4.65, actor = 5.85)

# Table text at tick-label size; column heads bold (group headers)
attrs_plot <- ggplot(policy_attrs, aes(y = policy_label)) +
  geom_text(aes(x = col_x[["fuel"]], label = fuel), hjust = 0, vjust = 0.5,
            size = paper_text_size, lineheight = 0.9) +
  geom_text(aes(x = col_x[["offsetting"]], label = offsetting), hjust = 0,
            vjust = 0.5, size = paper_text_size, lineheight = 0.9) +
  geom_text(aes(x = col_x[["durability"]], label = durability), hjust = 0,
            vjust = 0.5, size = paper_text_size, lineheight = 0.9) +
  geom_text(aes(x = col_x[["actor"]], label = actor), hjust = 0, vjust = 0.5,
            size = paper_text_size, lineheight = 0.9) +
  scale_x_continuous(
    breaks = col_x,
    labels = c("Fuel", "Offsetting activity", "Durability", "Responsible actor"),
    limits = c(0.8, 7.6),
    position = "top",
    expand = c(0, 0)
  ) +
  theme_paper(legend = "none") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.line   = element_blank(),
    axis.text.x = element_text(face = "bold", hjust = 0, color = axis_color),
    panel.grid  = element_blank()
  ) +
  labs(x = NULL, y = NULL)

choice_with_attrs <- choice_plot + attrs_plot +
  plot_layout(widths = c(0.4, 0.6))

save_paper_figure(choice_with_attrs, choice_out, height_mm = 65)
message("Policy choice plot saved: ", choice_out)

rating_df <- read_csv(policy_rating_emm, show_col_types = FALSE) |>
  rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL) |>
  mutate(
    policy_label = factor(
      policy_display[policy_type],
      levels = policy_level_labels
    )
  )

rating_plot <- ggplot(rating_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(3) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                 color = policy_color, linewidth = paper_ci_width) +
  geom_point(color = policy_color, size = paper_point_size) +
  coord_flip(
    ylim = symmetric_limits(c(rating_df$asymp.LCL, rating_df$asymp.UCL), 3)
  ) +
  theme_paper(legend = "none") +
  labs(
    x = NULL,
    y = "Estimated rating"
  )

save_paper_figure(rating_plot, rating_out, width_mm = paper_width_single_mm,
                  height_mm = 65)
message("Policy rating plot saved: ", rating_out)
