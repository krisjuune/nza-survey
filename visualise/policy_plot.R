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

policy_labels <- c(
  "GBF"            = "Supplier-mandated carbon removal\n(geologically balanced fuels)",
  "CORSIA"         = "Mandatory airline offsetting\n(like CORSIA)",
  "SAF_ETS"        = "Regulated clean fuel transition\n(like SAF + ETS mandate)",
  "VCM_SBTi"       = "Voluntary passenger-funded carbon removal\n(voluntary carbon market aligned with the SBTi)",
  "VCM_status_quo" = "Voluntary passenger-funded offsetting\n(voluntary carbon market as currently practised)"
)

# Top-to-bottom display order, highest choice probability first; reversed for
# the factor levels since coord_flip puts the first level at the bottom.
policy_order <- c("SAF_ETS", "GBF", "CORSIA", "VCM_SBTi", "VCM_status_quo")
policy_level_labels <- policy_labels[rev(policy_order)]

choice_df <- read_csv(policy_choice_emm, show_col_types = FALSE) |>
  mutate(
    policy_label = factor(
      policy_labels[policy_type],
      levels = policy_level_labels
    )
  )

choice_plot <- ggplot(choice_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(0.5) +
  geom_point(size = 3, color = accent_color) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, color = accent_color
  ) +
  scale_y_continuous(breaks = choice_breaks) +
  coord_flip() +
  theme_clean() +
  labs(
    x = NULL,
    y = "Choice probability"
  )

# -------------------
# Attribute classification table, shown alongside the choice plot.
# Mirrors the policy_type rules in scripts/preprocessing/reshape_conjoint.R.
# -------------------

policy_attrs <- tibble(
  policy_type = names(policy_labels),
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
    policy_label = factor(policy_labels[policy_type], levels = policy_level_labels)
  )

col_x <- c(fuel = 1, offsetting = 2.6, durability = 4.9, actor = 6.1)

attrs_plot <- ggplot(policy_attrs, aes(y = policy_label)) +
  geom_text(aes(x = col_x[["fuel"]], label = fuel), hjust = 0, vjust = 0.5, size = 3.1) +
  geom_text(aes(x = col_x[["offsetting"]], label = offsetting), hjust = 0, vjust = 0.5, size = 3.1) +
  geom_text(aes(x = col_x[["durability"]], label = durability), hjust = 0, vjust = 0.5, size = 3.1) +
  geom_text(aes(x = col_x[["actor"]], label = actor), hjust = 0, vjust = 0.5, size = 3.1) +
  scale_x_continuous(
    breaks = col_x,
    labels = c("Fuel", "Offsetting activity", "Durability", "Actor"),
    limits = c(0.8, 7.8),
    position = "top",
    expand = c(0, 0)
  ) +
  theme_clean() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.line   = element_blank(),
    axis.text.x = element_text(face = "bold", hjust = 0),
    panel.grid  = element_blank()
  ) +
  labs(x = NULL, y = NULL)

choice_with_attrs <- choice_plot + attrs_plot +
  plot_layout(widths = c(0.4, 0.6))

ggsave(choice_out, choice_with_attrs, width = plot_size$strip$width, height = plot_size$strip$height)
message("Policy choice plot saved: ", choice_out)

rating_df <- read_csv(policy_rating_emm, show_col_types = FALSE) |>
  rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL) |>
  mutate(
    policy_label = factor(
      policy_labels[policy_type],
      levels = policy_level_labels
    )
  )

rating_plot <- ggplot(rating_df, aes(x = policy_label, y = prob)) +
  geom_neutral_line(3) +
  geom_point(size = 3, color = accent_color) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2, color = accent_color
  ) +
  coord_flip(
    ylim = symmetric_limits(c(rating_df$asymp.LCL, rating_df$asymp.UCL), 3)
  ) +
  theme_clean() +
  labs(
    x = NULL,
    y = "Estimated rating"
  )

ggsave(rating_out, rating_plot, width = 7, height = plot_size$strip$height)
message("Policy rating plot saved: ", rating_out)
