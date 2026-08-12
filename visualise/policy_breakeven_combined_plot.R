library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(here)
library(purrr)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  grid_file  <- snakemake@input[["grid"]]
  cost_file  <- snakemake@input[["cost_scenarios"]]
  out_file   <- snakemake@output[["combined"]]
} else {
  grid_file  <- here("data", "policy_breakeven_country_grid.csv")
  cost_file  <- here("data", "airfare_cost_scenarios.csv")
  out_file   <- here("output", "policy_breakeven_combined.png")
}

saf_color <- policy_contrast_colors[["saf"]]
gbf_color <- policy_contrast_colors[["gbf"]]
country_order  <- c("Australia", "Brazil", "Germany", "Kenya", "UAE", "Vietnam")
plot_font_size <- base_font_size + 2

color_scale <- scale_color_manual(
  name   = "Pathway",
  values = c(
    "Clean fuel transition (eSAF + ETS)" = saf_color,
    "Geologically balanced fuels (GBF)"  = gbf_color
  ),
  labels = c(
    "Clean fuel transition (eSAF + ETS)" = "Clean fuel transition\n(eSAF + ETS)",
    "Geologically balanced fuels (GBF)"  = "Geologically balanced\nfuels (GBF)"
  )
)

fill_scale <- scale_fill_manual(
  name   = NULL,
  values = c(
    "Clean fuel transition (eSAF + ETS)" = saf_color,
    "Geologically balanced fuels (GBF)"  = gbf_color
  ),
  guide = "none"
)

y_scale_prob <- scale_y_continuous(
  limits = c(0.20, 0.85),
  breaks = seq(0.20, 0.80, by = 0.10),
  labels = scales::label_number(accuracy = 0.01)
)

shared_theme <- list(
  theme_clean(base_size = plot_font_size),
  theme(
    strip.text    = element_text(face = "bold"),
    panel.spacing = unit(10, "pt"),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )
)

# ── Load data ────────────────────────────────────────────────────────────────

prob_df <- read_csv(grid_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_order))

scenarios <- read_csv(cost_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_order))

# ── Panel A: probability curves ──────────────────────────────────────────────

p_a <- ggplot(prob_df, aes(x = cost_pct)) +
  geom_ribbon(aes(ymin = prob_gbf_lower, ymax = prob_gbf_upper,
                  fill = "Geologically balanced fuels (GBF)"),
              alpha = 0.15) +
  geom_line(aes(y = prob_gbf,
                color = "Geologically balanced fuels (GBF)"),
            linewidth = 0.8) +
  geom_ribbon(aes(ymin = prob_saf_lower, ymax = prob_saf_upper,
                  fill = "Clean fuel transition (eSAF + ETS)"),
              alpha = 0.15) +
  geom_line(aes(y = prob_saf,
                color = "Clean fuel transition (eSAF + ETS)"),
            linewidth = 0.8) +
  geom_neutral_line(0.5) +
  facet_wrap(~country, nrow = 1) +
  color_scale + fill_scale +
  scale_x_continuous(breaks = c(0, 25, 50),
                     labels = c("0%", "25%", "50%"),
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 55)) +
  y_scale_prob +
  shared_theme +
  theme(axis.title.x = element_text(margin = margin(t = 8))) +
  labs(
    x = "Ticket price increase relative to current fares (%)",
    y = "Choice\nprobability"
  )

# ── Shared Panel B data ───────────────────────────────────────────────────────

interp <- prob_df |>
  group_by(country) |>
  group_split() |>
  setNames(levels(prob_df$country)) |>
  lapply(function(g) list(
    saf = approxfun(g$cost_pct, g$prob_saf, rule = 2),
    gbf = approxfun(g$cost_pct, g$prob_gbf, rule = 2)
  ))

saf_ranges <- scenarios |>
  distinct(country, pathway = esaf_pathway, esaf_median, esaf_lower, esaf_upper) |>
  rowwise() |>
  mutate(
    prob_mid = interp[[as.character(country)]]$saf(esaf_median),
    prob_lo  = interp[[as.character(country)]]$saf(esaf_lower),
    prob_hi  = interp[[as.character(country)]]$saf(esaf_upper)
  ) |>
  ungroup() |>
  group_by(country) |>
  summarise(prob_lo  = min(prob_lo,  prob_mid, na.rm = TRUE),
            prob_hi  = max(prob_hi,  prob_mid, na.rm = TRUE),
            prob_mid = median(prob_mid, na.rm = TRUE), .groups = "drop") |>
  mutate(policy = "Clean fuel transition (eSAF + ETS)")

gbf_ranges <- scenarios |>
  distinct(country, pathway = gbf_pathway, gbf_median, gbf_lower, gbf_upper) |>
  rowwise() |>
  mutate(
    prob_mid = interp[[as.character(country)]]$gbf(gbf_median),
    prob_lo  = interp[[as.character(country)]]$gbf(gbf_lower),
    prob_hi  = interp[[as.character(country)]]$gbf(gbf_upper)
  ) |>
  ungroup() |>
  group_by(country) |>
  summarise(prob_lo  = min(prob_lo,  prob_mid, na.rm = TRUE),
            prob_hi  = max(prob_hi,  prob_mid, na.rm = TRUE),
            prob_mid = median(prob_mid, na.rm = TRUE), .groups = "drop") |>
  mutate(policy = "Geologically balanced fuels (GBF)")

policy_levels <- c("Clean fuel transition (eSAF + ETS)",
                   "Geologically balanced fuels (GBF)")

plot_data_b <- bind_rows(saf_ranges, gbf_ranges) |>
  mutate(country = factor(country, levels = country_order),
         policy  = factor(policy, levels = policy_levels))

# ── Panel B: country on x-axis ───────────────────────────────────────────────

p_b <- ggplot(plot_data_b,
                      aes(x = country, y = prob_mid, ymin = prob_lo, ymax = prob_hi,
                          color = policy, group = policy)) +
  geom_pointrange(position = position_dodge(width = 0.45),
                  linewidth = 0.7, size = 0.45, key_glyph = "path") +
  geom_neutral_line(0.5) +
  color_scale +
  scale_y_continuous(
    limits = c(0.20, 0.85),
    breaks = c(0.25, 0.50, 0.75),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  theme_clean(base_size = plot_font_size) +
  guides(color = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(face = "bold")) +
  labs(y = "Choice\nprobability")

# ── Assemble and save ─────────────────────────────────────────────────────────

plot_w <- plot_size$wide$width * 1.2
plot_h <- (plot_size$strip$height + 1) * 1.6

combined <- (p_a / p_b) +
  plot_layout(guides = "collect", heights = c(1.7, 0.8)) +
  plot_annotation(tag_levels = "a") &
  theme(legend.position  = "right",
        legend.key.height = unit(2.5, "lines"),
        plot.tag          = element_text(face = "bold"))

ggsave(out_file, combined, width = plot_w, height = plot_h)
message("Combined breakeven plot saved: ", out_file)
