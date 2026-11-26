library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  wtp_file     <- snakemake@input[["wtp"]]
  wtp_usd_file <- snakemake@input[["wtp_usd"]]
  plot_out     <- snakemake@output[["plot"]]
} else {
  wtp_file     <- here("data", "wtp_country.csv")
  wtp_usd_file <- here("data", "wtp_country_usd.csv")
  plot_out     <- here("output", "wtp_plot.png")
}

# Two premiums (fuel, durability), each shown in relative terms (panels A/B:
# percentage points of ticket price) and in nominal terms (panels C/D: 2020
# USD, using each country's own baseline airfare - see
# scripts/analysis/wtp_usd.R). Same row layout and colour-coding
# (attribute_group_colors) in all four panels, so they read as one set.

premiums <- c(
  saf_vs_fossil          = "Biofuels or synthetic fuels\nover fossil fuels",
  geological_vs_offsets  = "Direct air or point-source capture over\ntraditional or nature-based offsets",
  permanent_vs_temporary = "Permanent over\ntemporary offsets"
)
legend_labels <- c(
  saf_vs_fossil          = "Fuel type",
  geological_vs_offsets  = "Offsetting activity",
  permanent_vs_temporary = "Durability of offsets"
)
premium_colors <- c(
  saf_vs_fossil          = attribute_group_colors[["Fuel"]],
  geological_vs_offsets  = attribute_group_colors[["Offsetting activity"]],
  permanent_vs_temporary = attribute_group_colors[["Durability of offsets"]]
)

# Pooled estimate on top (bold), then countries, each spaced a bit further
# apart than the default 1 unit for a less cramped feel; a wider gap
# separates the two groups, shared by every panel below.
row_step    <- 1.2
row_gap     <- 2.2
row_labels  <- c("Pooled", country_levels)
row_pos     <- setNames(
  c((length(country_levels) + 1) * row_step + row_gap,
    rev(seq_along(country_levels)) * row_step),
  row_labels
)
row_ylim <- range(row_pos) + c(-0.8, 0.8)
axis_labels <- if_else(row_labels == "Pooled", "<b>All countries</b>", row_labels)

base_theme <- function() {
  theme_paper(legend = "right") +
    theme(axis.text.y   = ggtext::element_markdown(),
          axis.ticks.y  = element_blank(),
          plot.title    = element_text(face = "bold", size = paper_font_size,
                                       hjust = 0.5),
          plot.margin   = margin(t = 4, r = 10, b = 4, l = 4, unit = "pt"))
}

# A row's x-axis title as its own thin, text-only "panel", centred under the
# two plots above it (rather than left panel's title looking off-centre once
# the right panel's duplicate is dropped).
axis_title_row <- function(text) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = text, size = paper_font_size / .pt) +
    theme_void()
}

# drop = FALSE: each panel's data has only one premium's rows, but the
# legend (shown once, from panel A) must still list both. A key for a level
# with zero rows in the panel's own data renders blank even with
# override.aes - panel A gets one real (in-range, alpha = 0) dummy row per
# level purely so both legend keys have a row to draw from; override.aes
# forces the legend copy back to full opacity and the right colour.
color_scale <- scale_color_manual(
  name = NULL, values = premium_colors, breaks = names(premiums),
  labels = legend_labels, drop = FALSE,
  guide = guide_legend(override.aes = list(
    color = unname(premium_colors[names(premiums)]), alpha = 1
  ))
)

# ── Panels A/B: relative WTP (percentage points of ticket price) ────────────

rel_df <- read_csv(wtp_file, show_col_types = FALSE) |>
  filter(code %in% names(premiums)) |>
  mutate(y = row_pos[country], code = factor(code, levels = names(premiums)))

rel_x_max <- ceiling(max(rel_df$upper) / 20) * 20
# The offsetting-activity premium is negative, so the shared axis extends
# below zero (dashed zero line marks no premium)
rel_x_min <- min(0, floor(min(rel_df$lower) / 20) * 20)

plot_relative <- function(code_name, title = TRUE) {
  d <- filter(rel_df, code == code_name)
  ggplot(d, aes(x = wtp, y = y, color = code)) +
    geom_vline(xintercept = 0, color = reference_line_color, linetype = "dashed",
               linewidth = 0.3) +
    geom_linerange(aes(xmin = lower, xmax = upper), linewidth = paper_ci_width) +
    geom_point(size = paper_point_size) +
    color_scale +
    scale_y_continuous(breaks = row_pos, labels = axis_labels,
                       expand = expansion(add = 0.8)) +
    scale_x_continuous(breaks = seq(rel_x_min, rel_x_max, by = 20),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(xlim = c(rel_x_min, rel_x_max), ylim = row_ylim) +
    base_theme() +
    labs(title = if (title) premiums[[code_name]] else NULL, x = NULL, y = NULL)
}

# ── Panels C/D: nominal WTP (2020 USD) ──────────────────────────────────────

usd_df <- read_csv(wtp_usd_file, show_col_types = FALSE) |>
  filter(code %in% names(premiums)) |>
  mutate(y = row_pos[country], code = factor(code, levels = names(premiums)))

usd_x_max <- ceiling(max(usd_df$upper_usd) / 10) * 10
usd_x_min <- min(0, floor(min(usd_df$lower_usd) / 10) * 10)

plot_usd <- function(code_name, title = TRUE) {
  d <- filter(usd_df, code == code_name)
  ggplot(d, aes(x = wtp_usd, y = y, color = code)) +
    geom_vline(xintercept = 0, color = reference_line_color, linetype = "dashed",
               linewidth = 0.3) +
    geom_linerange(aes(xmin = lower_usd, xmax = upper_usd), linewidth = paper_ci_width) +
    geom_point(size = paper_point_size) +
    color_scale +
    scale_y_continuous(breaks = row_pos, labels = axis_labels,
                       expand = expansion(add = 0.8)) +
    scale_x_continuous(breaks = seq(usd_x_min, usd_x_max, by = 10),
                       labels = scales::dollar_format(),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(xlim = c(usd_x_min, usd_x_max), ylim = row_ylim) +
    base_theme() +
    labs(title = if (title) premiums[[code_name]] else NULL, x = NULL, y = NULL)
}

# ── Assemble: relative estimates on top (tag A), nominal USD below (tag B).
# Titles (premium names) only on the top row - the legend on the right names
# them for both rows. Only the first panel keeps its (full two-entry) legend;
# the rest suppress theirs so there is exactly one, collected on the right.
# Each row's x-axis title is its own centred text row below the two panels,
# rather than a per-panel label (which would either repeat or look off-centre
# once the right panel's copy is dropped).

# legend_dummy: a real, in-range row for both levels (alpha = 0, so
# invisible in the panel itself) purely so the legend - collected from panel
# A only - has something to draw both keys from. Both the point and the
# linerange layers need their own dummy row, or the key for a level with no
# real linerange row shows only the dot, missing the CI dash.
legend_dummy <- tibble(code = factor(names(premiums), levels = names(premiums)),
                       dx = rel_x_max / 2, dy = mean(row_ylim))

p_a <- plot_relative("saf_vs_fossil") +
  geom_linerange(data = legend_dummy,
                 aes(x = dx, xmin = dx - 1, xmax = dx + 1, y = dy, color = code),
                 linewidth = paper_ci_width, alpha = 0, inherit.aes = FALSE) +
  geom_point(data = legend_dummy, aes(x = dx, y = dy, color = code),
            size = paper_point_size, alpha = 0, inherit.aes = FALSE) +
  labs(tag = "A")
no_rows <- list(guides(color = "none"), theme(axis.text.y = element_blank()))
p_a2 <- plot_relative("geological_vs_offsets") + no_rows
p_b  <- plot_relative("permanent_vs_temporary") + no_rows
p_c  <- plot_usd("saf_vs_fossil", title = FALSE) +
  guides(color = "none") +
  labs(tag = "B")
p_c2 <- plot_usd("geological_vs_offsets", title = FALSE) + no_rows
p_d  <- plot_usd("permanent_vs_temporary", title = FALSE) + no_rows

rel_label <- axis_title_row("Willingness to pay (percentage points of ticket price)")
usd_label <- axis_title_row("Willingness to pay (2020 USD)")

combined <- ((p_a | p_a2 | p_b) / rel_label / (p_c | p_c2 | p_d) / usd_label) +
  plot_layout(heights = c(1, 0.08, 1, 0.08), guides = "collect")

save_paper_figure(combined, plot_out, width_mm = 260, height_mm = 120)
message("WTP plot saved: ", plot_out)
