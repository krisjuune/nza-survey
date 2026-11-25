library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(tidyr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  grid_file    <- snakemake@input[["grid"]]
  out_file     <- snakemake@output[["combined"]]
} else {
  grid_file    <- here("data", "policy_breakeven_country_grid.csv")
  out_file     <- here("output", "policy_breakeven_combined.png")
}

policies      <- c("SAF_ETS", "GBF")
policy_names  <- setNames(policy_label(policies), policies)
policy_values <- c(SAF_ETS = policy_contrast_colors[["saf"]],
                   GBF     = policy_contrast_colors[["gbf"]])

color_scale <- scale_color_manual(name = NULL, values = policy_values,
                                  labels = policy_names, breaks = policies)
fill_scale  <- scale_fill_manual(values = policy_values, guide = "none")

shared_theme <- theme_paper()

# ── Load data ────────────────────────────────────────────────────────────────

prob_df <- read_csv(grid_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_levels),
         policy  = factor(policy, levels = policies))

# ── Panel a: choice probability vs ticket price ──────────────────────────────

# The axis stops at 60% (beyond the tested range only extrapolation); the
# ticks mark the three price levels tested in the survey.
x_max <- 60
tested_cost_levels <- c(10, 30, 50)

p_a <- ggplot(filter(prob_df, cost_pct <= x_max), aes(x = cost_pct)) +
  geom_neutral_line(0.5) +
  geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper, fill = policy),
              alpha = 0.10) +
  geom_line(aes(y = prob, color = policy), linewidth = 0.6) +
  facet_wrap(~country, nrow = 1) +
  color_scale + fill_scale +
  scale_x_continuous(breaks = tested_cost_levels,
                     labels = paste0("+", tested_cost_levels, "%"),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.3, 0.4, 0.5, 0.6, 0.7)) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0.3, 0.8)) +
  shared_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 0.8))) +
  labs(x = "Ticket price increase", y = "Choice probability")

# ── Panel b: slices of panel a at a common price and cheaper GBF ─────────────

# SAF + carbon storage stays at the reference price increase (+30%) in every
# column, so only the GBF dot moves;
# GBF's price increase is `discount`% smaller (e.g. 30% cheaper: +30% -> +21%).
# Values are read off the panel-a curves (same estimates and 95% CIs).
reference_cost <- 30
gbf_discounts  <- c(0, 10, 30, 50)

scenarios <- tibble(
  discount = gbf_discounts,
  gbf_cost = reference_cost * (1 - gbf_discounts / 100),
  scenario = if_else(
    gbf_discounts == 0,
    sprintf("Same price\n(GBF +%g%%)", reference_cost),
    sprintf("GBF %g%% cheaper\n(GBF +%g%%)", gbf_discounts,
            reference_cost * (1 - gbf_discounts / 100))
  )
)

# Read the grid at the nearest grid point (0.5 pp steps)
at_cost <- function(pol, cost) {
  prob_df |>
    filter(policy == pol) |>
    group_by(country) |>
    slice_min(abs(cost_pct - cost), n = 1, with_ties = FALSE) |>
    ungroup()
}

panel_b_df <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  bind_rows(at_cost("SAF_ETS", reference_cost),
            at_cost("GBF", scenarios$gbf_cost[i])) |>
    mutate(scenario = scenarios$scenario[i])
}))

# Countries top to bottom in the usual order; SAF row above GBF in each country
# (manual offsets, as position_dodge doesn't offset points and lineranges the
# same way on a continuous y axis)
row_pos      <- setNames(rev(seq_along(country_levels)), country_levels)
dodge_offset <- c(SAF_ETS = 0.15, GBF = -0.15)

panel_b_df <- panel_b_df |>
  mutate(
    scenario = factor(scenario, levels = scenarios$scenario),
    y        = row_pos[as.character(country)] + dodge_offset[as.character(policy)]
  )

p_b <- ggplot(panel_b_df, aes(x = prob, y = y, color = policy)) +
  geom_vline(xintercept = 0.5, color = reference_line_color, linetype = "dashed",
             linewidth = 0.3) +
  geom_linerange(aes(xmin = prob_lower, xmax = prob_upper),
                 linewidth = paper_ci_width) +
  geom_point(size = paper_point_size) +
  facet_wrap(~scenario, nrow = 1) +
  color_scale +
  scale_x_continuous(breaks = c(0.4, 0.5, 0.6, 0.7)) +
  scale_y_continuous(breaks = row_pos, labels = names(row_pos),
                     expand = expansion(add = 0.6)) +
  shared_theme +
  guides(color = "none") +  # legend comes from panel a
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Choice probability")

# ── Assemble and save ────────────────────────────────────────────────────────

# One shared legend at the bottom (identical colour scales are merged)
combined <- (p_a / p_b) +
  plot_layout(heights = c(1, 1.15), guides = "collect") +
  panel_tags() &
  theme(legend.position = "bottom")

save_paper_figure(combined, out_file, height_mm = 150)
message("Combined breakeven plot saved: ", out_file)
