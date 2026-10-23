library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  grid_file    <- snakemake@input[["grid"]]
  summary_file <- snakemake@input[["summary"]]
  out_file     <- snakemake@output[["combined"]]
} else {
  grid_file    <- here("data", "policy_breakeven_country_grid.csv")
  summary_file <- here("data", "policy_breakeven_country_summary.csv")
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

# Panel a shows every country; countries without a 2050 cost projection (UAE)
# get curves but no projected-fare dots, and are left out of panel b.
summary_df <- read_csv(summary_file, show_col_types = FALSE) |>
  filter(!is.na(gbf_central))
cost_countries <- intersect(country_levels, summary_df$country)

summary_df <- summary_df |>
  mutate(country = factor(country, levels = cost_countries))

prob_df <- read_csv(grid_file, show_col_types = FALSE) |>
  mutate(country = factor(country, levels = country_levels),
         policy  = factor(policy, levels = policies))

# ── Panel a: choice probability vs ticket price, projected fares marked ──────

# Every central 2050 projection lies below ~52%, so the axis stops at 60% and
# the ticks mark the three price levels tested in the survey.
x_max <- 60
tested_cost_levels <- c(10, 30, 50)

projected <- summary_df |>
  transmute(
    country,
    SAF_ETS_cost = esaf_central, SAF_ETS_prob = prob_saf_at_projected,
    GBF_cost     = gbf_central,  GBF_prob     = prob_gbf_at_projected
  ) |>
  pivot_longer(-country, names_to = c("policy", ".value"),
               names_pattern = "^(SAF_ETS|GBF)_(.*)$") |>
  mutate(policy = factor(policy, levels = policies))

projected <- projected |>
  mutate(country = factor(as.character(country), levels = country_levels))

p_a <- ggplot(filter(prob_df, cost_pct <= x_max), aes(x = cost_pct)) +
  geom_neutral_line(0.5) +
  geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper, fill = policy),
              alpha = 0.10) +
  geom_line(aes(y = prob, color = policy), linewidth = 0.6) +
  geom_point(data = projected,
             aes(x = cost, y = prob, fill = policy), shape = 21,
             color = "white", stroke = 0.6, size = 2.2) +
  facet_wrap(~country, nrow = 2) +
  color_scale + fill_scale +
  scale_x_continuous(breaks = tested_cost_levels,
                     labels = paste0("+", tested_cost_levels, "%"),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.3, 0.4, 0.5, 0.6, 0.7)) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0.3, 0.8)) +
  shared_theme +
  guides(color = guide_legend(override.aes = list(linewidth = 0.8))) +
  labs(x = "Ticket price increase", y = "Choice probability")

# ── Panel b: same price vs projected 2050 fares ──────────────────────────────

# "Same price" = both pathways at GBF's projected fare, so the GBF dot is the
# same in both columns and only the SAF dot moves.
scenario_levels <- c("If both cost\nthe same", "At projected\n2050 fares")

panel_b_df <- bind_rows(
  summary_df |> transmute(
    country, scenario = scenario_levels[1], policy = "SAF_ETS",
    prob = prob_saf_equal_price,
    lo = prob_saf_equal_price_lower, hi = prob_saf_equal_price_upper),
  summary_df |> transmute(
    country, scenario = scenario_levels[2], policy = "SAF_ETS",
    prob = prob_saf_at_projected,
    lo = prob_saf_at_projected_lower, hi = prob_saf_at_projected_upper),
  summary_df |> transmute(
    country, scenario = scenario_levels[1], policy = "GBF",
    prob = prob_gbf_at_projected,
    lo = prob_gbf_at_projected_lower, hi = prob_gbf_at_projected_upper),
  summary_df |> transmute(
    country, scenario = scenario_levels[2], policy = "GBF",
    prob = prob_gbf_at_projected,
    lo = prob_gbf_at_projected_lower, hi = prob_gbf_at_projected_upper)
) |>
  mutate(
    scenario = factor(scenario, levels = scenario_levels),
    policy   = factor(policy, levels = policies),
    country  = factor(country, levels = rev(cost_countries))
  )

dodge <- position_dodge(width = 0.5)

p_b <- ggplot(panel_b_df,
              aes(x = prob, y = country, color = policy,
                  group = factor(policy, levels = rev(policies)))) +
  geom_vline(xintercept = 0.5, color = reference_line_color, linetype = "dashed",
             linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), position = dodge, linewidth = 0.5) +
  geom_point(position = dodge, size = 1.8) +
  facet_wrap(~scenario, nrow = 1) +
  color_scale +
  scale_x_continuous(breaks = c(0.5, 0.6, 0.7)) +
  coord_cartesian(xlim = c(0.44, 0.76)) +
  shared_theme +
  guides(color = "none") +  # legend comes from panel a
  theme(axis.title.y    = element_blank(),
        axis.ticks.y    = element_blank(),
        axis.line.y     = element_blank()) +
  labs(x = "Choice probability")

# ── Assemble and save ────────────────────────────────────────────────────────

# One shared legend at the bottom (identical colour scales are merged)
combined <- (p_a | p_b) +
  plot_layout(widths = c(3, 1.9), guides = "collect") +
  panel_tags() &
  theme(legend.position = "bottom")

save_paper_figure(combined, out_file, height_mm = 85)
message("Combined breakeven plot saved: ", out_file)
