library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_file <- snakemake@input[["choice"]]
  nz_file     <- snakemake@input[["nz_choice"]]
  plot_out    <- snakemake@output[["choice_plot"]]
  results_out <- snakemake@output[["results_plot"]]
} else {
  choice_file <- here("data", "framing_effect_choice.csv")
  nz_file     <- here("data", "nz_choice_emm.csv")
  plot_out    <- here("output", "supp_figs", "country_framing_choice.png")
  results_out <- here("output", "supp_figs", "framing_results.png")
}

framing_levels <- c("No information", "Net-zero information")

# No-information arm in dark ink, net-zero arm in the secondary grey
framing_values <- c("No information" = attribute_color,
                    "Net-zero information" = secondary_color)

df <- read_csv(choice_file, show_col_types = FALSE) |>
  mutate(
    framing = recode(framing, `0` = "No information", `1` = "Net-zero information"),
    country = factor(country, levels = country_levels)
  )

plot <- plot_attribute_emm(df, x_label = "Choice probability", midline = 0.5,
                           breaks = choice_breaks, facet = "country",
                           group = "framing", group_values = framing_values,
                           group_name = NULL)

save_paper_figure(plot, plot_out, height_mm = 125)

# ── Combined figure: A = net-zero-aligned vs other packages, B = the plot above ──

nz_rows <- c("Net-zero aligned" = 1, "Not net-zero aligned" = 2)

nz <- read_csv(nz_file, show_col_types = FALSE) |>
  mutate(
    framing = factor(recode(framing, `0` = "No information",
                            `1` = "Net-zero information"), levels = framing_levels),
    package = if_else(nz_binary == 1, "Net-zero aligned", "Not net-zero aligned"),
    # same within-row offsets as the grouped dots of panel B
    y_pos = nz_rows[package] + c(0.25, -0.25)[as.integer(framing)],
    # one-panel facet so the title uses the same strip style as panel B
    panel = "Pooled effect across countries and attributes"
  )

nz_plot <- ggplot(nz, aes(x = prob, y = y_pos, color = framing)) +
  geom_vline(xintercept = 0.5, color = reference_line_color, linetype = "dashed",
             linewidth = 0.3) +
  geom_linerange(aes(xmin = asymp.LCL, xmax = asymp.UCL), linewidth = paper_ci_width) +
  geom_point(size = paper_point_size * 0.8) +
  scale_color_manual(values = framing_values, breaks = framing_levels, guide = "none") +
  scale_y_continuous(breaks = nz_rows, labels = names(nz_rows),
                     limits = c(0.4, 2.6), expand = expansion(add = 0)) +
  scale_x_continuous(breaks = seq(0.40, 0.60, by = 0.05)) +
  facet_wrap(~panel) +
  theme_paper(legend = "none") +
  theme(axis.ticks.y = element_blank()) +
  labs(x = "Choice probability", y = NULL)

results_plot <- (nz_plot + plot_spacer() +
                   plot_layout(widths = c(1, 1.4))) /
  plot +
  plot_layout(heights = c(0.16, 1)) +
  panel_tags()

save_paper_figure(results_plot, results_out, height_mm = 150)

message("Country framing-effect plots saved: ", plot_out, ", ", results_out)
