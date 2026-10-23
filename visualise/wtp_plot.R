library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  wtp_file <- snakemake@input[["wtp"]]
  plot_out <- snakemake@output[["plot"]]
} else {
  wtp_file <- here("data", "wtp_country.csv")
  plot_out <- here("output", "wtp_plot.png")
}

premiums <- c(
  saf_vs_fossil          = "SAF over fossil fuels",
  permanent_vs_temporary = "Permanent over temporary offsets"
)

# Pooled estimate on top (bold), then countries; a gap separates the two
row_labels <- c("Pooled", country_levels)
row_pos    <- setNames(c(length(country_levels) + 1.6, rev(seq_along(country_levels))),
                       row_labels)
axis_labels <- if_else(row_labels == "Pooled", "<b>All countries</b>", row_labels)

wtp_df <- read_csv(wtp_file, show_col_types = FALSE) |>
  filter(code %in% names(premiums)) |>
  mutate(y = row_pos[country])

x_max <- ceiling(max(wtp_df$upper) / 20) * 20

plot_premium <- function(code_name) {
  d <- filter(wtp_df, code == code_name)
  ggplot(d, aes(x = wtp, y = y)) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = highlight_color,
                   linewidth = paper_ci_width) +
    geom_point(color = highlight_color, size = paper_point_size) +
    scale_y_continuous(breaks = row_pos, labels = axis_labels,
                       expand = expansion(add = 0.6)) +
    scale_x_continuous(breaks = seq(0, x_max, by = 20),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(xlim = c(0, x_max)) +
    theme_paper(legend = "none") +
    theme(axis.text.y  = ggtext::element_markdown(),
          axis.ticks.y = element_blank(),
          plot.title   = element_text(face = "bold", size = paper_font_size,
                                      hjust = 0.5)) +
    labs(title = premiums[[code_name]],
         x = "Willingness to pay (percentage points of ticket price)", y = NULL)
}

p_fuel       <- plot_premium("saf_vs_fossil")
p_durability <- plot_premium("permanent_vs_temporary") +
  theme(axis.text.y = element_blank())

combined <- (p_fuel | p_durability) + panel_tags()

save_paper_figure(combined, plot_out, height_mm = 70)
message("WTP plot saved: ", plot_out)
