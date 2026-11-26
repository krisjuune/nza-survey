library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  durability_choice_file <- snakemake@input[["durability_choice"]]
  fuel_choice_file       <- snakemake@input[["fuel_choice"]]
  activity_choice_file   <- snakemake@input[["activity_choice"]]
  combined_out           <- snakemake@output[["plot"]]
} else {
  durability_choice_file <- here("data", "durability_cost_choice_emm.csv")
  fuel_choice_file       <- here("data", "fuel_cost_choice_emm.csv")
  activity_choice_file   <- here("data", "activity_cost_choice_emm.csv")
  combined_out           <- here("output", "supp-figs", "interact_combined_plot.png")
}

dodge <- position_dodge(width = 0.2)

# Choice probability by ticket price, one line per level of `group`.
# The first level of `levels` is drawn solid/filled, the second dashed/open.
# Each panel in its attribute colour (attribute_group_colors, as in the
# country choice figure).
plot_by_cost <- function(df, group, levels, legend_title,
                         ink = attribute_group_colors[[legend_title]]) {
  df <- df |>
    mutate(
      group     = factor(.data[[group]], levels = levels),
      cost_code = factor(cost_level_label(cost_code), levels = cost_levels),
      country   = factor(country, levels = country_levels)
    )

  y_breaks <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  y_limits <- range(c(df$asymp.LCL, df$asymp.UCL))

  ggplot(df, aes(x = cost_code, y = prob, group = group)) +
    geom_neutral_line(0.5) +
    geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL), color = ink,
                   linewidth = paper_ci_width, position = dodge) +
    geom_line(aes(linetype = group), color = ink,
              linewidth = paper_line_width, position = dodge) +
    geom_point(aes(fill = group), shape = 21, color = ink, stroke = 0.6,
               size = paper_point_size, position = dodge) +
    scale_fill_manual(values = setNames(c(ink, "white"), levels)) +
    scale_linetype_manual(values = setNames(c("solid", "22"), levels)) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    facet_wrap(~country, nrow = 1) +
    theme_paper() +
    labs(x = "Ticket price increase", y = "Choice probability",
         fill = legend_title, linetype = legend_title)
}

# ── A: fuel type × ticket price ──────────────────────────────────────────────

fuel_df <- read_csv(fuel_choice_file, show_col_types = FALSE) |>
  mutate(fuel_binary = recode(fuel_binary, "SAFs" = "Biofuels or synthetic fuels"))

p_fuel <- plot_by_cost(fuel_df, "fuel_binary",
                       levels = c("Biofuels or synthetic fuels", "Fossil fuels"),
                       legend_title = "Fuel")

# ── B: offsetting activity × ticket price ────────────────────────────────────

activity_labels <- c(Capture = "Direct air or point-source capture",
                     Offsets = "Traditional or nature-based offsets")
activity_df <- read_csv(activity_choice_file, show_col_types = FALSE) |>
  mutate(activity_binary = activity_labels[activity_binary])

p_activity <- plot_by_cost(activity_df, "activity_binary",
                           levels = unname(activity_labels),
                           legend_title = "Offsetting activity")

# ── C: offset durability × ticket price ──────────────────────────────────────

durability_df <- read_csv(durability_choice_file, show_col_types = FALSE) |>
  mutate(durability_code = level_label("durability_code", durability_code))

p_durability <- plot_by_cost(durability_df, "durability_code",
                             levels = c("Permanent", "Temporary"),
                             legend_title = "Durability of offsets")

# ── Assemble and save ────────────────────────────────────────────────────────

combined <- (p_fuel / p_activity / p_durability) + panel_tags()

save_paper_figure(combined, combined_out, height_mm = 170)
message("Combined interaction plot saved: ", combined_out)
