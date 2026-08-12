library(dplyr)
library(ggplot2)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  choice_emm_file <- snakemake@input[["choice"]]
  rating_emm_file <- snakemake@input[["rating"]]
  choice_out      <- snakemake@output[["choice_plot"]]
  rating_out      <- snakemake@output[["rating_plot"]]
} else {
  choice_emm_file <- here("data", "fuel_cost_choice_emm.csv")
  rating_emm_file <- here("data", "fuel_cost_rating_emm.csv")
  choice_out      <- here("output", "interact_fuel_wtp.png")
  rating_out      <- here("output", "supp_figs", "interact_fuel_wtp_rating.png")
}


recode_fuel_cost <- function(df) {
  df |>
    mutate(
      fuel_binary = factor(fuel_binary, levels = c("Fossil fuels", "SAFs")),
      cost_code = recode(as.character(cost_code),
        "10" = "10%", "30" = "30%", "50" = "50%"
      ),
      cost_code = factor(cost_code, levels = c("10%", "30%", "50%")),
      country = factor(country, levels = country_levels)
    )
}

choice_df <- read_csv(choice_emm_file, show_col_types = FALSE) |>
  recode_fuel_cost()

rating_df <- read_csv(rating_emm_file, show_col_types = FALSE) |>
  rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL) |>
  recode_fuel_cost()

plot_interaction <- function(df, y_label, midline, breaks = waiver(),
                              symmetric = FALSE) {
  y_limits <- if (symmetric) {
    symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), midline)
  } else {
    range(c(df$asymp.LCL, df$asymp.UCL), na.rm = TRUE)
  }
  y_limits <- widen_limits_to_breaks(y_limits, breaks)

  # Fuel shares its color with the main attribute plots (so color keeps one
  # consistent meaning across the whole figure set); SAFs vs Fossil fuels is
  # distinguished by shape/linetype instead of a second color.
  fuel_color <- attribute_colors[["Fuel"]]

  ggplot(df, aes(x = cost_code, y = prob, group = fuel_binary)) +
    geom_hline(yintercept = midline, color = "grey60", linetype = "dashed",
               linewidth = 0.3) +
    # Error bar drawn solid and behind the point, so the point's fill (white
    # for Fossil fuels) occludes the line passing through its center.
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      color = fuel_color, linetype = "solid",
      width = 0, position = position_dodge(width = 0.15)
    ) +
    geom_line(aes(linetype = fuel_binary), color = fuel_color,
              position = position_dodge(width = 0.15), linewidth = 0.5) +
    geom_point(aes(fill = fuel_binary), shape = 21,
               color = fuel_color, stroke = 0.8, size = 2.5,
               position = position_dodge(width = 0.15)) +
    scale_y_continuous(breaks = breaks, limits = y_limits) +
    scale_fill_manual(
      values = c("Fossil fuels" = "white", "SAFs" = fuel_color)
    ) +
    scale_linetype_manual(
      values = c("Fossil fuels" = "dashed", "SAFs" = "solid")
    ) +
    theme_clean() +
    theme(legend.position = "bottom") +
    labs(
      x = "Increase in ticket cost",
      y = y_label,
      fill = "Fuel type",
      linetype = "Fuel type"
    ) +
    facet_wrap(~ country, nrow = 1)
}

choice_plot <- plot_interaction(
  choice_df,
  y_label = "Choice probability",
  midline = 0.5,
  breaks = choice_breaks
)

rating_plot <- plot_interaction(
  rating_df,
  y_label = "Estimated rating",
  midline = 3,
  symmetric = TRUE
)

ggsave(choice_out, choice_plot, width = 11, height = 4)
ggsave(rating_out, rating_plot, width = 11, height = 4)

message("Fuel x cost plots saved: ", choice_out, " & ", rating_out)
