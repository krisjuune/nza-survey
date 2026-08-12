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
  choice_emm_file <- here("data", "durability_cost_choice_emm.csv")
  rating_emm_file <- here("data", "durability_cost_rating_emm.csv")
  choice_out      <- here("output", "durability_cost_choice_plot.png")
  rating_out      <- here("output", "durability_cost_rating_plot.png")
}


recode_durability_cost <- function(df) {
  df |>
    mutate(
      durability_code = recode(durability_code,
        "temporary" = "Temporary",
        "permanent" = "Permanent"
      ),
      durability_code = factor(
        durability_code, levels = c("Temporary", "Permanent")
      ),
      cost_code = recode(as.character(cost_code),
        "10" = "10%", "30" = "30%", "50" = "50%"
      ),
      cost_code = factor(cost_code, levels = c("10%", "30%", "50%")),
      country = factor(country, levels = country_levels)
    )
}

choice_df <- read_csv(choice_emm_file, show_col_types = FALSE) |>
  recode_durability_cost()

rating_df <- read_csv(rating_emm_file, show_col_types = FALSE) |>
  rename(prob = emmean, asymp.LCL = lower.CL, asymp.UCL = upper.CL) |>
  recode_durability_cost()

plot_interaction <- function(df, y_label, midline, breaks = waiver(),
                              symmetric = FALSE) {
  y_limits <- if (symmetric) {
    symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), midline)
  } else {
    range(c(df$asymp.LCL, df$asymp.UCL), na.rm = TRUE)
  }
  y_limits <- widen_limits_to_breaks(y_limits, breaks)

  # Durability shares its color with the main attribute plots (so color keeps
  # one consistent meaning across the whole figure set); Temporary vs
  # Permanent is distinguished by shape/linetype instead of a second color.
  durability_color <- attribute_colors[["Durability of offsets"]]

  ggplot(df, aes(x = cost_code, y = prob, group = durability_code)) +
    geom_hline(yintercept = midline, color = "grey60", linetype = "dashed",
               linewidth = 0.3) +
    # Error bar drawn solid and behind the point, so the point's fill (white
    # for Temporary) occludes the line passing through its center.
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      color = durability_color, linetype = "solid",
      width = 0, position = position_dodge(width = 0.15)
    ) +
    geom_line(aes(linetype = durability_code), color = durability_color,
              position = position_dodge(width = 0.15), linewidth = 0.5) +
    geom_point(aes(fill = durability_code), shape = 21,
               color = durability_color, stroke = 0.8, size = 2.5,
               position = position_dodge(width = 0.15)) +
    scale_y_continuous(breaks = breaks, limits = y_limits) +
    scale_fill_manual(
      values = c(Temporary = "white", Permanent = durability_color)
    ) +
    scale_linetype_manual(values = c(Temporary = "dashed", Permanent = "solid")) +
    theme_clean() +
    theme(legend.position = "bottom") +
    labs(
      x = "Increase in ticket cost",
      y = y_label,
      fill = "Durability of offsets",
      linetype = "Durability of offsets"
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

message("Durability x cost plots saved: ", choice_out, " & ", rating_out)
