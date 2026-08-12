library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  durability_choice_file <- snakemake@input[["durability_choice"]]
  fuel_choice_file       <- snakemake@input[["fuel_choice"]]
  combined_out           <- snakemake@output[["plot"]]
} else {
  durability_choice_file <- here("data", "durability_cost_choice_emm.csv")
  fuel_choice_file       <- here("data", "fuel_cost_choice_emm.csv")
  combined_out           <- here("output", "interact_combined_plot.png")
}

plot_color <- attribute_color

shared_theme <- list(
  theme_clean(base_size = base_font_size),
  theme(
    strip.text    = element_text(face = "bold"),
    panel.spacing = unit(10, "pt"),
    axis.title.y  = element_text(angle = 0, vjust = 0.5, hjust = 1)
  )
)

# -------------------------------------------------------------------
# Fuel type × cost (panel a)
# -------------------------------------------------------------------
choice_fuel_df <- read_csv(fuel_choice_file, show_col_types = FALSE) |>
  mutate(
    fuel_binary = factor(fuel_binary, levels = c("Fossil fuels", "SAFs")),
    cost_code = recode(as.character(cost_code),
      "10" = "10%", "30" = "30%", "50" = "50%"
    ),
    cost_code = factor(cost_code, levels = c("10%", "30%", "50%")),
    country = factor(country, levels = country_levels)
  )

y_limits_fuel <- widen_limits_to_breaks(
  range(c(choice_fuel_df$asymp.LCL, choice_fuel_df$asymp.UCL), na.rm = TRUE),
  choice_breaks
)

p_fuel <- ggplot(choice_fuel_df,
                 aes(x = cost_code, y = prob, group = fuel_binary)) +
  geom_hline(yintercept = 0.5, color = "grey60", linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    color = plot_color, linetype = "solid",
    width = 0, position = position_dodge(width = 0.15)
  ) +
  geom_line(aes(linetype = fuel_binary), color = plot_color,
            position = position_dodge(width = 0.15), linewidth = 0.5) +
  geom_point(aes(fill = fuel_binary), shape = 21,
             color = plot_color, stroke = 0.8, size = 2.5,
             position = position_dodge(width = 0.15)) +
  scale_y_continuous(breaks = choice_breaks, limits = y_limits_fuel) +
  scale_fill_manual(values = c("Fossil fuels" = "white", "SAFs" = plot_color),
                    guide = guide_legend(reverse = TRUE)) +
  scale_linetype_manual(values = c("Fossil fuels" = "dashed", "SAFs" = "solid"),
                        guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ country, nrow = 1) +
  shared_theme +
  theme(legend.position = "right") +
  labs(
    x        = "Ticket price increase",
    y        = "Choice\nprobability",
    fill     = "Fuel type",
    linetype = "Fuel type"
  )

# -------------------------------------------------------------------
# Durability × cost (panel b)
# -------------------------------------------------------------------
choice_durability_df <- read_csv(durability_choice_file, show_col_types = FALSE) |>
  mutate(
    durability_code = recode(durability_code,
      "temporary" = "Temporary",
      "permanent" = "Permanent"
    ),
    durability_code = factor(durability_code, levels = c("Temporary", "Permanent")),
    cost_code = recode(as.character(cost_code),
      "10" = "10%", "30" = "30%", "50" = "50%"
    ),
    cost_code = factor(cost_code, levels = c("10%", "30%", "50%")),
    country = factor(country, levels = country_levels)
  )

y_limits_dur <- widen_limits_to_breaks(
  range(c(choice_durability_df$asymp.LCL, choice_durability_df$asymp.UCL), na.rm = TRUE),
  choice_breaks
)

p_durability <- ggplot(choice_durability_df,
                       aes(x = cost_code, y = prob, group = durability_code)) +
  geom_hline(yintercept = 0.5, color = "grey60", linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    color = plot_color, linetype = "solid",
    width = 0, position = position_dodge(width = 0.15)
  ) +
  geom_line(aes(linetype = durability_code), color = plot_color,
            position = position_dodge(width = 0.15), linewidth = 0.5) +
  geom_point(aes(fill = durability_code), shape = 21,
             color = plot_color, stroke = 0.8, size = 2.5,
             position = position_dodge(width = 0.15)) +
  scale_y_continuous(breaks = choice_breaks, limits = y_limits_dur) +
  scale_fill_manual(values = c(Temporary = "white", Permanent = plot_color),
                    guide = guide_legend(reverse = TRUE)) +
  scale_linetype_manual(values = c(Temporary = "dashed", Permanent = "solid"),
                        guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~ country, nrow = 1) +
  shared_theme +
  theme(legend.position = "right") +
  labs(
    x        = "Ticket price increase",
    y        = "Choice\nprobability",
    fill     = "Offset durability",
    linetype = "Offset durability"
  )

# -------------------------------------------------------------------
# Combine with a/b tags
# -------------------------------------------------------------------
combined <- (p_fuel / p_durability) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(combined_out, combined,
       width  = plot_size$combined$width,
       height = plot_size$combined$height)
message("Combined interaction plot saved: ", combined_out)
