library(dplyr)
library(ggplot2)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  fit_indices_file <- snakemake@input[[1]]
  plot_out         <- snakemake@output[[1]]
} else {
  fit_indices_file <- here("data", "lpa_fit_indices.csv")
  plot_out         <- here("output", "lpa_fit_indices_plot.png")
}

df <- read_csv(fit_indices_file, show_col_types = FALSE) |>
  mutate(
    index     = factor(index, levels = c("AIC", "BIC", "SABIC", "ICL", "AWE")),
    item_type = recode(item_type,
      "raw"      = "Raw items",
      "centered" = "Within-person centered items"
    ),
    item_type = factor(
      item_type, levels = c("Raw items", "Within-person centered items")
    )
  )

plot <- ggplot(df, aes(x = G, y = value, color = item_type)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = sort(unique(df$G))) +
  scale_color_manual(values = c(
    "Raw items"                     = accent_color,
    "Within-person centered items" = "#4e79a7"
  )) +
  theme_clean(base_size = 11) +
  theme(legend.position = "bottom") +
  labs(
    x = "Number of profiles",
    y = "Information criterion (lower is better)",
    color = NULL
  ) +
  facet_wrap(~ index, scales = "free_y", nrow = 1)

ggsave(plot_out, plot, width = 13, height = 4)

message("LPA fit-indices elbow plot saved: ", plot_out)
