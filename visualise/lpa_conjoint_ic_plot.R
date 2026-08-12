library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  ic_file  <- snakemake@input[["ic"]]
  plot_out <- snakemake@output[["ic_plot"]]
} else {
  ic_file  <- here("data", "lpa_conjoint_ic.csv")
  plot_out <- here("output", "supp_figs", "lpa_conjoint_ic.png")
}

ic_df <- read_csv(ic_file, show_col_types = FALSE)

ic_long <- ic_df |>
  select(G, AIC, BIC, SABIC, AWE, ICL) |>
  # Convert to minimisation form (−2·loglik + penalty): positive values, lower = better
  mutate(across(c(AIC, BIC, SABIC, AWE, ICL), ~ -.x)) |>
  pivot_longer(
    cols      = c(AIC, BIC, SABIC, AWE, ICL),
    names_to  = "criterion",
    values_to = "value"
  ) |>
  mutate(criterion = factor(criterion, levels = c("AIC", "BIC", "SABIC", "AWE", "ICL")))

ic_plot <- ggplot(ic_long, aes(x = G, y = value)) +
  geom_line(color = accent_color, linewidth = 0.7) +
  geom_point(color = accent_color, size = point_size) +
  scale_x_continuous(breaks = 1:8) +
  facet_wrap(~ criterion, scales = "free_y", nrow = 1) +
  theme_clean(base_size = 11) +
  labs(
    x = "Number of profiles",
    y = "Information criterion\n(lower = better)"
  )

ggsave(plot_out, ic_plot, width = 14, height = 4)
message("LPA information criteria plot saved: ", plot_out)
