library(dplyr)
library(ggplot2)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  cor_file    <- snakemake@input[["correlations"]]
  pooled_out  <- snakemake@output[["pooled_plot"]]
  country_out <- snakemake@output[["country_plot"]]
} else {
  cor_file    <- here("data", "profile_predictor_correlations.csv")
  pooled_out  <- here("output", "supp-figs", "lpa_predictor_correlations.png")
  country_out <- here("output", "supp-figs", "lpa_predictor_correlations_country.png")
}

# Lower-triangle heatmaps of the Spearman correlations between the profile
# membership predictors: pooled (with values), and one panel per country.
# Variables in the order of the predictor figure.

var_labels <- c(
  flyer_type               = "Flying frequency",
  concern_group            = "Aviation climate concern",
  male                     = "Male",
  income_cat               = "Income",
  education_cat            = "Education",
  age                      = "Age",
  publicinput_co2na        = "Public input in decisions",
  actor_role_airlines      = "Resp.: airlines",
  actor_role_fuelsuppliers = "Resp.: fuel suppliers",
  actor_role_government    = "Resp.: government",
  actor_role_manufacturers = "Resp.: manufacturers",
  actor_role_nonprofit     = "Resp.: nonprofits",
  actor_role_passengers    = "Resp.: passengers",
  actor_role_researchers   = "Resp.: researchers",
  dailylife_actions_1      = "Turn off lights",
  dailylife_actions_2      = "Cut down heating/AC",
  dailylife_actions_3      = "Only run full loads",
  dailylife_actions_4      = "Switch off standby",
  dailylife_actions_5      = "Air-dry laundry",
  dailylife_actions_6      = "Public transport, walk, cycle",
  ev_sometimes             = "Electric car: at least sometimes"
)

cors <- read_csv(cor_file, show_col_types = FALSE) |>
  mutate(i = match(var1, names(var_labels)), j = match(var2, names(var_labels))) |>
  filter(i > j) |>
  mutate(var1 = factor(var_labels[var1], levels = rev(var_labels)),
         var2 = factor(var_labels[var2], levels = var_labels),
         country = factor(country, levels = c("All countries", country_levels)))

fill_scale <- scale_fill_gradient2(
  low = correlation_colors[["negative"]], mid = "white",
  high = correlation_colors[["positive"]], midpoint = 0, limits = c(-1, 1),
  breaks = seq(-1, 1, 0.5), name = "Spearman\ncorrelation"
)

heatmap <- function(d, values = FALSE) {
  ggplot(d, aes(x = var2, y = var1, fill = rho)) +
    geom_tile(color = "white", linewidth = 0.3) +
    { if (values) geom_text(aes(label = sub("^(-?)0\\.", "\\1.",
                                            sprintf("%.2f", round(rho, 2) + 0))),
                            size = paper_text_size * 0.75, color = attribute_color) } +
    fill_scale +
    coord_fixed() +
    theme_paper(legend = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.ticks  = element_blank(),
          axis.line   = element_blank(),
          panel.grid  = element_blank()) +
    labs(x = NULL, y = NULL)
}

p_pooled <- heatmap(filter(cors, country == "All countries"), values = TRUE)
save_paper_figure(p_pooled, pooled_out, height_mm = 165)

p_country <- heatmap(filter(cors, country != "All countries")) +
  facet_wrap(~ country, ncol = 2) +
  theme(axis.text = element_text(size = paper_font_size * 0.7),
        strip.text = element_text(face = "bold", size = paper_font_size))
save_paper_figure(p_country, country_out, height_mm = 240)
message("Predictor correlation plots saved: ", pooled_out, ", ", country_out)
