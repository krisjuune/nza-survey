library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(viridis)
library(here)

if (exists("snakemake")) {
  choice_file <- snakemake@input[["choice"]]
  rating_file <- snakemake@input[["rating"]]
  choice_out  <- snakemake@output[["choice_plot"]]
  rating_out  <- snakemake@output[["rating_plot"]]
} else {
  choice_file <- here("data", "choice_results.csv")
  rating_file <- here("data", "rating_results.csv")
  choice_out  <- here("output", "general_choice_conjoint.png")
  rating_out  <- here("output", "general_rating_conjoint.png")
}

choice_df <- read_csv(choice_file, show_col_types = FALSE)

rating_df <- read_csv(rating_file, show_col_types = FALSE) |>
  rename(
    prob = emmean,
    asymp.LCL = lower.CL,
    asymp.UCL = upper.CL
  )

plot_emm <- function(df, title = NULL, y_label = NULL) {

  df <- df |>
    mutate(
      code = case_when(
        code == "trees"       ~ "Nature-based offsets",
        code == "factory_ccs" ~ "Point source capture",
        code == "direct_air"  ~ "Direct air capture",
        code == "cookstoves"  ~ "Traditional offsets",
        code == "fossil"      ~ "Fossil fuels",
        code == "plants"      ~ "Biofuels",
        code == "electric"    ~ "Synthetic fuels",
        code == "temporary"   ~ "Temporary",
        code == "permanent"   ~ "Permanent",
        code == "fuel_suppliers" ~ "Fuel suppliers",
        code == "airline"       ~ "Airlines",
        code == "government"    ~ "Government",
        code == "passenger"     ~ "Passengers",
        code == "10"          ~ "10%",
        code == "30"          ~ "30%",
        code == "50"          ~ "50%",
        TRUE ~ as.character(code)
      ),
      attribute = case_when(
        attribute == "activity_code"       ~ "Offsetting activity",
        attribute == "fuel_code"           ~ "Fuel",
        attribute == "durability_code"     ~ "Durability of offsets",
        attribute == "responsibility_code" ~ "Responsible actors",
        attribute == "cost_code"           ~ "Increase in ticket cost",
        TRUE ~ attribute
      )
    )

  plot_levels <- c(
    "Fuel",
    "Fossil fuels", "Biofuels", "Synthetic fuels",
    "Offsetting activity",
    "Traditional offsets", "Direct air capture",
    "Point source capture", "Nature-based offsets",
    "Durability of offsets",
    "Temporary", "Permanent",
    "Responsible actors",
    "Fuel suppliers", "Airlines", "Government", "Passengers",
    "Increase in ticket cost",
    "10%", "30%", "50%"
  )

  empty_rows <- tibble(
    attribute = c("Fuel", "Offsetting activity", "Durability of offsets",
                  "Responsible actors", "Increase in ticket cost"),
    code = c("Fuel", "Offsetting activity", "Durability of offsets",
             "Responsible actors", "Increase in ticket cost"),
    prob = NA_real_,
    SE = NA_real_,
    df = NA_real_,
    asymp.LCL = NA_real_,
    asymp.UCL = NA_real_
  )

  df <- bind_rows(df, empty_rows)

  df <- df |>
    mutate(
      code = factor(code, levels = rev(plot_levels)),
      attribute = factor(attribute, levels = c(
        "Fuel",
        "Offsetting activity",
        "Durability of offsets",
        "Responsible actors",
        "Increase in ticket cost"
      ))
    )

  # Create labels AFTER factorization
  label_map <- tibble(
    code = levels(df$code)
  ) |>
    mutate(
      code_label = case_when(
        code %in% c(
          "Fuel",
          "Offsetting activity",
          "Durability of offsets",
          "Responsible actors",
          "Increase in ticket cost"
        ) ~ paste0("<b>", code, "</b>"),
        TRUE ~ code
      )
    )

  ggplot(df, aes(x = code, y = prob, color = attribute)) +
    geom_point(size = 3, na.rm = TRUE) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 0.2, na.rm = TRUE) +
    scale_x_discrete(labels = setNames(label_map$code_label, label_map$code)) +
    coord_flip() +
    theme_classic(base_size = 14) +
    theme(
      axis.text.y = ggtext::element_markdown()
    ) +
    scale_color_viridis(
      discrete = TRUE,
      end = .95,
      option = "D"
    ) +
    labs(
      x = NULL,
      y = y_label,
      color = "Attribute",
      title = title
    )
}

choice_plot <- plot_emm(
  choice_df,
  title = "Estimated choice probabilities",
  y_label = "Marginal means"
)

rating_plot <- plot_emm(
  rating_df,
  title = "Estimated rating scores",
  y_label = "Marginal means"
)

ggsave(choice_out, choice_plot, width = 8, height = 12)
ggsave(rating_out, rating_plot, width = 8, height = 12)

message("Plots saved: ", choice_out, " & ", rating_out)