library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(viridis)
library(here)

if (exists("snakemake")) {
  framing_choice_emm   <- snakemake@input[["framing_choice"]]
  framing_rating_emm   <- snakemake@input[["framing_rating"]]
  nz_choice_emm   <- snakemake@input[["nz_choice"]]
  nz_rating_emm   <- snakemake@input[["nz_rating"]]
  choice_out <- snakemake@output[["framing_choice_plot"]]
  rating_out <- snakemake@output[["framing_rating_plot"]]
  nz_summary_out <- snakemake@output[["nz_summary"]]
} else {
  framing_choice_emm   <- here("data", "framing_choice_emm.csv")
  framing_rating_emm   <- here("data", "framing_rating_emm.csv")
  nz_choice_emm   <- here("data", "nz_choice_emm.csv")
  nz_rating_emm   <- here("data", "nz_rating_emm.csv")
  choice_out <- here("output", "framing_choice_plot.png")
  rating_out <- here("output", "framing_rating_plot.png")
  nz_summary_out <- here("output", "nz_summary.txt")
}


framing_choice_df <- read_csv(framing_choice_emm, show_col_types = FALSE) |>
  mutate(
    framing = recode(framing,
      `0` = "No information",
      `1` = "Net-zero information"
    ),
    framing = factor(
      framing,
      levels = c("No information", "Net-zero information")
    )
  )

framing_rating_df <- read_csv(framing_rating_emm, show_col_types = FALSE) |>
  rename(
    prob = emmean,
    asymp.LCL = lower.CL,
    asymp.UCL = upper.CL
  ) |>
  mutate(
    framing = recode(framing,
      `0` = "No information",
      `1` = "Net-zero information"
    ),
    framing = factor(
      framing,
      levels = c("No information", "Net-zero information")
    )
  )

nz_choice <- read_csv(nz_choice_emm, show_col_types = FALSE) |>
  mutate(
    framing = recode(framing,
      `0` = "No information",
      `1` = "Net-zero information"
    ),
    framing = factor(
      framing,
      levels = c("No information", "Net-zero information")
    ),
    nz_binary = recode(nz_binary,
      `0` = "Not aligned with net-zero",
      `1` = "Aligned with net-zero"
    ),
    nz_binary = factor(
      nz_binary,
      levels = c("Not aligned with net-zero", "Aligned with net-zero")
    )
  )

nz_rating <- read_csv(nz_rating_emm, show_col_types = FALSE) |>
  mutate(
    framing = recode(framing,
      `0` = "No information",
      `1` = "Net-zero information"
    ),
    framing = factor(
      framing,
      levels = c("No information", "Net-zero information")
    ),
    nz_binary = recode(nz_binary,
      `0` = "Not aligned with net-zero",
      `1` = "Aligned with net-zero"
    ),
    nz_binary = factor(
      nz_binary,
      levels = c("Not aligned with net-zero", "Aligned with net-zero")
    )
  )


# -------------------
# Plot framing results
# -------------------

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

  empty_rows <- expand_grid(
    attribute = c("Fuel", "Offsetting activity", "Durability of offsets",
                  "Responsible actors", "Increase in ticket cost"),
    code = c("Fuel", "Offsetting activity", "Durability of offsets",
             "Responsible actors", "Increase in ticket cost"),
    framing = levels(df$framing)
  ) |>
    mutate(
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

  label_map <- tibble(code = levels(df$code)) |>
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
      axis.text.y = ggtext::element_markdown(),
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    scale_color_viridis(discrete = TRUE, end = .95, option = "D") +
    labs(
      x = NULL,
      y = y_label,
      color = "Attribute",
      title = title
    ) +
    facet_wrap(~ framing)
}

choice_plot <- plot_emm(
  framing_choice_df,
  title = "Choice probabilities by framing",
  y_label = "Marginal means"
)

rating_plot <- plot_emm(
  framing_rating_df,
  title = "Rating scores by framing",
  y_label = "Marginal means"
)

ggsave(choice_out, choice_plot, width = 8, height = 9)
ggsave(rating_out, rating_plot, width = 8, height = 9)

# -------------------
# Get text file with for net zero packages
# -------------------

nz_text <- c(
  "=== NZ EFFECT ON CHOICE (probabilities) ===",
  "",
  capture.output(print(nz_choice)),
  "",
  "=== NZ EFFECT ON RATINGS (means) ===",
  "",
  capture.output(print(nz_rating))
)

writeLines(nz_text, con = nz_summary_out)

message("Plots saved: ", choice_out, " & ", rating_out)
message("NZ summary saved: ", nz_summary_out)