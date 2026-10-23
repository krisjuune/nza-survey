library(dplyr)
library(tidyr)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  airfare_file <- snakemake@input[["airfare"]]
  out_file     <- snakemake@output[["costs"]]
} else {
  airfare_file <- here(switch(cost_source,
    cost_paper = "raw-data/airfare.csv",
    published  = "raw-data/airfare_costs_published.csv"
  ))
  out_file <- here("data", "airfare_costs.csv")
}

# Output schema (one row per country x fuel pathway), shared by all sources.
# fare_change_* are 2050 airfare increases vs a kerosene counterfactual (%).
# low/high are the min/max across the source's scenarios (e.g. kerosene price),
# so low <= median <= high regardless of how the source labels them.
# gbf_share is only set for mixed GBF-SAF pathways.
cost_schema <- c(
  "source", "country", "fuel_family", "pathway", "gbf_share",
  "fare_change_median", "fare_change_low", "fare_change_high", "reference"
)
fuel_families <- c("eSAF", "GBF", "mixed")

parse_pct <- function(x) {
  x <- trimws(x)
  x[x == "-" | x == "" | is.na(x)] <- NA_character_
  as.numeric(sub("%", "", x))
}

# CSV exported from the cost paper's "Results 2050 Different rate" Excel tab.
#   Rows 1-5:  metadata, main headers, sub-headers (Median / Lower / Upper)
#   Rows 6+:   data (fuel pathway x country)
# Columns by position: 1 = fuel pathway, 2 = region,
#   18-20 = 2050 airfare change Median / Lower / Upper (e.g. "38%").
# Lower/Upper refer to the kerosene price scenario (lower kerosene -> larger
# premium), so "Lower" is usually the larger number.
read_cost_paper <- function(path) {
  pathway_recode <- c(
    "Green DAC synfuel"          = "eSAF: green H2 + DAC",
    "Green point-source synfuel" = "eSAF: green H2 + PS",
    "Blue DAC synfuel"           = "eSAF: blue H2 + DAC",
    "Blue point-source synfuel"  = "eSAF: blue H2 + PS",
    "Point-source GBF"           = "GBF: PS",
    "DAC GBF"                    = "GBF: DAC"
  )

  read_csv(path, skip = 5, col_names = FALSE, show_col_types = FALSE) |>
    filter(!is.na(X1), X1 != "") |>
    transmute(
      pathway_raw = trimws(X1),
      country     = trimws(X2),
      median      = parse_pct(X18),
      a           = parse_pct(X19),
      b           = parse_pct(X20)
    ) |>
    filter(pathway_raw %in% names(pathway_recode)) |>
    transmute(
      source             = "cost_paper",
      country,
      fuel_family        = sub(":.*$", "", pathway_recode[pathway_raw]),
      pathway            = unname(pathway_recode[pathway_raw]),
      gbf_share          = NA_real_,
      fare_change_median = median,
      fare_change_low    = pmin(a, b, median),
      fare_change_high   = pmax(a, b, median),
      reference          = "Cost paper (in revision)"
    )
}

# Published cost data, already in `cost_schema` (see
# scripts/preprocessing/airfare_costs_template.csv).
read_published <- function(path) {
  df <- read_csv(path, show_col_types = FALSE, comment = "#")
  missing <- setdiff(cost_schema, names(df))
  if (length(missing) > 0) {
    stop("Published cost file is missing columns: ", paste(missing, collapse = ", "))
  }
  bad_family <- setdiff(unique(df$fuel_family), fuel_families)
  if (length(bad_family) > 0) {
    stop("Unknown fuel_family values: ", paste(bad_family, collapse = ", "))
  }
  df |>
    mutate(
      source             = "published",
      fare_change_low    = pmin(fare_change_low, fare_change_median),
      fare_change_high   = pmax(fare_change_high, fare_change_median)
    )
}

costs <- switch(cost_source,
  cost_paper = read_cost_paper(airfare_file),
  published  = read_published(airfare_file),
  stop("Unknown cost_source: ", cost_source)
)

# Countries without a 2050 projection (e.g. UAE in the cost paper) drop out
# here; downstream scripts treat them as "no cost projection".
costs <- costs |>
  filter(!is.na(fare_change_median), country %in% country_levels) |>
  select(all_of(cost_schema)) |>
  arrange(country, fuel_family, pathway)

write_csv(costs, out_file)
message("Airfare costs written (source: ", cost_source, "): ", out_file)
message(sprintf("  %d rows; countries: %s", nrow(costs),
                paste(sort(unique(costs$country)), collapse = ", ")))
missing_ctry <- setdiff(country_levels, costs$country)
if (length(missing_ctry) > 0) {
  message("  no cost projection for: ", paste(missing_ctry, collapse = ", "))
}
