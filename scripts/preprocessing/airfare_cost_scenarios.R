library(dplyr)
library(tidyr)
library(readr)
library(here)

if (exists("snakemake")) {
  airfare_file <- snakemake@input[["airfare"]]
  out_file     <- snakemake@output[["scenarios"]]
} else {
  airfare_file <- here("raw-data", "airfare.csv")
  out_file     <- here("data", "airfare_cost_scenarios.csv")
}

# CSV exported from "Results 2050 Different rate" Excel tab.
# Structure:
#   Rows 1-3:  metadata / blank
#   Row  4:    main column headers (merged cells → blanks for sub-columns)
#   Row  5:    sub-headers: cols 9-20 have Median / Lower / Upper
#   Rows 6+:   data (fuel pathway × country)
#
# Columns by position (1-indexed):
#   1  = Fuel pathway
#   2  = Region
#   18 = 2050 Airfare change — Median  (% vs kerosene, e.g. "38%")
#   19 = 2050 Airfare change — Lower
#   20 = 2050 Airfare change — Upper

parse_pct <- function(x) {
  x <- trimws(x)
  x[x == "-" | x == "" | is.na(x)] <- NA_character_
  as.numeric(sub("%", "", x))
}

raw <- read_csv(
  airfare_file,
  skip      = 5,   # skip rows 1-5 (metadata + header + sub-header)
  col_names = FALSE,
  show_col_types = FALSE
)

airfare <- raw |>
  filter(!is.na(X1), X1 != "") |>
  transmute(
    fuel_pathway  = trimws(X1),
    country       = trimws(X2),
    change_median = parse_pct(X18),
    change_lower  = parse_pct(X19),
    change_upper  = parse_pct(X20)
  ) |>
  filter(
    !is.na(change_median),
    !country %in% c("UAE", ""),
    fuel_pathway != "keresene"
  )

esaf_recode <- c(
  "Green DAC synfuel"         = "Green H2 + DAC eSAF",
  "Green point-source synfuel" = "Green H2 + PS eSAF",
  "Blue DAC synfuel"           = "Blue H2 + DAC eSAF",
  "Blue point-source synfuel"  = "Blue H2 + PS eSAF"
)
gbf_recode <- c(
  "Point-source GBF" = "PS GBF",
  "DAC GBF"          = "DAC GBF"
)

esaf <- airfare |>
  filter(fuel_pathway %in% names(esaf_recode)) |>
  mutate(esaf_pathway = esaf_recode[fuel_pathway]) |>
  select(country, esaf_pathway,
         esaf_median = change_median,
         esaf_lower  = change_lower,
         esaf_upper  = change_upper)

gbf <- airfare |>
  filter(fuel_pathway %in% names(gbf_recode)) |>
  mutate(gbf_pathway = gbf_recode[fuel_pathway]) |>
  select(country, gbf_pathway,
         gbf_median = change_median,
         gbf_lower  = change_lower,
         gbf_upper  = change_upper)

# All eSAF × GBF combinations per country.
# Differential = eSAF change - GBF change, matched by scenario
# (Lower/Upper refer to the kerosene price scenario: Lower kerosene →
# larger premium for both pathways, Upper kerosene → smaller premium).
scenarios <- cross_join(
  esaf |> rename(c_esaf = country),
  gbf  |> rename(c_gbf  = country)
) |>
  filter(c_esaf == c_gbf) |>
  rename(country = c_esaf) |>
  select(-c_gbf) |>
  mutate(
    x       = esaf_median - gbf_median,
    x_lower = esaf_lower  - gbf_lower,
    x_upper = esaf_upper  - gbf_upper
  ) |>
  select(country, esaf_pathway, gbf_pathway,
         esaf_median, esaf_lower, esaf_upper,
         gbf_median,  gbf_lower,  gbf_upper,
         x, x_lower, x_upper)

write_csv(scenarios, out_file)
message("Airfare cost scenarios written: ", out_file)
message(sprintf("  %d rows (%d countries × eSAF × GBF combinations)",
                nrow(scenarios), n_distinct(scenarios$country)))
