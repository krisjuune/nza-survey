library(dplyr)
library(readr)
library(here)

source(here("scripts", "shared", "constants.R"))

if (exists("snakemake")) {
  wtp_file      <- snakemake@input[["wtp"]]
  baseline_file <- snakemake@input[["baseline"]]
  out_file      <- snakemake@output[["wtp_usd"]]
  txt_file      <- snakemake@output[["txt"]]
} else {
  wtp_file      <- here("data", "wtp_country.csv")
  baseline_file <- here("data", "airfare_baseline.csv")
  out_file      <- here("data", "wtp_country_usd.csv")
  txt_file      <- here("output", "text-files", "wtp_usd_summary.txt")
}

# Converts the %-of-ticket-price WTP premiums into nominal 2020 USD, using
# each country's own baseline average airfare from the cost paper's raw
# sheet (a real domestic/regional route, not a single global reference fare -
# see scripts/preprocessing/airfare_cost_scenarios.R). The "Pooled" (all
# countries) row has no single real baseline fare, so it uses the median of
# the per-country baseline fares - the same pooling convention used for the
# "All countries" breakeven estimate (policy_breakeven_country.R).

premiums <- c("saf_vs_fossil", "permanent_vs_temporary")

baseline <- read_csv(baseline_file, show_col_types = FALSE)

if (nrow(baseline) == 0) {
  stop("No baseline airfare data (empty ", baseline_file, "). ",
       "Nominal-USD WTP requires cost_source: cost_paper in config.yaml.")
}

pooled_fare <- median(baseline$baseline_fare_usd)

fares <- bind_rows(
  baseline |> select(country, baseline_fare_usd),
  tibble(country = "Pooled", baseline_fare_usd = pooled_fare)
)

wtp_usd <- read_csv(wtp_file, show_col_types = FALSE) |>
  filter(code %in% premiums) |>
  inner_join(fares, by = "country") |>
  mutate(
    wtp_usd   = wtp   / 100 * baseline_fare_usd,
    lower_usd = lower / 100 * baseline_fare_usd,
    upper_usd = upper / 100 * baseline_fare_usd
  ) |>
  select(country, code, wtp_pct = wtp, lower_pct = lower, upper_pct = upper,
        baseline_fare_usd, wtp_usd, lower_usd, upper_usd)

write_csv(wtp_usd, out_file)
message("Nominal-USD WTP written: ", out_file)

# ── Text summary ─────────────────────────────────────────────────────────────

fmt <- function(x, lo, hi) sprintf("$%.0f [$%.0f, $%.0f]", x, lo, hi)

lines <- wtp_usd |>
  mutate(txt = fmt(wtp_usd, lower_usd, upper_usd)) |>
  select(country, code, txt) |>
  tidyr::pivot_wider(names_from = code, values_from = txt)

txt <- c(
  "=== Willingness to pay, nominal 2020 USD ===",
  "wtp_usd = wtp_pct / 100 * baseline_fare_usd, using each country's own",
  "baseline average airfare from the cost paper (a real domestic/regional",
  "route per country, not one global reference fare). 'Pooled' uses the",
  sprintf("median baseline fare across countries ($%.2f).", pooled_fare),
  "",
  "--- Baseline fares (2020 USD) ---",
  capture.output(print(as.data.frame(fares), digits = 4, row.names = FALSE)),
  "",
  "--- WTP premiums (nominal USD) ---",
  capture.output(print(as.data.frame(lines), row.names = FALSE))
)
writeLines(txt, txt_file)
message(paste(txt, collapse = "\n"))
