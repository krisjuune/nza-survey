library(dplyr)
library(readr)
library(here)
library(janitor)
library(stringr)
library(lubridate)

if (exists("snakemake")) {
  input_file    <- snakemake@input[[1]]
  output_file   <- snakemake@output[[1]]
  use_test_data <- snakemake@config[["use_test_data"]]
} else {
  input_file    <- here("raw-data", "GBF_130526.csv")
  output_file   <- here("raw-data", "raw_data.csv")
  use_test_data <- TRUE
}

df <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  slice(-(1:2)) |>
  clean_names()

if (use_test_data) {
  # -------------------
  # Add js variables to test data
  # -------------------

  df_test  <- df |> filter(distribution_channel == "test")
  df_real  <- df |> filter(distribution_channel != "test")

  for (t in 1:6) {

    cols_task <- names(df) |> str_subset(paste0("^js_task", t, "_"))
    cols_nz   <- names(df) |> str_subset(paste0("^js_[ab]_nz_binary_task", t, "$"))

    cols_all <- c(cols_task, cols_nz)

    pool <- df_real |>
      select(all_of(cols_all)) |>
      filter(if_all(everything(), ~ !is.na(.)))

    if (nrow(pool) == 0) next
    sampled <- pool[sample(nrow(pool), nrow(df_test), replace = TRUE), ]
    df_test[, cols_all] <- sampled
  }

  df_test <- df_test |>
    mutate(
      framing = sample(na.omit(df_real$framing), n(), replace = TRUE)
    )

  df <- bind_rows(df_real, df_test)
}

# -------------------
# Filter valid responses
# -------------------

df <- df |>
  mutate(
    start_date = ymd_hms(start_date)
  )

if (use_test_data) {
  cutoff <- ymd_hms("2026-03-25 14:20:00")
  df <- df |>
    filter(
      start_date <= cutoff,
      distribution_channel == "test"
    )
} else {
  df <- df |>
    filter(distribution_channel == "anonymous")
}

# -------------------
# Reconstruct nz_binary from package attributes (real data only)
# -------------------

if (!use_test_data) {
  is_nz_aligned <- function(fuel, activity, durability) {
    as.integer(
      fuel %in% c("plants", "electric") |
      (activity %in% c("trees", "direct_air") & durability == "permanent")
    )
  }

  for (t in 1:6) {
    df[[paste0("js_a_nz_binary_task", t)]] <- is_nz_aligned(
      df[[paste0("js_task", t, "_fuel1_code")]],
      df[[paste0("js_task", t, "_activity1_code")]],
      df[[paste0("js_task", t, "_durability1_code")]]
    )
    df[[paste0("js_b_nz_binary_task", t)]] <- is_nz_aligned(
      df[[paste0("js_task", t, "_fuel2_code")]],
      df[[paste0("js_task", t, "_activity2_code")]],
      df[[paste0("js_task", t, "_durability2_code")]]
    )
  }
}

df <- df |>
  mutate(id = row_number())

write_csv(df, output_file)

message("Valid responses saved to: ", output_file)
message("Remaining rows: ", nrow(df))