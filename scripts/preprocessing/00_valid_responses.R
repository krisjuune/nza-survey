library(dplyr)
library(readr)
library(here)
library(janitor)
library(stringr)
library(lubridate)

if (exists("snakemake")) {
  input_file  <- snakemake@input[[1]]
  output_file <- snakemake@output[[1]]
} else {
  input_file  <- here("raw-data", "GBF_250326.csv")
  output_file <- here("raw-data", "raw_data.csv")
}

df <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  slice(-(1:2)) |>
  clean_names()

# -------------------
# Add js variables to test data
# -------------------

df_test  <- df |> filter(distribution_channel == "test")
df_real  <- df |> filter(distribution_channel != "test")

task_cols <- function(task) {
  matches(paste0("^js_task", task, "_"))
}

nz_cols <- function(task) {
  matches(paste0("^js_[ab]_nz_binary_task", task, "$"))
}

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

# -------------------
# Filter valid (test) responses
# -------------------

df <- df |>
  mutate(
    start_date = ymd_hms(start_date)
  )

cutoff <- ymd_hms("2026-03-25 14:20:00")

df <- df |>
  filter(
    start_date <= cutoff,
    distribution_channel == "test"
  )

df <- df |>
  mutate(id = row_number())

write_csv(df, output_file)

message("Valid responses saved to: ", output_file)
message("Remaining rows: ", nrow(df))