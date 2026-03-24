library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(here)
library(janitor)

if (exists("snakemake")) {
  input_file <- snakemake@input[[1]]
  output_file <- snakemake@output[[1]]
} else {
  input_file <- here("raw-data", "GBF_150326.csv")
  output_file <- here("data", "conjoint_long.csv")
}

df <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  slice(-(1:2)) |>
  mutate(id = row_number()) |>
  clean_names()

n_resp  <- nrow(df)
n_tasks <- 6
n_alternatives  <- 2

expected_rows <- n_resp * n_tasks * n_alternatives

support_long <- df |>
  select(id, framing, matches("^x[1-6]_[ab]_support$")) |>
  pivot_longer(
    cols = -c(id, framing),
    names_to = "var",
    values_to = "support"
  ) |>
  mutate(
    task = as.integer(str_extract(var, "(?<=^x)[1-6]")),
    alternative  = str_extract(var, "(?<=_)[ab](?=_)")
  ) |>
  select(id, framing, task, alternative, support)

choice_long <- df |>
  select(id, matches("^x[1-6]_forced_choice$")) |>
  pivot_longer(
    cols = -id,
    names_to = "var",
    values_to = "forced_choice"
  ) |>
  mutate(
    task = as.integer(str_extract(var, "(?<=^x)[1-6]"))
  ) |>
  select(id, task, forced_choice)

outcomes_long <- support_long |>
  left_join(choice_long, by = c("id", "task")) |>
  mutate(
    forced_choice = as.character(forced_choice),
    binary_choice = case_when(
      forced_choice == "1" & alternative == "a" ~ 1,
      forced_choice == "2" & alternative == "b" ~ 1,
      TRUE ~ 0
    )
  )

attributes_long <- df |>
  select(id, matches("^js_task")) |>
  pivot_longer(
    cols = -id,
    names_to = "var",
    values_to = "value"
  ) |>
  mutate(
    task = as.integer(str_extract(var, "(?<=task)\\d+")),
    alternative = str_extract(var, "\\d(?=_code$|$)"),
    alternative = recode(alternative, "1" = "a", "2" = "b"),
    attribute = str_extract(var, "(fuel|activity|durability|responsibility|cost)"),
    type = if_else(str_detect(var, "_code$"), "code", "label")
  ) |>
  filter(!is.na(task), !is.na(alternative), !is.na(attribute), type == "code") |>
  mutate(name = paste0(attribute, "_", type)) |>
  select(id, task, alternative, name, value) |>
  pivot_wider(
    names_from = name,
    values_from = value
  )

if (nrow(attributes_long) != expected_rows) {
  stop(
    paste0(
      "Attribute reshaping failed!\n",
      "Expected rows: ", expected_rows, "\n",
      "Actual rows: ", nrow(attributes_long)
    )
  )
}

nz_long <- df |>
  select(id, matches("^[ab]_nz_binary_task[1-6]$")) |>
  pivot_longer(
    cols = -id,
    names_to = "var",
    values_to = "nz_binary"
  ) |>
  mutate(
    alternative = str_extract(var, "^[ab]"),
    task = as.integer(str_extract(var, "[1-6]$"))
  ) |>
  select(id, task, alternative, nz_binary)


final_df <- outcomes_long |>
  left_join(attributes_long, by = c("id", "task", "alternative")) |>
  left_join(nz_long, by = c("id", "task", "alternative"))

actual_rows <- nrow(final_df)

if (actual_rows != expected_rows) {
  stop(
    paste0(
      "Row count mismatch!\n",
      "Expected: ", expected_rows, "\n",
      "Actual: ", actual_rows
    )
  )
} else {
  message(
    paste0(
      "Row count check passed: ",
      actual_rows, " rows (",
      n_resp, " respondents × ",
      n_tasks, " tasks × ",
      n_alternatives, " alternatives)"
    )
  )
}

choice_check <- final_df |>
  group_by(id, task) |>
  summarise(sum_choice = sum(binary_choice), .groups = "drop")

if (any(choice_check$sum_choice != 1)) {
  stop("Binary choice check failed: not exactly one chosen alternative per task. This is expected in a test dataset.")
} else {
  message("Binary choice check passed.")
}

write_csv(final_df, output_file)