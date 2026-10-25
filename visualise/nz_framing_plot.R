library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  framing_choice_emm   <- snakemake@input[["framing_choice"]]
  framing_rating_emm   <- snakemake@input[["framing_rating"]]
  nz_choice_emm   <- snakemake@input[["nz_choice"]]
  nz_rating_emm   <- snakemake@input[["nz_rating"]]
  nz_summary_out <- snakemake@output[["nz_summary"]]
} else {
  framing_choice_emm   <- here("data", "framing_choice_emm.csv")
  framing_rating_emm   <- here("data", "framing_rating_emm.csv")
  nz_choice_emm   <- here("data", "nz_choice_emm.csv")
  nz_rating_emm   <- here("data", "nz_rating_emm.csv")
  nz_summary_out <- here("output", "text-files", "nz_summary.txt")
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

message("NZ summary saved: ", nz_summary_out)