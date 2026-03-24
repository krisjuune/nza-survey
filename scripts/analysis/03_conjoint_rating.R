library(dplyr)
library(tidyr)
library(lme4)
library(Matrix)
library(emmeans)
library(readr)
library(here)

if (exists("snakemake")) {
  input_file  <- snakemake@input[[1]]
  output_file <- snakemake@output[[1]]
} else {
  input_file  <- here("data", "conjoint_long.csv")
  output_file <- here("data", "rating_results.csv")
}

df <- read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = cols(
    cost_code = col_character()
  )
)

model <- suppressWarnings(
  lmer(
    support ~ fuel_code + activity_code + durability_code + responsibility_code + cost_code +
      (1 | id),
    data = df
  )
)

attributes <- c("fuel_code", "activity_code", "durability_code", "responsibility_code", "cost_code")

emm_list <- lapply(attributes, function(attr) {
  emmeans(model, as.formula(paste0("~ ", attr))) |>
    as.data.frame() |>
    mutate(attribute = attr)
})

emm_df <- bind_rows(emm_list) |>
  select(attribute, everything())

emm_long <- emm_df |>
  pivot_longer(
    cols = ends_with("_code"),
    names_to = "tmp",
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  select(attribute, code, emmean, SE, df, lower.CL, upper.CL)

write_csv(emm_long, output_file)
message("Conjoint rating analysis completed: marginal means saved to ", output_file)