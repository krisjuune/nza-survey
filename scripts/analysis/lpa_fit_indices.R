library(dplyr)
library(tidyr)
library(readr)
library(here)
library(mclust)

if (exists("snakemake")) {
  covariates_file <- snakemake@input[[1]]
  output_file     <- snakemake@output[[1]]
} else {
  covariates_file <- here("data", "covariates.csv")
  output_file     <- here("data", "lpa_fit_indices.csv")
}

covariates <- read_csv(covariates_file, show_col_types = FALSE)

lpa_items <- c(
  "pref_path_trad_offset", "measure_achiev_trad_offset",
  "pref_path_geol_offset", "measure_achiev_geol_offset",
  "pref_path_safs", "measure_achiev_safs",
  "pref_path_synfuels", "measure_achiev_synfuels",
  "pref_path_electricplanes", "measure_achiev_electrification"
)

raw_items <- covariates |> select(all_of(lpa_items))

# Within-person centering removes each respondent's overall response level
# (e.g. acquiescence/enthusiasm), leaving only their relative preference across
# the 5 pathways. Centering on the row mean makes the items exactly collinear
# (every respondent's centered values sum to 0, whatever set they're centered
# over) - the classic ipsatized-data problem, which degenerates covariance-
# based mixture models (especially G=1). Fixed the same way as a reference
# level in dummy coding: center on the full 10-item row mean (so it still
# reflects each respondent's overall level across all 5 pathways), then drop
# one of the resulting columns so the remaining 9 are full rank - dropping
# after centering, rather than excluding it from the mean, is what actually
# breaks the sum-to-zero constraint.
centered_items <- (raw_items - rowMeans(raw_items)) |>
  select(-measure_achiev_electrification)

item_sets <- list(
  raw      = scale(raw_items),
  centered = scale(centered_items)
)

g_range <- 1:10

# ----------------------------
# Fit indices, all on the classic deviance scale (lower is better), so AIC,
# BIC, SABIC, ICL and AWE can be plotted together on the same footing.
#   AIC   = -2*LL + 2*npar
#   BIC   = -2*LL + npar*log(n)                    (Schwarz, 1978)
#   SABIC = -2*LL + npar*log((n+2)/24)              (Sclove, 1987)
#   ICL   = BIC + 2*sum_i log(z_i,MAP)              (Biernacki et al., 2000;
#           mclust's icl() is exactly this, on its own higher-is-better scale)
#   AWE   = -2*(LL + EN) + 2*npar*(1.5 + log(n))    (Banfield & Raftery, 1993)
#           where EN = sum(z * log(z)) (classification/entropy term, <= 0)
# ----------------------------
fit_indices <- function(model) {
  ll    <- model$loglik
  n     <- model$n
  npar  <- model$df
  z     <- model$z
  en    <- sum(z * log(ifelse(z > 0, z, 1)))

  c(
    AIC   = -2 * ll + 2 * npar,
    BIC   = -2 * ll + npar * log(n),
    SABIC = -2 * ll + npar * log((n + 2) / 24),
    ICL   = unname(-icl(model)),
    AWE   = -2 * (ll + en) + 2 * npar * (1.5 + log(n))
  )
}

ts_msg <- function(...) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
}

t0 <- Sys.time()

results <- lapply(names(item_sets), function(item_type) {
  data <- item_sets[[item_type]]
  lapply(g_range, function(g) {
    ts_msg(
      item_type, " items, G=", g, " (",
      round(as.numeric(difftime(Sys.time(), t0, units = "secs"))),
      "s elapsed)"
    )
    set.seed(2026)
    model <- Mclust(data, G = g, verbose = FALSE)
    tibble(
      item_type  = item_type,
      G          = g,
      model_name = model$modelName,
      index      = names(fit_indices(model)),
      value      = unname(fit_indices(model))
    )
  }) |>
    bind_rows()
}) |>
  bind_rows()

write_csv(results, output_file)
message("LPA fit indices written to: ", output_file)
