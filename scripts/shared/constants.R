if (exists("snakemake")) {
  cfg <- snakemake@config

  # Respondent groupings
  country_recode  <- unlist(cfg$country_recode)
  country_levels  <- unlist(cfg$country_levels)
  flyer_recode    <- unlist(cfg$flyer_recode)
  flyer_levels    <- unlist(cfg$flyer_levels)
  concern_levels  <- unlist(cfg$concern_levels)
  policy_levels   <- unlist(cfg$policy_levels)

  # Conjoint attributes
  attributes      <- unlist(cfg$attributes)
  plot_levels     <- unlist(cfg$plot_levels)
  attribute_headers <- unlist(cfg$attribute_headers)

  # Reproducibility
  random_seed <- cfg$random_seed

  # Plot aesthetics
  attribute_colors  <- unlist(cfg$attribute_colors)
  accent_color      <- cfg$accent_color
  concern_colors    <- unlist(cfg$concern_colors)
  profile_colors    <- unlist(cfg$profile_colors)
  plot_size         <- lapply(cfg$plot_size, function(s) list(width = s$width, height = s$height))
  point_size        <- cfg$point_size
  errorbar_width    <- cfg$errorbar_width

} else {
  # Fallback values for running scripts standalone (outside Snakemake).
  # Keep in sync with config.yaml.

  country_recode <- c(
    "1" = "Australia", "2" = "Brazil",  "3" = "Germany",
    "4" = "Kenya",     "5" = "Vietnam", "6" = "UAE"
  )
  country_levels <- c("Australia", "Brazil", "Germany", "Kenya", "Vietnam", "UAE")

  flyer_recode <- c(
    "Non- Flyer"       = "Never-flyer",
    "Infrequent Flyer" = "Infrequent/lapsed flyer",
    "Occasional Flyer" = "Occasional flyer",
    "Frequent Flyer"   = "Frequent flyer"
  )
  flyer_levels <- c(
    "Never-flyer", "Infrequent/lapsed flyer", "Occasional flyer", "Frequent flyer"
  )

  concern_levels <- c("Low", "Mid", "High")

  policy_levels  <- c("GBF", "CORSIA", "SAF_ETS", "VCM_SBTi", "VCM_status_quo")

  attributes <- c(
    "fuel_code", "activity_code", "durability_code",
    "responsibility_code", "cost_code"
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

  attribute_headers <- c(
    "Fuel", "Offsetting activity", "Durability of offsets",
    "Responsible actors", "Increase in ticket cost"
  )

  attribute_colors <- c(
    "Fuel"                    = "#4e79a7",
    "Offsetting activity"     = "#f28e2b",
    "Durability of offsets"   = "#59a14f",
    "Responsible actors"      = "#b07aa1",
    "Increase in ticket cost" = "#76b7b2"
  )

  accent_color <- "#e15759"

  concern_colors <- c(
    "Low"  = "#a8c8e8",
    "Mid"  = "#3a82c4",
    "High" = "#0d3a6e"
  )

  profile_colors <- c(
    "Profile 1" = "#66c2a5",
    "Profile 2" = "#fc8d62",
    "Profile 3" = "#8da0cb"
  )

  plot_size <- list(
    narrow = list(width =  8, height = 9),
    wide   = list(width = 11, height = 7),
    strip  = list(width = 11, height = 4)
  )

  point_size     <- 2.0
  errorbar_width <- 0.2

  random_seed <- 2024
}
