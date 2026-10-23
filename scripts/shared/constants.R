if (exists("snakemake")) {
  cfg <- snakemake@config

  # Respondent groupings
  country_recode  <- unlist(cfg$country_recode)
  country_levels  <- unlist(cfg$country_levels)
  flyer_recode    <- unlist(cfg$flyer_recode)
  flyer_levels    <- unlist(cfg$flyer_levels)
  concern_levels  <- unlist(cfg$concern_levels)
  policy_levels   <- unlist(cfg$policy_levels)
  policy_labels   <- cfg$policy_labels

  # Airfare cost projections
  cost_source       <- cfg$cost_source
  tested_cost_range <- unlist(cfg$tested_cost_range)

  # Conjoint attributes
  attributes      <- unlist(cfg$attributes)
  attribute_display <- cfg$attribute_display

  # Reproducibility
  random_seed <- cfg$random_seed

  # Plot aesthetics
  accent_color           <- cfg$accent_color
  attribute_color        <- cfg$attribute_color
  highlight_color        <- cfg$highlight_color
  policy_contrast_colors <- unlist(cfg$policy_contrast_colors)
  policy_color           <- cfg$policy_color
  secondary_color        <- cfg$secondary_color
  reference_line_color   <- cfg$reference_line_color
  axis_color             <- cfg$axis_color
  concern_colors    <- unlist(cfg$concern_colors)
  profile_colors    <- unlist(cfg$profile_colors)
  plot_size         <- lapply(cfg$plot_size, function(s) list(width = s$width, height = s$height))
  point_size        <- cfg$point_size
  errorbar_width    <- cfg$errorbar_width
  base_font_size    <- cfg$base_font_size
  save_pdf          <- isTRUE(as.logical(cfg$save_pdf))  # also accepts "true" from --config

} else {
  # Fallback values for running scripts standalone (outside Snakemake).
  # Keep in sync with config.yaml.

  country_recode <- c(
    "1" = "Australia", "2" = "Brazil",  "3" = "Germany",
    "4" = "Kenya",     "5" = "Vietnam", "6" = "UAE"
  )
  country_levels <- c("Australia", "Brazil", "Germany", "Kenya", "UAE", "Vietnam")

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

  policy_labels <- list(
    SAF_ETS        = list(long = "Clean fuel transition (SAF mandate + carbon removal)",
                          short = "SAF mandate + carbon removal"),
    GBF            = list(long = "Geologically balanced fuels (GBF)",          short = "GBF"),
    CORSIA         = list(long = "Mandatory airline offsetting (CORSIA-like)", short = "CORSIA-like"),
    VCM_SBTi       = list(long = "Voluntary carbon removal (SBTi-aligned)",    short = "SBTi-aligned"),
    VCM_status_quo = list(long = "Voluntary offsetting (status quo)",          short = "Status quo")
  )

  cost_source       <- "cost_paper"
  tested_cost_range <- c(10, 50)

  attributes <- c(
    "fuel_code", "activity_code", "durability_code",
    "responsibility_code", "cost_code"
  )

  attribute_display <- list(
    fuel_code = list(label = "Fuel", levels = list(
      fossil = "Fossil fuels", plants = "Biofuels", electric = "Synthetic fuels")),
    activity_code = list(label = "Offsetting activity", levels = list(
      cookstoves = "Traditional offsets", direct_air = "Direct air capture",
      factory_ccs = "Point source capture", trees = "Nature-based offsets")),
    durability_code = list(label = "Durability of offsets", levels = list(
      temporary = "Temporary", permanent = "Permanent")),
    responsibility_code = list(label = "Responsible actor", levels = list(
      fuel_suppliers = "Fuel suppliers", airline = "Airlines",
      government = "Government", passenger = "Passengers")),
    cost_code = list(label = "Ticket price increase", levels = list(
      "10" = "+10%", "30" = "+30%", "50" = "+50%"))
  )

  accent_color           <- "#333333"
  attribute_color        <- "#333333"
  highlight_color        <- "#3f3d99"
  policy_contrast_colors <- c(saf = "#4575b4", gbf = "#d73027")
  policy_color           <- "#d73027"
  secondary_color        <- "#999999"
  reference_line_color   <- "#999999"
  axis_color             <- "#000000"

  concern_colors <- c(
    "Low"  = "#a8c8e8",
    "Mid"  = "#3a82c4",
    "High" = "#0d3a6e"
  )

  profile_colors <- c(
    "Profile 1" = "#1b9e77",
    "Profile 2" = "#b07d1a",
    "Profile 3" = "#9b4f96"
  )

  plot_size <- list(
    narrow     = list(width =  8,   height = 9),
    wide       = list(width = 11,   height = 7),
    strip      = list(width = 11,   height = 4),
    strip_wide = list(width = 13.5, height = 4),
    combined   = list(width = 13.5, height = 8)
  )

  point_size     <- 2.0
  errorbar_width <- 0
  base_font_size <- 12
  save_pdf       <- FALSE

  random_seed <- 2024
}

# Derived from attribute_display (shared by both branches above):
# attribute_headers = attribute labels in display order; plot_levels = the
# y-axis order, each header followed by its level labels.
attribute_headers <- vapply(attribute_display, function(a) a$label, character(1),
                            USE.NAMES = FALSE)
plot_levels <- unlist(lapply(attribute_display, function(a) c(a$label, unlist(a$levels))),
                      use.names = FALSE)
