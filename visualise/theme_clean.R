library(ggplot2)

# Single accent color for plots with no attribute grouping to encode (e.g.
# policy types, where each row is already its own top-level category).
# Deliberately not one of the attribute_colors below, so it never implies a
# false association with a specific attribute.
accent_color <- "#e15759"

# One color per conjoint attribute, fixed so they stay consistent across
# figures. Each color is "claimed" by exactly one attribute and should not be
# reused elsewhere (e.g. for level-level distinctions within an attribute -
# use shape/linetype for that instead, see durability in
# durability_cost_plot.R).
attribute_colors <- c(
  "Fuel"                    = "#4e79a7",
  "Offsetting activity"     = "#f28e2b",
  "Durability of offsets"   = "#59a14f",
  "Responsible actors"      = "#b07aa1",
  "Increase in ticket cost" = "#76b7b2"
)

scale_color_attribute <- function(...) {
  scale_color_manual(values = attribute_colors, ...)
}

theme_clean <- function(base_size = 11, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line        = element_line(linewidth = 0.3, color = "black"),
      axis.ticks       = element_line(linewidth = 0.3, color = "black"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "none"
    )
}

# Thin, low-contrast reference line for neutral/midpoint values.
geom_neutral_line <- function(yintercept) {
  geom_hline(
    yintercept = yintercept,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.3
  )
}

# Axis limits centered on `midline` (e.g. the scale's neutral point), so the
# reference line sits in the middle of the plot and over/under support reads
# at a glance.
symmetric_limits <- function(values, midline) {
  half_range <- max(abs(range(values, na.rm = TRUE) - midline))
  c(midline - half_range, midline + half_range)
}

# Sparse, fixed tick labels for choice-probability axes (0-1 scale), instead
# of ggplot's default breaks.
choice_breaks <- c(0.4, 0.5, 0.6)

# Widen axis limits to cover fixed breaks that fall outside the raw data
# range (otherwise ggplot silently drops those tick labels).
widen_limits_to_breaks <- function(limits, breaks) {
  if (identical(breaks, waiver()) || is.null(breaks)) {
    return(limits)
  }
  range(c(limits, breaks))
}
