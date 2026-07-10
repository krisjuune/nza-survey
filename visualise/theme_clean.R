library(ggplot2)

source(here::here("scripts", "shared", "constants.R"))

scale_color_attribute <- function(...) {
  scale_color_manual(values = attribute_colors, ...)
}

# Lightened (toward white) version of each attribute color, for marking a
# secondary/contrast condition with an actual paler hue rather than alpha
# transparency - transparency would let anything drawn underneath (e.g. an
# error bar) show through the fill; a literal lighter color stays opaque.
lighten_color <- function(hex, factor = 0.55) {
  rgb_mat <- col2rgb(hex) / 255
  blended <- rgb_mat + (1 - rgb_mat) * factor
  apply(blended, 2, function(x) rgb(x[1], x[2], x[3]))
}

attribute_colors_light <- setNames(
  lighten_color(attribute_colors), names(attribute_colors)
)

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
