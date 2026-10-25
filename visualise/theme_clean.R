library(ggplot2)

source(here::here("scripts", "shared", "constants.R"))

theme_clean <- function(base_size = 11, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line        = element_line(linewidth = 0.3, color = axis_color),
      axis.ticks       = element_line(linewidth = 0.3, color = axis_color),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "none"
    )
}

# Thin, low-contrast reference line for neutral/midpoint values.
geom_neutral_line <- function(yintercept) {
  geom_hline(
    yintercept = yintercept,
    color = reference_line_color,
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

# Display labels for conjoint attributes and levels, from `attribute_display`
# in config.yaml. Unknown codes pass through unchanged.
attribute_label <- function(attribute) {
  vapply(as.character(attribute), function(a) {
    lab <- attribute_display[[a]]$label
    if (is.null(lab)) a else lab
  }, character(1), USE.NAMES = FALSE)
}

level_label <- function(attribute, code) {
  mapply(function(a, k) {
    lab <- attribute_display[[a]]$levels[[k]]
    if (is.null(lab)) k else lab
  }, as.character(attribute), as.character(code), USE.NAMES = FALSE)
}

# Relabel the `attribute` and `code` columns of an emmeans table in place
label_attributes <- function(df) {
  dplyr::mutate(df,
    code      = level_label(attribute, code),
    attribute = attribute_label(attribute)
  )
}

# Display name for policy package codes (e.g. "SAF_ETS"), from
# `policy_labels` in config.yaml. form = "long" (axis/legend) or "short" (tag).
# wrap = TRUE puts the bracketed tag of a long name on its own line.
policy_label <- function(code, form = c("long", "short"), wrap = FALSE) {
  form <- match.arg(form)
  out <- vapply(code, function(k) policy_labels[[k]][[form]], character(1),
                USE.NAMES = FALSE)
  if (wrap) out <- sub(" (", "\n(", out, fixed = TRUE)
  out
}

# Display order of the policy packages in every figure (top to bottom):
# the order of `policy_labels` in config.yaml.
policy_display_order <- names(policy_labels)

# ── Paper house style ────────────────────────────────────────────────────────
# Every figure (main and supplementary) is drawn at its final print size, so
# text is the same physical size everywhere:
#   width   180 mm (double column) or 88 mm (single column)
#   font    Helvetica 8 pt for axis titles, facet titles and legend text;
#           tick labels and in-plot annotations at 80% (6.4 pt)
#   bold    only panel tags, facet titles and group headers (attribute names,
#           table column heads); everything else plain
#   case    sentence case; panel tags A, B, C
#   marks   points 1.8, CI lines 0.5, data lines 0.6
paper_width_mm        <- 180
paper_width_single_mm <- 88
paper_font_size       <- 8
paper_point_size      <- 1.8
paper_ci_width        <- 0.5
paper_line_width      <- 0.6
# geom_text size matching tick labels (geom_text sizes are in mm)
paper_text_size       <- paper_font_size * 0.8 / ggplot2::.pt

theme_paper <- function(legend = "bottom") {
  theme_clean(base_size = paper_font_size) +
    theme(
      strip.text        = element_text(face = "bold", size = rel(1)),
      legend.position   = legend,
      legend.title      = element_text(size = rel(0.8)),
      legend.key.width  = unit(14, "pt"),
      legend.margin     = margin(0, 0, 0, 0),
      panel.spacing.x   = unit(12, "pt"),
      panel.spacing.y   = unit(8, "pt"),
      plot.tag          = element_text(face = "bold", size = paper_font_size + 2)
    )
}

# Opaque colour that looks like `hex` drawn at opacity `alpha` on white
tint <- function(hex, alpha) {
  rgb_mat <- grDevices::col2rgb(hex) / 255
  blended <- 1 - outer(as.vector(1 - rgb_mat), alpha)
  apply(blended, 2, function(x) grDevices::rgb(x[1], x[2], x[3]))
}

# Capital panel tags for multi-panel (patchwork) figures
panel_tags <- function() patchwork::plot_annotation(tag_levels = "A")

# Ticket price levels as shown on every axis: "+10%", "+30%", "+50%"
cost_level_label <- function(x) level_label(rep("cost_code", length(x)), x)
cost_levels      <- unlist(attribute_display$cost_code$levels, use.names = FALSE)


# cairo_pdf embeds Unicode glyphs (minus sign etc.); fall back to the base pdf
# device where cairo is unavailable (e.g. R without XQuartz on macOS).
cairo_ok <- function() {
  isTRUE(tryCatch(nzchar(grDevices::cairoVersion()),
                  warning = function(w) FALSE, error = function(e) FALSE))
}

# Save a figure as PNG. Main figures additionally get a vector PDF (for
# submission) when `save_pdf` is on in config.yaml; supplementary figures
# (anything under supp-figs/) are always PNG only. The Snakefile declares the
# same PDFs as outputs (MAIN_FIGURES), so the two stay in sync.
is_supplementary <- function(path) grepl("(^|/)supp-figs/", path)

save_paper_figure <- function(plot, path, width_mm = paper_width_mm,
                              height_mm, dpi = 600) {
  ggsave(path, plot, width = width_mm, height = height_mm, units = "mm",
         dpi = dpi, bg = "white")
  if (save_pdf && !is_supplementary(path)) {
    ggsave(sub("\\.png$", ".pdf", path), plot, width = width_mm,
           height = height_mm, units = "mm",
           device = if (cairo_ok()) cairo_pdf else "pdf")
  }
}

# ── Attribute-level marginal-means plot (shared by all conjoint figures) ─────

# Assign numeric y-positions so rows within a group are spaced `within` apart
# and there is an extra `between` gap AFTER the last item of each group
# (between the last item and the next group's header below it).
# Returns a named numeric vector keyed by level name.
make_row_positions <- function(lvls, hdrs, within = 1.0, between = 0.6) {
  pos <- numeric(length(lvls))
  current <- 0
  prev_was_header <- FALSE
  for (lv in rev(lvls)) {
    current <- current + within + if (prev_was_header) between else 0
    pos[match(lv, lvls)] <- current
    prev_was_header <- lv %in% hdrs
  }
  setNames(pos, lvls)
}

# Marginal means by attribute level: one row per level, attribute names as
# bold headers. `df` needs attribute, code, prob, asymp.LCL, asymp.UCL (raw
# codes; labelled here). Optional:
#   facet         column to facet by (one row of panels)
#   group         column drawn as offset, coloured dots within each row
#   group_values  named colours for the group levels (legend order = names)
#   color         ink for ungrouped plots (default: dark attribute ink)
plot_attribute_emm <- function(df, x_label, midline, symmetric = FALSE,
                               breaks = waiver(), facet = NULL, group = NULL,
                               group_values = NULL, group_name = NULL,
                               color = attribute_color) {
  row_pos    <- make_row_positions(plot_levels, attribute_headers)
  row_labels <- dplyr::if_else(plot_levels %in% attribute_headers,
                               paste0("<b>", plot_levels, "</b>"), plot_levels)

  df <- df |>
    label_attributes() |>
    dplyr::mutate(y_pos = row_pos[code])

  # Offset grouped dots within a row (first group level on top)
  if (!is.null(group)) {
    lv <- names(group_values)
    df[[group]] <- factor(df[[group]], levels = lv)
    offsets <- seq(0.25, -0.25, length.out = length(lv))
    if (length(lv) == 1) offsets <- 0
    df$y_pos <- df$y_pos + offsets[as.integer(df[[group]])]
  }

  x_limits <- if (symmetric) {
    symmetric_limits(c(df$asymp.LCL, df$asymp.UCL), midline)
  } else {
    range(c(df$asymp.LCL, df$asymp.UCL), na.rm = TRUE)
  }
  x_limits <- widen_limits_to_breaks(x_limits, breaks)

  p <- ggplot(df, aes(x = prob, y = y_pos)) +
    geom_vline(xintercept = midline, color = reference_line_color, linetype = "dashed",
               linewidth = 0.3) +
    scale_y_continuous(
      breaks = row_pos, labels = row_labels,
      limits = c(min(row_pos) - 0.8, max(row_pos) + 0.8),
      expand = expansion(add = 0)
    ) +
    scale_x_continuous(breaks = breaks) +
    coord_cartesian(xlim = x_limits) +
    theme_paper(legend = if (is.null(group)) "none" else "bottom") +
    theme(axis.text.y  = ggtext::element_markdown(),
          axis.ticks.y = element_blank()) +
    labs(x = x_label, y = NULL, color = group_name)

  if (is.null(group)) {
    p <- p +
      geom_linerange(aes(xmin = asymp.LCL, xmax = asymp.UCL),
                     color = color, linewidth = paper_ci_width,
                     na.rm = TRUE) +
      geom_point(color = color, size = paper_point_size, na.rm = TRUE)
  } else {
    p <- p +
      geom_linerange(aes(xmin = asymp.LCL, xmax = asymp.UCL,
                         color = .data[[group]]),
                     linewidth = paper_ci_width, na.rm = TRUE) +
      geom_point(aes(color = .data[[group]]), size = paper_point_size * 0.8,
                 na.rm = TRUE) +
      scale_color_manual(values = group_values, breaks = names(group_values))
  }

  if (!is.null(facet)) p <- p + facet_wrap(vars(.data[[facet]]), nrow = 1)
  p
}
