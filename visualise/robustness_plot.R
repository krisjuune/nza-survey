library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(here)
library(patchwork)

source(here("visualise", "theme_clean.R"))

if (exists("snakemake")) {
  variants_file   <- snakemake@input[["variants"]]
  validation_file <- snakemake@input[["validation"]]
  sample_out      <- snakemake@output[["sample_plot"]]
  income_out      <- snakemake@output[["income_plot"]]
  stated_out      <- snakemake@output[["stated_plot"]]
  ranking_out     <- snakemake@output[["ranking_plot"]]
  country_choice_file <- snakemake@input[["country_choice"]]
  country_rating_file <- snakemake@input[["country_rating"]]
  overall_choice_file <- snakemake@input[["overall_choice"]]
  overall_rating_file <- snakemake@input[["overall_rating"]]
} else {
  variants_file   <- here("data", "robustness_country_choice.csv")
  validation_file <- here("data", "robustness_validation.csv")
  sample_out      <- here("output", "supp_figs", "robustness_sample.png")
  income_out      <- here("output", "supp_figs", "robustness_income.png")
  stated_out      <- here("output", "supp_figs", "validation_stated.png")
  ranking_out     <- here("output", "supp_figs", "validation_ranking.png")
  country_choice_file <- here("data", "country_choice_emm.csv")
  country_rating_file <- here("data", "country_rating_emm.csv")
  overall_choice_file <- here("data", "overall_choice_emm.csv")
  overall_rating_file <- here("data", "overall_rating_emm.csv")
}

variants <- read_csv(variants_file, show_col_types = FALSE,
                     col_types = cols(code = col_character())) |>
  mutate(country = factor(country, levels = country_levels))

# Country choice results re-estimated on alternative samples / weights, with
# the reference estimate in the main-figure indigo
plot_variants <- function(labels, values) {
  d <- variants |>
    filter(variant %in% names(labels)) |>
    mutate(variant = labels[variant])
  plot_attribute_emm(d, x_label = "Choice probability", midline = 0.5,
                     breaks = choice_breaks, facet = "country",
                     group = "variant",
                     group_values = setNames(values, unname(labels)),
                     group_name = NULL)
}

# ── Sample exclusions (same glmer model as the main figure) ──────────────────

sample_plot <- plot_variants(
  c(full = "Full sample", no_speeders = "Excluding fastest 5%",
    attention_passed = "Attention-check passers"),
  tint(highlight_color, c(1, robustness_alphas[["no_speeders"]],
                          robustness_alphas[["attention_passed"]]))
)
save_paper_figure(sample_plot, sample_out, height_mm = 135)

# ── Income weighting (design-based marginal means) ───────────────────────────

income_plot <- plot_variants(
  c(design_unweighted = "Unweighted", design_weighted = "Weighted to income deciles"),
  tint(highlight_color, c(1, robustness_alphas[["design_weighted"]]))
)
save_paper_figure(income_plot, income_out, height_mm = 125)

# ── Stated pathway ratings vs conjoint choices ───────────────────────────────
# Facet titles: stated pathway item vs the conjoint level it is matched to

items <- c(
  pref_path_safs        = "SAFs\nvs biofuels",
  pref_path_synfuels    = "Synthetic fuels\nvs synthetic fuels",
  pref_path_geol_offset = "Geological offsets\nvs permanent",
  pref_path_trad_offset = "Traditional offsets\nvs traditional offsets"
)
row_labels <- c("Pooled", country_levels)
row_pos    <- setNames(c(length(country_levels) + 1.6, rev(seq_along(country_levels))),
                       row_labels)

stated <- read_csv(validation_file, show_col_types = FALSE) |>
  filter(check == "stated_vs_choice") |>
  mutate(item = factor(items[item], levels = items), y = row_pos[country])

stated_plot <- ggplot(stated, aes(x = estimate, y = y)) +
  geom_vline(xintercept = 0, color = reference_line_color, linetype = "dashed",
             linewidth = 0.3) +
  geom_linerange(aes(xmin = lower, xmax = upper), color = highlight_color,
                 linewidth = paper_ci_width) +
  geom_point(color = highlight_color, size = paper_point_size) +
  scale_y_continuous(breaks = row_pos,
                     labels = if_else(row_labels == "Pooled", "<b>All countries</b>",
                                      row_labels),
                     expand = expansion(add = 0.6)) +
  facet_wrap(~item, nrow = 1) +
  theme_paper(legend = "none") +
  theme(axis.text.y = ggtext::element_markdown(), axis.ticks.y = element_blank()) +
  labs(x = "Difference in choice probability of the matching level (percentage points),\nfavourable (4–5) minus other ratings (1–3)",
       y = NULL)
save_paper_figure(stated_plot, stated_out, height_mm = 75)

# ── Internal validation: conjoint ratings vs choices ─────────────────────────

panel_levels <- c("Pooled", country_levels)
panel_labels <- setNames(c("Pooled", country_levels), panel_levels)
validation   <- read_csv(validation_file, show_col_types = FALSE)

# B: rank of each policy package (1 = most preferred) by choice probability and
# by mean support rating; coinciding dots = same rank under both measures
ranks <- validation |>
  filter(check %in% c("package_choice", "package_rating")) |>
  group_by(check, country) |>
  mutate(rank = rank(-estimate, ties.method = "min")) |>
  ungroup() |>
  arrange(desc(check)) |>
  mutate(
    measure = factor(if_else(check == "package_choice", "Rank by choice",
                             "Rank by rating"),
                     levels = c("Rank by choice", "Rank by rating")),
    country = factor(panel_labels[country], levels = panel_labels),
    package = factor(policy_label(item, wrap = TRUE),
                     levels = rev(policy_label(policy_display_order, wrap = TRUE)))
  )

p_rank <- ggplot(ranks, aes(x = rank, y = package)) +
  geom_line(aes(group = package), color = policy_color, linewidth = paper_ci_width) +
  # rating = open ring drawn larger, choice = filled dot on top: where the two
  # ranks agree the dot sits inside the ring
  geom_point(aes(shape = measure, size = measure), color = policy_color,
             fill = NA, stroke = 0.6) +
  scale_shape_manual(values = c("Rank by choice" = 16, "Rank by rating" = 21),
                     name = NULL) +
  scale_size_manual(values = c("Rank by choice" = paper_point_size,
                               "Rank by rating" = paper_point_size * 1.8),
                    name = NULL) +
  scale_x_continuous(breaks = 1:5, limits = c(0.7, 5.3)) +
  facet_wrap(~country, nrow = 1) +
  theme_paper() +
  theme(axis.ticks.y = element_blank()) +
  labs(x = "Rank (1 = most preferred)", y = NULL)

# A: marginal means of every attribute level, support rating vs choice
level_mm <- bind_rows(
  inner_join(read_csv(overall_choice_file, show_col_types = FALSE),
             read_csv(overall_rating_file, show_col_types = FALSE),
             by = c("attribute", "code")) |> mutate(country = "Pooled"),
  inner_join(read_csv(country_choice_file, show_col_types = FALSE),
             read_csv(country_rating_file, show_col_types = FALSE),
             by = c("country", "attribute", "code"))
) |>
  mutate(country = factor(panel_labels[country], levels = panel_labels))

level_r <- level_mm |>
  group_by(country) |>
  summarise(r = cor(prob, emmean), .groups = "drop") |>
  mutate(label = sprintf("r = %.2f", r))

# The y-axis label is drawn as horizontal text just left of the first panel's
# axis (in the space the package names of the other panel reserve anyway),
# instead of a vertical axis title pushed away by the panel alignment.
x_range  <- c(0.38, 0.64)
y_label  <- tibble(country = factor(panel_labels[["Pooled"]], levels = panel_labels),
                   x = x_range[1] - 0.11, y = mean(range(level_mm$emmean)),
                   label = "Estimated rating\nof attribute level")

p_levels <- ggplot(level_mm, aes(x = prob, y = emmean)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = reference_line_color,
              linewidth = 0.3, linetype = "dashed") +
  geom_point(color = highlight_color, size = paper_point_size * 0.8) +
  geom_text(data = level_r, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.4, size = paper_text_size, color = attribute_color) +
  geom_text(data = y_label, aes(x = x, y = y, label = label), hjust = 1,
            size = paper_font_size / .pt, lineheight = 0.95, color = "black") +
  scale_x_continuous(breaks = c(0.4, 0.5, 0.6)) +
  coord_cartesian(xlim = x_range, expand = FALSE, clip = "off") +
  facet_wrap(~country, nrow = 1) +
  theme_paper(legend = "none") +
  labs(x = "Choice probability of attribute level", y = NULL)

ranking_plot <- (p_levels / p_rank) +
  plot_layout(heights = c(0.8, 1)) +
  panel_tags()
save_paper_figure(ranking_plot, ranking_out, height_mm = 130)

message("Robustness plots saved: ", sample_out, ", ", income_out, ", ", stated_out,
        ", ", ranking_out)
