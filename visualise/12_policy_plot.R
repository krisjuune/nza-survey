library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(here)

if (exists("snakemake")) {
  policy_choice_emm <- snakemake@input[["choice"]]
  choice_out        <- snakemake@output[["choice_plot"]]
} else {
  policy_choice_emm <- here("data", "policy_choice_emm.csv")
  choice_out        <- here("output", "policy_choice_plot.png")
}

policy_labels <- c(
  "GBF"            = "GBF",
  "CORSIA"         = "CORSIA",
  "SAF_ETS"        = "SAF + ETS Mandate",
  "VCM_SBTi"       = "VCM SBTi",
  "VCM_status_quo" = "VCM Status Quo"
)

df <- read_csv(policy_choice_emm, show_col_types = FALSE) |>
  mutate(
    policy_label = factor(
      policy_labels[policy_type],
      levels = rev(policy_labels)
    )
  )

choice_plot <- ggplot(
  df,
  aes(x = policy_label, y = prob, color = policy_label)
) +
  geom_hline(yintercept = 0.5, color = "grey60", linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2
  ) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, end = .95, option = "D") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = "Choice probability (marginal mean)",
    title = "Choice probabilities by policy type"
  )

ggsave(choice_out, choice_plot, width = 7, height = 4)
message("Policy choice plot saved: ", choice_out)
