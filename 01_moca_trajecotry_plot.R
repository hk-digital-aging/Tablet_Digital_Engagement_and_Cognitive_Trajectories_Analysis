library(ggplot2)

# Read input data
df <- read.csv(
  "~/Downloads/suppl_data_3_metadata_9m_cumulative_usage.csv",
  stringsAsFactors = FALSE
)

# Ensure class is treated as a categorical variable
df$Class <- factor(df$Class)

# Create plot
p1 <- ggplot(df, aes(x = Time, y = Scores, group = Class, colour = Class)) +
  geom_line(alpha = 0) +
  geom_smooth(
    aes(group = Class, color = Class, fill = Class),
    method = "lm",
    formula = y ~ poly(x, degree = 2),
    alpha = 0.6
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3),
    labels = c("T0", "T1", "T2", "T3")
  ) +
  labs(
    x = "Timepoint",
    y = "MoCA score",
    colour = "Trajectory class",
    title = "MoCA score changes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) +
  scale_color_manual(
    values = c("#CA601B", "#1B85B2", "#DD0187", "#037203"),
    labels = c(
      "Class 1 (n = 12)",
      "Class 2 (n = 717)",
      "Class 3 (n = 212)",
      "Class 4 (n = 244)"
    )
  ) +
  scale_fill_manual(
    values = c("#FADDA1", "#AFE9F8", "#FEA3BE", "#EFF9E1")
  ) +
  guides(fill = "none")

# Print plot
print(p1)

# Save output
ggsave(
  filename = "~/Downloads/figure_1a_moca_trajectory.jpg",
  plot = p1,
  dpi = 600,
  width = 10,
  height = 10,
  units = "in"
)
