library(ggplot2)
library(reshape2)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

df_1 <- subset(
  df,
  select = c("Case_ID", "Digital_T0", "Digital_T1", "Digital_T2", "Digital_T3")
)

# Reshape data to long format
df_long <- melt(
  df_1,
  id.vars = "Case_ID",
  variable.name = "Timepoint",
  value.name = "Scores"
)

df_long$Timepoint <- factor(
  df_long$Timepoint,
  levels = c("Digital_T0", "Digital_T1", "Digital_T2", "Digital_T3"),
  labels = c("T0", "T1", "T2", "T3")
)

# Calculate median scores for each timepoint
median_scores <- aggregate(Scores ~ Timepoint, data = df_long, FUN = median, na.rm = TRUE)

p2 <- ggplot(median_scores, aes(x = Timepoint, y = Scores, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Median digital health care literacy scores over time",
    x = "Timepoint",
    y = "Median digital health care literacy score"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.border = element_blank(),
    axis.line = element_blank()
  )

print(p2)

ggsave(
  filename = "~/Downloads/supplementary_figure_3_longitudinal_digital_health_care_literacy.jpg",
  plot = p2,
  dpi = 600,
  width = 8,
  height = 10,
  units = "in"
)
