library(ggplot2)
library(reshape2)

md_1 <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

df_1 <- subset(
  md_1,
  select = c("Case_ID", "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3")
)

df_long <- melt(
  df_1,
  id.vars = "Case_ID",
  variable.name = "Timepoint",
  value.name = "Scores"
)

df_long$Timepoint <- factor(
  df_long$Timepoint,
  levels = c("MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3"),
  labels = c("T0", "T1", "T2", "T3")
)

median_scores <- aggregate(Scores ~ Timepoint, data = df_long, FUN = median, na.rm = TRUE)

p1 <- ggplot(median_scores, aes(x = Timepoint, y = Scores, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Median MoCA scores over time",
    x = "Timepoint",
    y = "Median MoCA score"
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

print(p1)

ggsave(
  filename = "~/Downloads/supplementary_figure_2_longitudinal_moca_scores.jpg",
  plot = p1,
  dpi = 600,
  width = 8,
  height = 10,
  units = "in"
)
