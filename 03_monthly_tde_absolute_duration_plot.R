library(dplyr)
library(ggplot2)
library(ggpubr)

df <- read.csv("~/Downloads/Suppl_data_4_Metadata_MonthlyUsage.csv", stringsAsFactors = FALSE)

df_T3_ALL <- subset(df, Categories == "ALL")
df_T3_ALL$Months_From_Earliest <- as.factor(df_T3_ALL$Months_From_Earliest)
df_T3_ALL$Class <- as.factor(df_T3_ALL$Class)

# Summarise median and IQR by month and class
df_T3_ALL_summary <- df_T3_ALL |>
  group_by(Months_From_Earliest, Class) |>
  summarise(
    med = median(Absolute_duration_hours_monthly, na.rm = TRUE),
    Q1 = quantile(Absolute_duration_hours_monthly, 0.25, na.rm = TRUE),
    Q3 = quantile(Absolute_duration_hours_monthly, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

pa <- ggplot(
  df_T3_ALL,
  aes(x = Months_From_Earliest, y = Absolute_duration_hours_monthly, color = Class)
) +
  geom_pointrange(
    data = df_T3_ALL_summary,
    aes(y = med, ymin = Q1, ymax = Q3),
    position = position_dodge(width = 0.5),
    size = 0.1,
    show.legend = TRUE
  ) +
  geom_line(
    data = df_T3_ALL_summary,
    aes(y = med, group = Class)
  ) +
  stat_compare_means(
    aes(group = Class),
    method = "kruskal.test",
    label = "p.signif",
    label.y = 55,
    show.legend = FALSE
  ) +
  labs(
    x = "Months",
    y = "Hours",
    title = "A. Absolute monthly duration (TDE)",
    color = "MoCA trajectory classes"
  ) +
  theme_light() +
  scale_color_manual(
    values = c("#CA601B", "#1B85B2", "#DD0187", "#037203"),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4")
  ) +
  scale_x_discrete(
    labels = paste("Month", seq_along(levels(df_T3_ALL$Months_From_Earliest)))
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 20))
  )

print(pa)

ggsave(
  filename = "~/Downloads/figure_2a_monthly_tde_absolute_duration.jpg",
  plot = pa,
  dpi = 600,
  width = 12,
  height = 8,
  units = "in"
)
