library(dplyr)
library(ggplot2)
library(patchwork)

# Read data
df <- read.csv("/Users/wangmoyao/Library/CloudStorage/OneDrive-TheUniversityOfHongKong/Tablet/Final_How_to_be_a_J_person/Yap_Sai_hui/Supplementary_figures/05_changes_in_patterns_of_TDE_PAE_by_week/input/Suppl_data_5_Metadata_WeeklyUsage.csv")
colnames(df)

# ----- 1. Absolute Weekly Duration (TDE) -----
df_T3_ALL <- subset(df, Categories == "ALL")
df_T3_ALL$Weeks_From_Earliest <- as.factor(df_T3_ALL$Weeks_From_Earliest)
df_T3_ALL$Class <- as.factor(df_T3_ALL$Class)

df_T3_ALL_summary <- df_T3_ALL %>%
  group_by(Weeks_From_Earliest, Class) %>%
  summarise(med = median(Absolute_duration_hours_weekly),
            Q1 = quantile(Absolute_duration_hours_weekly, 0.25),
            Q3 = quantile(Absolute_duration_hours_weekly, 0.75),
            .groups = 'drop')

p8 <- ggplot(df_T3_ALL, aes(x = Weeks_From_Earliest, y = Absolute_duration_hours_weekly, color = Class)) +
  geom_pointrange(data = df_T3_ALL_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_ALL_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 22, show.legend = FALSE) +
  labs(x = "Weeks", y = 'Hours', title = 'Absolute Weekly Duration (TDE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_ALL_summary$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 
ggsave(filename = "output/abs_duration_all.png", plot = p8, dpi = 600, width = 12, height = 8, units = "in")

# ----- 2. Absolute Weekly Duration (PAE) -----
df_T3_Psychological_capacity <- subset(df, Categories == "Psychological_capacity")
df_T3_Psychological_capacity$Weeks_From_Earliest <- as.factor(df_T3_Psychological_capacity$Weeks_From_Earliest)
df_T3_Psychological_capacity$Class <- as.factor(df_T3_Psychological_capacity$Class)

df_T3_Psychological_capacity_summary <- df_T3_Psychological_capacity %>% 
  group_by(Weeks_From_Earliest, Class) %>% 
  summarise(med = median(Absolute_duration_hours_weekly),
            Q1 = quantile(Absolute_duration_hours_weekly, 0.25),
            Q3 = quantile(Absolute_duration_hours_weekly, 0.75))

p10 <- ggplot(df_T3_Psychological_capacity, aes(x = Weeks_From_Earliest, y = Absolute_duration_hours_weekly, color = Class)) +
  geom_pointrange(data = df_T3_Psychological_capacity_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_Psychological_capacity_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 9, show.legend = FALSE) +
  labs(x = "Weeks", y = 'Hours', title = 'Absolute Weekly Duration (PAE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_Psychological_capacity$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 

ggsave(filename = "output/abs_duration_psy.png", plot = p10, dpi = 600, width = 12, height = 8, units = "in")

# ----- 3. Cumulative Weekly Duration (TDE) -----
df_T3_ALL_cum_summary <- df_T3_ALL %>%
  group_by(Weeks_From_Earliest, Class) %>%
  summarise(med = median(Cumulative_duration_hours_weekly),
            Q1 = quantile(Cumulative_duration_hours_weekly, 0.25),
            Q3 = quantile(Cumulative_duration_hours_weekly, 0.75))

p7 <- ggplot(df_T3_ALL, aes(x = Weeks_From_Earliest, y = Cumulative_duration_hours_weekly, color = Class)) +
  geom_pointrange(data = df_T3_ALL_cum_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_ALL_cum_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 300, show.legend = FALSE) +
  labs(x = "Weeks", y = 'Hours', title = 'Cumulative Weekly Duration (TDE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_ALL_cum_summary$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 
ggsave(filename = "output/cum_duration_all.png", plot = p7, dpi = 600, width = 12, height = 8, units = "in")

# ----- 4. Cumulative Weekly Duration (PAE) -----
df_T3_Psychological_capacity_cum_summary <- df_T3_Psychological_capacity %>%
  group_by(Weeks_From_Earliest, Class) %>%
  summarise(med = median(Cumulative_duration_hours_weekly),
            Q1 = quantile(Cumulative_duration_hours_weekly, 0.25),
            Q3 = quantile(Cumulative_duration_hours_weekly, 0.75))

p9 <- ggplot(df_T3_Psychological_capacity, aes(x = Weeks_From_Earliest, y = Cumulative_duration_hours_weekly, color = Class)) +
  geom_pointrange(data = df_T3_Psychological_capacity_cum_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_Psychological_capacity_cum_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 190, show.legend = FALSE) +
  labs(x = "Weeks", y = 'Hours', title = 'Cumulative Weekly Duration (PAE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_Psychological_capacity_cum_summary$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 
ggsave(filename = "output/cum_duration_psy.png", plot = p9, dpi = 600, width = 12, height = 8, units = "in")

# ----- 5. Absolute Weekly Frequency (TDE) -----
df_T3_ALL_freq_summary <- df_T3_ALL %>%
  group_by(Weeks_From_Earliest, Class) %>%
  summarise(med = median(absolute_freq_weekly),
            Q1 = quantile(absolute_freq_weekly, 0.25),
            Q3 = quantile(absolute_freq_weekly, 0.75))

p11 <- ggplot(df_T3_ALL, aes(x = Weeks_From_Earliest, y = absolute_freq_weekly, color = Class)) +
  geom_pointrange(data = df_T3_ALL_freq_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_ALL_freq_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 1.05, show.legend = FALSE) +  
  labs(x = "Weeks", y = 'Frequency', title = 'Absolute Weekly Frequency (TDE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_ALL_freq_summary$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 
ggsave(filename = "output/abs_frequency_all.png", plot = p11, dpi = 600, width = 12, height = 8, units = "in")

# ----- 6. Absolute Weekly Frequency (PAE) -----
df_T3_Psychological_capacity_freq_summary <- df_T3_Psychological_capacity %>%
  group_by(Weeks_From_Earliest, Class) %>%
  summarise(med = median(absolute_freq_weekly),
            Q1 = quantile(absolute_freq_weekly, 0.25),
            Q3 = quantile(absolute_freq_weekly, 0.75))

p12 <- ggplot(df_T3_Psychological_capacity, aes(x = Weeks_From_Earliest, y = absolute_freq_weekly, color = Class)) +
  geom_pointrange(data = df_T3_Psychological_capacity_freq_summary, aes(y = med, ymin = Q1, ymax = Q3), position = position_dodge(width = 0.5)) +
  geom_line(data = df_T3_Psychological_capacity_freq_summary, aes(y = med, group = Class)) +
  stat_compare_means(aes(group = Class), method = "kruskal.test", label = "p.signif", label.y = 0.8, show.legend = FALSE) +  
  labs(x = "Weeks", y = 'Frequency', title = 'Absolute Weekly Frequency (PAE)') +
  theme_light() +
  scale_color_manual(values = c("#CA601B", "#1B85B2", "#DD0187", "#037203")) +
  scale_x_discrete(labels = paste("Week", seq_along(levels(df_T3_Psychological_capacity_freq_summary$Weeks_From_Earliest)))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), axis.title.x = element_text(margin = margin(t = 20))) 
ggsave(filename = "output/abs_frequency_psy.png", plot = p12, dpi = 600, width = 12, height = 8, units = "in")

# ----- Combine All Plots -----
p_combined <- p8 + p10 + p7 + p9 + p11 + p12 +
  plot_layout(ncol = 2, nrow = 4, guides = "collect") & theme(legend.position = 'bottom')

# Save combined plot
ggsave(filename = "output/combined_weekly_tde_pae_plots.png", plot = p_combined, dpi = 1000, width = 20, height = 25, units = "in")
