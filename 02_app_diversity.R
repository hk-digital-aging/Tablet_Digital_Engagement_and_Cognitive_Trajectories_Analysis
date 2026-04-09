library(svglite)
library(dplyr)
library(ggplot2)
library(patchwork)

df <- read.csv("~/Downloads/Metadata_4_apps_new_V2.csv", stringsAsFactors = FALSE)

beautiful_colors <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#0ec434", "#228c68",
  "#8ad8e8", "#235b54", "#29bdab", "#3998f5", "#37294f", "#277da7",
  "#3750db", "#f22020", "#991919", "#ffcba5", "#e68f66", "#c56133",
  "#96341c", "#632819", "#ffc413", "#f47a22", "#2f2aa0", "#b732cc",
  "#772b9d", "#f07cab", "#d30b94", "#edeff3", "#c3a5b4", "#946aa2"
)

make_class_plot <- function(data, count_col, denom, class_title) {
  
  df_plot <- data %>%
    group_by(Categories) %>%
    arrange(desc(ALL_Count_App), .by_group = TRUE) %>%
    ungroup()
  
  df_plot$Categories <- factor(df_plot$Categories)
  df_plot <- df_plot %>% arrange(Categories)
  
  df_plot$id <- seq_len(nrow(df_plot))
  df_plot$perc <- df_plot[[count_col]] / denom * 100
  
  label_data <- df_plot
  number_of_bar <- nrow(label_data)
  
  label_data <- label_data %>%
    mutate(angle = 90 - 360 * (id - 0.5) / number_of_bar)
  
  label_data$hjust <- ifelse(label_data$angle < -90, 1, 0)
  label_data$angle <- ifelse(label_data$angle < -90, label_data$angle + 180, label_data$angle)
  
  ggplot(label_data, aes(x = factor(id), y = perc, fill = Categories)) +
    geom_vline(
      xintercept = seq(1, nrow(label_data), by = 1),
      colour = "white",
      linewidth = 0.25
    ) +
    geom_rect(
      aes(
        fill = Googleplay,
        xmin = id - 0.5,
        xmax = id + 0.5,
        ymin = 100,
        ymax = 110
      ),
      color = "black",
      linewidth = 0.01
    ) +
    geom_bar(
      stat = "identity",
      alpha = 0.8,
      width = 0.5
    ) +
    geom_hline(yintercept = 25, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
    geom_hline(yintercept = 75, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
    geom_text(
      aes(
        x = id,
        y = 122,
        label = APP.name,
        angle = angle,
        hjust = hjust
      ),
      size = 1.8,
      fontface = "plain",
      family = "sans"
    ) +
    coord_polar(start = 0) +
    scale_fill_manual(values = beautiful_colors) +
    ylim(-100, 215) +
    labs(title = class_title) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "grey92", colour = NA),
      plot.background = element_rect(fill = "grey92", colour = NA),
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        face = "bold",
        family = "sans"
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
}

p1 <- make_class_plot(df, "Class_1_Count_App", 12,  "Class 1")
p2 <- make_class_plot(df, "Class_2_Count_App", 717, "Class 2")
p3 <- make_class_plot(df, "Class_3_Count_App", 212, "Class 3")
p4 <- make_class_plot(df, "Class_4_Count_App", 244, "Class 4")

p_all <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "1B",
    theme = theme(
      plot.title = element_text(
        hjust = 0,
        size = 22,
        face = "bold",
        family = "sans"
      )
    )
  )

print(p_all)

svglite("~/Downloads/figure_1b_app_diversity_combined.svg", width = 16, height = 15)
print(p_all)
dev.off()
