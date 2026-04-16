library(readr)
library(dplyr)
library(tidyr)
library(tableone)

df <- read_csv(
  "~/Downloads/Suppl_data_5_Metadata_WeeklyUsage.csv",
  show_col_types = FALSE
)

df_wide <- spread(df, key = Categories, value = Cumulative_duration_hours_weekly)

df_wide <- df_wide[, c("Case_ID", "Weeks_From_Earliest", "Class", "ALL")]

df_final <- df_wide %>%
  group_by(Case_ID, Weeks_From_Earliest, Class) %>%
  summarise(
    ALL = max(ALL, na.rm = TRUE),
    .groups = "drop"
  )

df_final$ALL[df_final$ALL == -Inf] <- NA

df_final <- df_final %>%
  filter(Class %in% c("3", "4"))

myVars <- c("ALL")
catVars <- c("Class")
file_prefix <- "s_tab_12_cum_weekly_duration_all_apps_class3_vs_class4_week_"

dir.create(
  "~/Downloads/supplementary_table_12_output/",
  recursive = TRUE,
  showWarnings = FALSE
)

# Export week-specific tables
for (week in 0:37) {
  df_week <- df_final[df_final$Weeks_From_Earliest == week, ]
  
  tab <- CreateTableOne(
    vars = myVars,
    strata = "Class",
    data = df_week,
    factorVars = catVars
  )
  
  nonpara1 <- c("ALL")
  
  week_table <- print(
    tab,
    nonnormal = nonpara1,
    showAllLevels = TRUE,
    formatOptions = list(big.mark = ",")
  )
  
  file_path <- paste0(
    "~/Downloads/supplementary_table_12_output/",
    file_prefix,
    week,
    ".csv"
  )
  
  write.csv(week_table, file = file_path, row.names = TRUE)
}

# Merge all week-specific tables
file_list <- paste0(
  "~/Downloads/supplementary_table_12_output/",
  file_prefix,
  0:37,
  ".csv"
)

data_list <- lapply(file_list, function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  data <- data %>% mutate(across(everything(), as.character))
  data$file_name <- basename(file)
  data
})

merged_data <- bind_rows(data_list)

write.csv(
  merged_data,
  file = "~/Downloads/supplementary_table_12_cum_weekly_duration_all_apps_class3_vs_class4.csv",
  row.names = FALSE
)
