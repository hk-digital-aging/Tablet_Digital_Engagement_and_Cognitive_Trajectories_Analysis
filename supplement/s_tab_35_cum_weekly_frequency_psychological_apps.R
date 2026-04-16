library(readr)
library(dplyr)
library(tidyr)
library(tableone)

df <- read_csv(
  "~/Downloads/Suppl_data_5_Metadata_WeeklyUsage.csv",
  show_col_types = FALSE
)

df_wide <- spread(df, key = Categories, value = cumulative_freq_weekly)

df_wide <- df_wide[, c("Case_ID", "Weeks_From_Earliest", "Class", "Psychological_capacity")]

df_final <- df_wide %>%
  group_by(Case_ID, Weeks_From_Earliest, Class) %>%
  summarise(
    Psychological_capacity = max(Psychological_capacity, na.rm = TRUE),
    .groups = "drop"
  )

df_final$Psychological_capacity[df_final$Psychological_capacity == -Inf] <- NA

myVars <- c("Psychological_capacity")
catVars <- c("Class")
file_prefix <- "s_tab_35_cum_weekly_frequency_psychological_apps_week_"

dir.create(
  "~/Downloads/supplementary_table_35_output/",
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
  
  nonpara1 <- c("Psychological_capacity")
  
  week_table <- print(
    tab,
    nonnormal = nonpara1,
    showAllLevels = TRUE,
    formatOptions = list(big.mark = ",")
  )
  
  file_path <- paste0(
    "~/Downloads/supplementary_table_35_output/",
    file_prefix,
    week,
    ".csv"
  )
  
  write.csv(week_table, file = file_path, row.names = TRUE)
}

# Merge all week-specific tables
file_list <- paste0(
  "~/Downloads/supplementary_table_35_output/",
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
  file = "~/Downloads/supplementary_table_35_cum_weekly_frequency_psychological_apps.csv",
  row.names = FALSE
)
