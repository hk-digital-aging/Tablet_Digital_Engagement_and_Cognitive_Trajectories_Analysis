library(readr)
library(dplyr)
library(tidyr)
library(tableone)

df <- read_csv(
  "~/Downloads/Suppl_data_4_Metadata_MonthlyUsage.csv",
  show_col_types = FALSE
)

df_wide <- spread(df, key = Categories, value = cumulative_freq_monthly)

df_wide <- df_wide[, c("Case_ID", "Months_From_Earliest", "Class", "Cognitive_capacity")]

df_final <- df_wide %>%
  group_by(Case_ID, Months_From_Earliest, Class) %>%
  summarise(
    Cognitive_capacity = max(Cognitive_capacity, na.rm = TRUE),
    .groups = "drop"
  )

df_final$Cognitive_capacity[df_final$Cognitive_capacity == -Inf] <- NA

myVars <- c("Cognitive_capacity")
catVars <- c("Class")
file_prefix <- "s_tab_43_cum_monthly_frequency_cognitive_apps_month_"

dir.create(
  "~/Downloads/supplementary_table_43_output/",
  recursive = TRUE,
  showWarnings = FALSE
)

# Export month-specific tables
for (month in 0:8) {
  df_month <- df_final[df_final$Months_From_Earliest == month, ]
  
  tab <- CreateTableOne(
    vars = myVars,
    strata = "Class",
    data = df_month,
    factorVars = catVars
  )
  
  nonpara1 <- c("Cognitive_capacity")
  
  month_table <- print(
    tab,
    nonnormal = nonpara1,
    showAllLevels = TRUE,
    formatOptions = list(big.mark = ",")
  )
  
  file_path <- paste0(
    "~/Downloads/supplementary_table_43_output/",
    file_prefix,
    month,
    ".csv"
  )
  
  write.csv(month_table, file = file_path, row.names = TRUE)
}

# Merge all month-specific tables
file_list <- paste0(
  "~/Downloads/supplementary_table_43_output/",
  file_prefix,
  0:8,
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
  file = "~/Downloads/supplementary_table_43_cum_monthly_frequency_cognitive_apps.csv",
  row.names = FALSE
)
