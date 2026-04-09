library(dplyr)
library(reshape2)
library(PMCMRplus)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

df_1 <- subset(
  df,
  select = c("Case_ID", "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3")
)

# Reshape data to long format
df_long <- melt(
  df_1,
  id.vars = "Case_ID",
  variable.name = "Timepoint",
  value.name = "Scores"
)

# Remove missing observations
df_long_complete <- na.omit(df_long)

# Keep only complete cases with all timepoints available
df_long_complete <- df_long_complete %>%
  group_by(Case_ID) %>%
  filter(n() == length(unique(df_long$Timepoint))) %>%
  ungroup()

# Friedman test
friedman_result <- friedman.test(Scores ~ Timepoint | Case_ID, data = df_long_complete)

friedman_df <- data.frame(
  method = friedman_result$method,
  data.name = friedman_result$data.name,
  statistic = as.numeric(friedman_result$statistic),
  parameter = as.numeric(friedman_result$parameter),
  p.value = friedman_result$p.value
)

write.csv(
  friedman_df,
  file = "~/Downloads/supplementary_table_1_friedman_result.csv",
  row.names = FALSE
)

# Nemenyi post-hoc test
nemenyi_test <- frdAllPairsNemenyiTest(Scores ~ Timepoint | Case_ID, data = df_long_complete)

nemenyi_pvalues <- as.data.frame(nemenyi_test$p.value)
nemenyi_statistics <- as.data.frame(nemenyi_test$statistic)

write.csv(
  nemenyi_pvalues,
  file = "~/Downloads/supplementary_table_1_nemenyi_pvalues.csv",
  row.names = TRUE
)

write.csv(
  nemenyi_statistics,
  file = "~/Downloads/supplementary_table_1_nemenyi_statistics.csv",
  row.names = TRUE
)
