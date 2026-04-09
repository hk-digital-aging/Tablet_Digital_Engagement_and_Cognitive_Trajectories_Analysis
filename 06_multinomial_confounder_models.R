library(nnet)
library(questionr)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

# Function to add significance symbols
add_p_signif <- function(tab) {
  tab$p_signif <- ifelse(
    tab$p < 0.001, "***",
    ifelse(
      tab$p < 0.01, "**",
      ifelse(
        tab$p < 0.05, "*",
        ifelse(tab$p < 0.1, ".", " ")
      )
    )
  )
  tab
}

# Function to run a univariate multinomial model and save results
run_and_save_multinom <- function(data, ref_class, predictor, output_name) {
  data$Class <- as.factor(data$Class)
  data$Class <- relevel(data$Class, ref = ref_class)
  
  formula_obj <- as.formula(paste("Class ~", predictor))
  
  invisible(
    capture.output(
      model <- multinom(formula_obj, data = data, model = TRUE)
    )
  )
  
  result_tab <- odds.ratio(model)
  result_tab <- add_p_signif(result_tab)
  
  write.csv(
    result_tab,
    file = paste0(
      "~/Downloads/table_3_output/01_confounder/",
      "Class", ref_class, "_Model0_",
      output_name,
      ".csv"
    ),
    row.names = FALSE
  )
}

# Models with Class 2 as the reference
run_and_save_multinom(df, ref_class = "2", predictor = "Age",       output_name = "regm_age")
run_and_save_multinom(df, ref_class = "2", predictor = "Education", output_name = "regm_education")
run_and_save_multinom(df, ref_class = "2", predictor = "Gender",    output_name = "regm_gender")
run_and_save_multinom(df, ref_class = "2", predictor = "Literacy",  output_name = "regm_digital_literacy")

# Models with Class 4 as the reference
run_and_save_multinom(df, ref_class = "4", predictor = "Age",       output_name = "regm_age")
run_and_save_multinom(df, ref_class = "4", predictor = "Education", output_name = "regm_education")
run_and_save_multinom(df, ref_class = "4", predictor = "Gender",    output_name = "regm_gender")
run_and_save_multinom(df, ref_class = "4", predictor = "Literacy",  output_name = "regm_digital_literacy")
