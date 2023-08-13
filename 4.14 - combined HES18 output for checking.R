TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/save_standard_tar_nomoe_divide_into.R")
library(data.table)
library(magrittr)

etrs_list <- list(
  c(1, 2, 3, 4),
  c(51, 52, 53, 54),
  c(61, 62, 71, 72, 73, 74),
  c(75, 76, 77, 78),
  c(91, 92, 93, 94),
  c(95, 96, 97, 98)
)

OUTPUT_PATH <- "Results/TAR323_2023-04-18_EATRs-1-7_and_9.xlsx"

all_raw_output <- data.table()

for (etrs in etrs_list) {
  loginfo("Loading output for checking for %s", etrs)
  
  family_etrs_raw_output <- fread(sprintf(
    "data/HES18_analysis_raw_output_%s.csv.gz", paste(etrs, collapse = "-")
  ))
  family_etr_breakdowns_raw_output <- fread(sprintf(
    "data/HES18_eatr_breakdowns_raw_%s.csv.gz", paste(etrs, collapse = "-")
  ))
  gini_raw_output <- fread(sprintf(
    "data/HES18_analysis_gini_raw_output_%s.csv.gz", paste(etrs, collapse = "-")
  ))
  
  raw_output <- rbindlist(list(
    family_etrs_raw_output,
    family_etr_breakdowns_raw_output,
    gini_raw_output
  ), fill = TRUE)
  
  all_raw_output <- rbind(all_raw_output, raw_output, fill = TRUE)
}
  
  
all_raw_output[, ":="(
  Index = 1:.N,
  Scenario = "SQ"
)]
# Default to Values
all_raw_output[, Sheet := "Values"]
# Override for medians
all_raw_output[Average_Type == "Median", Sheet := "Quantiles"]

# Suppressed & rounded
suppressed_output <- TAWApost::suppress(all_raw_output, "HES18")
suppressed_and_rounded_output <- TAWApost::tawa_round(
  suppressed_output,
  rounding_rules = list("EATR_Percentage" = 0.1, "Gini" = 0.1)
)

loginfo("Saving output for checking")
save_standard_tar_nomoe_divide_into(
  output_dt = suppressed_and_rounded_output,
  output_path = OUTPUT_PATH,
  template_path = TEMPLATE_PATH,
  divide_into = 4
)

# # Save to Stats checking format
# value_cols <- c("Index", "Value", "Population", "Sample")
# quantiles_cols <- c("Index", "Quantile", "Value", "Population", "Sample")
# non_descriptor_cols <- c("Value", "Margin_Of_Error", "Population", "Sample", "Quantile")
# 
# values_dt <- suppressed_and_rounded_output[Sheet == "Values", .SD, .SDcols = value_cols]
# quantiles_dt <- suppressed_and_rounded_output[Sheet == "Quantiles", .SD, .SDcols = quantiles_cols]
# descriptors_dt <- suppressed_and_rounded_output[, .SD, .SDcol = !non_descriptor_cols]
# setcolorder(descriptors_dt, c("Index", "Sheet", "Scenario", "Variable"))
# 
# TAWApost::save_stats_output(
#   values_dt = values_dt,
#   quantiles_dt = quantiles_dt,
#   descriptors_dt = descriptors_dt,
#   output_path = OUTPUT_PATH,
#   template_path = TEMPLATE_PATH
# )
