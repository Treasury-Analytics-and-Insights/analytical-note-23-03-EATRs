TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWArun_setup.R")
# Setting this to TRUE runs TAWA in test mode.
# When you have set everything up,
# change this to FALSE to run TAWA for real.

DRY_RUN <- FALSE

# Main options of run_scenarios
SRC_DIR <- "tawaproc/R"

# Scenario parameters: {filename = scenario_names}
SCENARIO_LIST <- list(
  "Parameters/TY18_BEFU22.xlsx" = c("SQ")
)

TERMINAL_VALUES <- c(
  TAWArun::DEFAULT_TERMINAL_VALUES,
  "P_Benefits_All_NonTaxable",
  "P_Income_StudentAllowance",
  "snz_hes_uid",
  "snz_hes_hhld_uid",
  "H_Tenure_Owned",
  "H_Tenure_Rented"
  # "P_Income_Raw_StudentAllowance",
  # 
  # "P_Income_Raw_ACCDependent",
  # "P_Income_Calculated_ACCReceived",
  # 
  # "P_Attributes_PersonNumberInHES"
)

# Run TAWA, in aggregated period mode
TAWArun::run_TAWA(
  scenario_list = SCENARIO_LIST,
  db_path = DB_PATH,
  src_dir = SRC_DIR,
  inflators_file = INFLATORS_PATH,
  inflators_mapping_file = INFLATORS_MAPPING_PATH,
  survey = SURVEY,
  terminal_values = TERMINAL_VALUES,
  aggregate_period_data = TRUE,
  output_dir = "TAWA Output Full AS",
  dry_run = DRY_RUN
)
