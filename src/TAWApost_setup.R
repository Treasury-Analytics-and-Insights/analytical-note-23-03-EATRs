# Helper script to setup a standard TAWApost runscript
library(magrittr)
library(data.table)
library(logging)

# Setup a new logging file
logReset()
DATE_STAMP <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
setLevel("DEBUG") # can be any of `loglevels`, e.g. "INFO", "WARN", "ERROR"
addHandler(writeToFile, file = sprintf("logs/TAWApost_%s.log", DATE_STAMP))
addHandler(writeToConsole)

TAR_settings <- yaml::read_yaml(TAR_settings_path)
TAR_NUMBER <- as.numeric(TAR_settings[["TAR"]])

NUM_REPLICATES <- TAR_settings[["NUM_REPLICATES"]]

TEMPLATE_PATH <- TAR_settings[["TEMPLATE_PATH"]]

WEIGHTS_DIR <- TAR_settings[["WEIGHTS_DIR"]]
EXP_WEIGHTS_DIR <- TAR_settings[["EXP_WEIGHTS_DIR"]]

BASE_AHC_MEDIANS_PATH  <- TAR_settings[["BASE_AHC_MEDIANS_PATH"]]

SURVEY <- TAR_settings[["SURVEY"]]

INFLATORS_PATH <- TAR_settings[["INFLATORS_PATH"]]
logging::loginfo(
  "Creating fixed-line inflator from given inflators file: %s", INFLATORS_PATH)
FIXED_LINE_INFLATOR <-
  TAWApost::get_fixed_line_inflator_from_file(INFLATORS_PATH)

