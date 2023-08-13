# Helper script to setup a standard TAWArun runscript
library(data.table)
library(magrittr)
library(logging)

# Setup a new logging file
logReset()
DATE_STAMP <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
setLevel("DEBUG") # can be any of `loglevels`, e.g. "INFO", "WARN", "ERROR"
addHandler(writeToFile, file = sprintf("logs/TAWArun_%s.log", DATE_STAMP))
addHandler(writeToConsole)

TAR_settings <- yaml::read_yaml(TAR_settings_path)

# TAWA parameters (currently an excel file)
PARAMETERS_DIR <- TAR_settings[["PARAMETERS_DIR"]]

# Get Inflator and Mapping files
INFLATORS_PATH <- TAR_settings[["INFLATORS_PATH"]]
INFLATORS_MAPPING_PATH <- TAR_settings[["INFLATORS_MAPPING_PATH"]]

SURVEY <- TAR_settings[["SURVEY"]]

# Get TAWA input databases
# there is no need for this to be a data.table, but for now this is required by
# TAWArun
DB_PATH <- TAR_settings[["DB_PATH"]]
