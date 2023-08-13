# Helper script to setup a standard TAWApost runscript
library(magrittr)
library(data.table)
library(logging)

# Setup a new logging file
logReset()
DATE_STAMP <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
setLevel("DEBUG") # can be any of `loglevels`, e.g. "INFO", "WARN", "ERROR"
addHandler(writeToFile, file = sprintf("logs/assignAS_%s.log", DATE_STAMP))
addHandler(writeToConsole)

TAR_settings <- yaml::read_yaml(TAR_settings_path)

AS_DATA_DIR <- TAR_settings[["AS_DATA_DIR"]]
AS_PROB_NAME <- TAR_settings[["AS_PROB_NAME"]]
AS_RANDOM_NUMBERS_PATH <- TAR_settings[["AS_RANDOM_NUMBERS_PATH"]]

SURVEY <- TAR_settings[["SURVEY"]]

WEIGHTS_DIR <- TAR_settings[["WEIGHTS_DIR"]]
