TAR_settings_path <- "TAR_settings.yaml"
source('src/assignAS_setup.R')
devtools::load_all("tawaadminas")

SURVEY <- "HES19"

SQ_key <- "SQ"
reform_keys <- c()
scenarios <- c(SQ_key, reform_keys)
tax_year <- 19

tawa_paths <- TAWApost::get_tawa_paths(
  scenarios, tax_year, survey = SURVEY, tawa_output_dir = "TAWA Output Full AS"
)

fit_paths <-
  list(
    Ben = file.path(
      AS_DATA_DIR, paste0(SURVEY, "_logit_Ben_", AS_PROB_NAME, ".rds")),
    NZS = file.path(
      AS_DATA_DIR, paste0(SURVEY, "_logit_NZS_", AS_PROB_NAME, ".rds")),
    Other = file.path(
      AS_DATA_DIR, paste0(SURVEY, "_logit_Other_", AS_PROB_NAME, ".rds"))
  )

TAWAadminAS::standard_tar_assign_AS(
  tawa_paths, WEIGHTS_DIR, reform_keys, SQ_key, SURVEY, tax_year, fit_paths,
  output_dir = "TAWA Output Assigned AS",
  random_numbers_path = AS_RANDOM_NUMBERS_PATH)