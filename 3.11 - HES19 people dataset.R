TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/add_etr_income_cols.R")
source("src/get_etrs.R")
source("src/get_hh_fam_static.R")
source("src/add_etr_quantiles.R")
source("src/get_family_etrs.R")
source("src/get_household_etrs.R")
source("src/get_average_etrs.R")

weight_col <- "Weight"

# Ventiles
quantile_probs <- seq(0, 1, 0.05) %>% round(2)

# Load data
dt_people <- fread("TAWA Output Assigned AS/HES19_TY19_SQ.csv")

# Load and merge weights for replicate 0 (point estimate only)
# Use Expenditure Weights! Directory defined in TAWApost_setup
weights_path <- TAWApost::get_weights_path(
  weights_dir = EXP_WEIGHTS_DIR, survey = "HES19", tax_year = 19
)
dt_weights <- TAWApost::load_weights(weights_path, num_replicates = 0)
dt_people <- merge(dt_people, dt_weights, by = "H_ID")

##### Load and merge GST data #####
dt_gst <- fread("data/HES19_expenditure_gst.csv.gz")
# Drop very small number of households that do not exist in the people dataset
dt_people <- merge(dt_people, dt_gst, by = "snz_hes_hhld_uid", all.x = TRUE)

# Apportion household expenditure to individuals based on their disposable income
dt_people[, prop_DI := P_Income_Disposable / sum(P_Income_Disposable), by = H_ID]


#### Imputed rents
# Add TA
dt_hes_to_ta <- fread("data/HES19_concordance_hes_to_ta.csv")
dt_people <- merge(dt_people, dt_hes_to_ta, by = "snz_hes_hhld_uid", all.x = TRUE)
assertthat::assert_that(dt_people[is.na(ta_code), .N] == 0)

# Add number of bedrooms
dt_bedrooms <- fread("data/HES19_hh_bedrooms.csv")
# Enfore bedrooms to be between 1-5. Assume unknown is equal to 1
dt_bedrooms[is.na(num_bedrooms), num_bedrooms := 1]
dt_bedrooms[num_bedrooms > 5, num_bedrooms := 5]
assertthat::assert_that(dt_bedrooms[, all(num_bedrooms %between% c(1, 5))])

dt_people <- merge(dt_people, dt_bedrooms, by = "snz_hes_hhld_uid", all.x = TRUE)
assertthat::assert_that(dt_people[is.na(num_bedrooms), .N] == 0)

#### Add imputed rents from average rent ####
dt_rents <- fread("data/rents_2019.csv.gz")
dt_people <- merge(
  dt_people, dt_rents[, .(ta_code, num_bedrooms, Rent_GeoMean)],
  by = c("ta_code", "num_bedrooms"), all.x = TRUE
)
dt_people[, H_AverageRent_Imputed_Rent := 0] # initialise with zeros
dt_people[
  H_Tenure_Owned == TRUE,
  H_AverageRent_Imputed_Rent := Rent_GeoMean # a very small number are NA
  ]
dt_people[H_Tenure_Owned == TRUE & is.na(Rent_GeoMean), .N]


fwrite(dt_people, "data/HES19_analysis_input_people.csv.gz")