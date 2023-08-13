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
dt_people <- fread("data/HES18_analysis_input_people.csv.gz")

# Load and merge weights for replicate 0 (point estimate only)
weights_path <- TAWApost::get_weights_path(
  weights_dir = WEIGHTS_DIR, survey = "HES18", tax_year = 18
)
dt_weights <- TAWApost::load_weights(weights_path, num_replicates = 0)
dt_people <- merge(dt_people, dt_weights, by = "H_ID")

##### Load and merge imputed GST data #####
dt_imputed_gst <- fread("data/HES18_imputed_gst.csv.gz")

# Expand the people dataset to ensure every person is duplicated by the number
# of gst imputations
all_imputations <- dt_imputed_gst[, unique(Imputation)]
all_people <- dt_people[, unique(P_ID)]
people_imputations <- expand.grid(
  P_ID = all_people, Imputation = all_imputations
) %>% setDT()
dt_people <- merge(dt_people, people_imputations, by = "P_ID", all = TRUE)

# Normalise weights by dividing by the number of imputations
num_imputations <- length(all_imputations)
dt_people[, Weight := Weight / num_imputations]

dt_people <- merge(
  dt_people, dt_imputed_gst,
  by = c("snz_hes_hhld_uid", "Imputation"), all = TRUE
)
# Assume missing values (only a small number of samples) are zero
dt_people[is.na(H_Expenditure_GST_Payable), H_Expenditure_GST_Payable := 0]

# Create new id's
dt_people[, H_ID_old := H_ID]
dt_people[, F_ID_old := F_ID]
dt_people[, P_ID_old := P_ID]
dt_people[, H_ID := H_ID + (Imputation - 1)*max(H_ID)]
dt_people[, F_ID := F_ID + (Imputation - 1)*max(F_ID)]
dt_people[, P_ID := P_ID + (Imputation - 1)*max(P_ID)]

dt_people[, assertthat::are_equal(uniqueN(H_ID), uniqueN(H_ID_old)*num_imputations)]
dt_people[, assertthat::are_equal(uniqueN(F_ID), uniqueN(F_ID_old)*num_imputations)]
dt_people[, assertthat::are_equal(uniqueN(P_ID), uniqueN(P_ID_old)*num_imputations)]

# Apportion household expenditure to individuals based on their disposable income
dt_people[, prop_DI := P_Income_Disposable / sum(P_Income_Disposable), by = H_ID]
dt_people[, P_Expenditure_GST_Payable := prop_DI*H_Expenditure_GST_Payable]
dt_people[, P_Expenditure_GST_Payable := prop_DI*H_Expenditure_GST_Payable]

# ETR numerators and denominators
dt_people <- add_etr_income_cols(dt_people)

fwrite(dt_people, "data/HES18_analysis_input_people_gst.csv.gz")

#### Families ####
family_etrs <- get_family_etrs(dt_people, weight_col)
family_etrs <- get_family_static_wealth_extra(dt_people, family_etrs)

############ Subset to the GST/income EATR ###########
family_etrs <- family_etrs[
  startsWith(as.character(ETR_Type), "9") |
    startsWith(as.character(ETR_Type), "10")
]
######################################################

# Drop families without adults
# there is one family with zero adults and two dependent children aged 12 and 18
family_etrs <- family_etrs[F_Adults > 0]

# Drop families with zero income causing undefined ETR's
family_etrs <- family_etrs[ETR_Income != 0 & !is.na(ETR_Value)]

# Drop inconsistent ETR 6+ - mismatches between paying rates and having housing assets
family_etrs <- family_etrs[
  !(ETR_Type >= 61 & F_Owner_Occupied > 0 & F_HousingCosts_Rates_PrimaryProp == 0)
]

# Exclude records with "data quality issues"
family_gst <- dt_people[, .(F_Expenditure_GST_Payable = sum(P_Expenditure_GST_Payable)), by = .(H_ID, F_ID)]
family_etrs <- merge(family_etrs, family_gst, by = c("H_ID", "F_ID"))
family_etrs[, F_Expenditure_GST_Inclusive := F_Expenditure_GST_Payable / 0.15 * (1 + 0.15)]

family_etrs[, Exclude_From_GST_ETR := (
  (F_Expenditure_GST_Inclusive / F_DI) > 4 | # Exp/Income ratio greater than 4
    F_DI < 0 | # Negative income
    # Zero food expenditure - proxy via zero total expenditure (don't have breakdown in the imputed GST data
    # Actually, exclude negative expenditure too
    F_Expenditure_GST_Inclusive <= 0
)]
num_to_exclude <- family_etrs[, .(N_before = .N, N_exclude = sum(Exclude_From_GST_ETR == TRUE)), by = ETR_Type]
num_to_exclude[, prop_exclude := N_exclude / N_before]
# Assert that we are excluding less than 2%
assertthat::are_equal(num_to_exclude[, max(prop_exclude)] < 2/100, TRUE)

family_etrs <- family_etrs[Exclude_From_GST_ETR == FALSE]

# Add income quantiles
family_etrs <- add_etr_quantiles(
  family_etrs,
  quantile_col = "Eq_ETR_Income",
  quantile_weight_col = "AdultEquivalentsWeight",
  quantile_probs = quantile_probs,
  group_by = "ETR_Type"
)
# Add wealth quantiles
family_etrs <- add_etr_quantiles(
  family_etrs,
  quantile_col = "Eq_Net_Worth",
  quantile_weight_col = "AdultEquivalentsWeight",
  quantile_probs = quantile_probs,
  group_by = "ETR_Type"
)

# Save
fwrite(family_etrs, "data/HES18_imputed_gst_family_etrs.csv.gz")
