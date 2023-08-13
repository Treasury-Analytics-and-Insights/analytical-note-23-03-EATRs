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

# ETR numerators and denominators
dt_people <- add_etr_income_cols(dt_people)

#### Families ####
family_etrs <- get_family_etrs(dt_people, weight_col)
family_etrs <- get_family_static_wealth_extra(dt_people, family_etrs)

# Drop families without adults
# there is one family with zero adults and two dependent children aged 12 and 18
family_etrs <- family_etrs[F_Adults > 0]

# check exclusions
family_etrs[, .(
  All_Pop = sum(Weight),
  All_Sample = .N,
  Ex1_Pop = sum(Weight*(ETR_Income != 0 & !is.na(ETR_Value))),
  Ex1_Sample = sum(ETR_Income != 0 & !is.na(ETR_Value)),
  Ex2_Pop = sum(Weight*(ETR_Income != 0 & !is.na(ETR_Value) & !(ETR_Type >= 61 & F_Owner_Occupied > 0 & F_HousingCosts_Rates_PrimaryProp == 0))),
  Ex2_Sample = sum(ETR_Income != 0 & !is.na(ETR_Value) & !(ETR_Type >= 61 & F_Owner_Occupied > 0 & F_HousingCosts_Rates_PrimaryProp == 0))
), by = ETR_Type]

# Drop families with zero income causing undefined ETR's
family_etrs <- family_etrs[ETR_Income != 0 & !is.na(ETR_Value)]

# Drop inconsistent ETR 6+ - mismatches between paying rates and having housing assets
family_etrs <- family_etrs[
  !(ETR_Type >= 61 & F_Owner_Occupied > 0 & F_HousingCosts_Rates_PrimaryProp == 0)
]

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
fwrite(family_etrs, "data/HES18_analysis_input_family_etrs.csv.gz")