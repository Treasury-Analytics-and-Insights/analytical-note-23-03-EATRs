library(data.table)
library(magrittr)
TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/get_hh_fam_static.R")
source("src/weighted_gini.R")

etrs_list <- list(
  c(1, 2, 3, 4),
  c(51, 52, 53, 54),
  c(61, 62, 71, 72, 73, 74),
  c(75, 76, 77, 78),
  c(91, 92, 93, 94),
  c(95, 96, 97, 98)
)

# Load data
dt_people <- fread("data/HES18_analysis_input_people.csv.gz")

# Load and merge weights for replicate 0 (point estimate only)
weights_path <- TAWApost::get_weights_path(
  weights_dir = WEIGHTS_DIR, survey = "HES18", tax_year = 18
)
dt_weights <- TAWApost::load_weights(weights_path, num_replicates = 0)

all_family_etrs <- rbind(
  fread("data/HES18_analysis_input_family_etrs.csv.gz"),
  fread("data/HES18_imputed_gst_family_etrs.csv.gz"),
  fill = TRUE
)

for (etrs in etrs_list) {
  loginfo("Calculating family ginis for %s", etrs)
  family_etrs <- all_family_etrs[ETR_Type %in% etrs]
  
  families <- dt_people[, .(
    F_Income_Taxable_noACC = sum(P_Income_Taxable_no_ACC_income),
    F_Income_TaxPayable_noACC = sum(P_Income_TaxPayable_no_ACC_income),
    F_Transfers_NonTaxable = sum(P_Benefits_All_NonTaxable + P_FamilyAssistance_Total + P_TaxCredit_IETC)
  ), by = .(F_ID, H_ID)]
  
  dt_fam_static <- get_family_static(dt_people)
  families <- merge(families, dt_fam_static, by = c("F_ID", "H_ID"))
  
  families <- get_family_static_wealth_extra(dt_people, families)
  
  families <- merge(families, dt_weights, by = "H_ID", all = TRUE)
  families[, AdultEquivalentsWeight := Weight*Family_MOECD_Eq_Factor]
  
  wealth_cols <- c(
    "F_Net_Worth",
    "F_Assets",
    "F_Liabilities",
    "F_Owner_Occupied",
    "F_Other_Residential",
    "F_Non_Residential",
    "F_Vacant_Land",
    "F_Kiwisaver_Assets",
    "F_Other_Superannuation_Funds",
    "F_Investment_Funds",
    "F_Listed_Equities",
    "F_Unlisted_Equities",
    "F_Unincorporated_Bus"
  )
  
  # Calculate ginis
  overall_ginis <- families[
    , c(
      .(
        Gini_Taxable_Income = weighted_gini(F_TI, AdultEquivalentsWeight),
        Gini_Disposable_Income = weighted_gini(F_DI, AdultEquivalentsWeight)
      ),
      lapply(.SD, function(x) weighted_gini(x / Family_MOECD_Eq_Factor, AdultEquivalentsWeight))
    ),
    .SDcols = wealth_cols
  ]
  etr_income_ginis <- family_etrs[
    , c(.(
      Gini_Eq_ETR_Income = weighted_gini(Eq_ETR_Income, AdultEquivalentsWeight),
      Gini_Eq_ETR_Net_Income = weighted_gini(Eq_ETR_Net_Income, AdultEquivalentsWeight),
      Gini_ETR_Income = weighted_gini(ETR_Income, AdultEquivalentsWeight),
      Gini_ETR_DI = weighted_gini(ETR_DI, AdultEquivalentsWeight),
      Gini_TI = weighted_gini(F_TI, AdultEquivalentsWeight),
      Gini_DI = weighted_gini(F_DI, AdultEquivalentsWeight),
      # Gini_All_TI = overall_ginis$Gini_Taxable_Income,
      # Gini_All_DI = overall_ginis$Gini_Disposable_Income,
      Population = sum(Weight),
      Sample = .N
    ), overall_ginis), by = ETR_Type
  ]
  etr_income_ginis_for_display <- etr_income_ginis[
    , c(
      lapply(.SD, function(x) scales::percent(x, accuracy = 0.1)),
      .(Population = Population, Sample = Sample)
    ), by = ETR_Type, .SDcols = !c("Population", "Sample")]
  print(etr_income_ginis_for_display)
  
  etr_income_ginis_long <- melt(
    etr_income_ginis, id.vars = c("ETR_Type", "Population", "Sample"), value.name = "Value", variable.name = "Variable"
  )
  etr_income_ginis_long[, ":="(
    Value = 100*Value, # Convert to percentage
    Table = "Ginis",
    Rounding_Rule = "Gini",
    Sheet = "Values"
  )]
  
  
  output_path <- file.path(
    sprintf(
      "data/HES18_analysis_gini_raw_output_%s.csv.gz",
      paste(etrs, collapse = "-")
    )
  )
  fwrite(etr_income_ginis_long, output_path)
}
