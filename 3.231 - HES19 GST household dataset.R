TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/add_etr_income_cols.R")
source("src/get_etrs.R")
source("src/get_hh_fam_static.R")
source("src/add_etr_quantiles.R")
source("src/get_household_etrs.R")
source("src/get_household_etrs.R")
source("src/get_average_etrs.R")

# Ventiles
quantile_probs <- seq(0, 1, 0.05) %>% round(2)

dt_people <- fread("data/HES19_analysis_input_people.csv.gz")

#### ETR numerators and denominators ####
households <- dt_people[, .(
  # No exclusions
  ETR_81_Total_Exp = sum(H_Total_Exp_81),
  # Exclude housing, interest, savings, donations, insurance
  ETR_82_Total_Exp = sum(H_Total_Exp_82),
  # Exclude vehicles
  ETR_83_Total_Exp = sum(H_Total_Exp_83),
  # Include only rent
  ETR_841_Total_Exp = sum((H_Total_Exp_83 + H_Exp_Rent)),
  # Include only imputed rent
  ETR_842_Total_Exp = sum((H_Total_Exp_83 + H_AverageRent_Imputed_Rent)),
  # Include rent and imputed rent
  ETR_843_Total_Exp = sum((H_Total_Exp_83 + H_Exp_Rent + H_AverageRent_Imputed_Rent)),
  #
  ETR_81_GST_Exp = sum(H_GST_Exp_81),
  ETR_82_GST_Exp = sum(H_GST_Exp_82),
  ETR_83_GST_Exp = sum(H_GST_Exp_83),
  ETR_841_GST_Exp = sum(H_GST_Exp_83),
  ETR_842_GST_Exp = sum(H_GST_Exp_83),
  ETR_843_GST_Exp = sum(H_GST_Exp_83),
  #
  Food_Exp = sum(H_Exp_Food)
), by = .(H_ID)]

household_etrs <- melt(
  households,
  measure.vars = patterns(
    "ETR_Total_Exp" = (glob2rx("ETR_*_Total_Exp")),
    "ETR_GST_Exp" = (glob2rx("ETR_*_GST_Exp"))
  ),
  variable.name = "ETR_Type"
)

etrs <- names(households) %>%
  .[. %like% "ETR_[0-9]{1,3}_GST_Exp"] %>%
  stringr::str_extract("(?!ETR_)[0-9]{1,3}") %>% as.numeric()
household_etrs[, ETR_Type := etrs[ETR_Type]]

# household_etrs[, ":="(
#   ETR_Type = stringr::str_extract(ETR_Type, "(?!ETR_)[0-9]{1,2}") %>% as.numeric()
# )]

household_etrs[, ":="(
  ETR_GST_Payable = 0.15/(1 + 0.15)*ETR_GST_Exp
)]

household_etrs[, ":="(
  ETR_Value = ETR_GST_Payable / ETR_Total_Exp * 100
)]

household_etrs <- add_household_constants(dt_people, household_etrs)

# Family equivalised expenditure
household_etrs[, Eq_ETR_Exp := ETR_Total_Exp / Household_MOECD_Eq_Factor]

# Family weights
household_etrs[, PeopleWeight := H_People*Weight]
household_etrs[, AdultEquivalentsWeight := Household_MOECD_Eq_Factor*Weight]

# ######################################################
# 
# Drop families without adults
household_etrs <- household_etrs[H_Adults > 0]

# Drop families with zero income causing undefined ETR's
household_etrs <- household_etrs[ETR_Total_Exp != 0 & !is.na(ETR_Value)]

# # Plot to check
# stop()
# library(ggplot2)
# ggplot(household_etrs, aes(x = ETR_Value)) + geom_freqpoly(bins = 100) + facet_grid(ETR_Type~.)
# household_etrs[ETR_GST_Payable > 0.15/(1 + 0.15)*ETR_Total_Exp, Data_Type := "More GST than total exp suggests"]
# household_etrs[ETR_GST_Payable < 0.15/(1 + 0.15)*ETR_Total_Exp, Data_Type := "Less GST than total exp suggests"]
# ggplot(household_etrs, aes(x = ETR_Total_Exp, y = ETR_GST_Payable, colour = Data_Type)) + geom_point() + facet_grid(ETR_Type~.)

# Exclude records with "data quality issues"
household_etrs[, Exclude_From_GST_ETR := (
  (ETR_Total_Exp / H_DI) > 4 | # Exp/Income ratio greater than 4
    H_DI < 0 | # Negative income
    # Zero food expenditure - proxy via zero total expenditure (don't have breakdown in the imputed GST data)
    # Actually, exclude negative expenditure too
    ETR_Total_Exp <= 0
)]
num_to_exclude <- household_etrs[, .(N_before = .N, N_exclude = sum(Exclude_From_GST_ETR == TRUE)), by = ETR_Type]
num_to_exclude[, prop_exclude := N_exclude / N_before]
# Assert that we are excluding less than 2%
assertthat::are_equal(num_to_exclude[, max(prop_exclude)] < 2/100, TRUE)

# Apply exclusions
household_etrs <- rbindlist(list(
  # No exclusions
  household_etrs[ETR_Type == 81][, Exclusions := "No exclusions"],
  # Data quality exclusions - add variant to 81
  household_etrs[ETR_Type == 81 & Exclude_From_GST_ETR == FALSE][, ETR_Type := 811][, Exclusions := "Applied exclusions"],
  # Data quality exclusions are applied to all other variants
  household_etrs[ETR_Type != 81 & Exclude_From_GST_ETR == FALSE][, Exclusions := "Applied exclusions"],
  # No Data quality exclusions
  household_etrs[ETR_Type != 81][, Exclusions := "No exclusions"]
))

# Add expenditure quantiles
household_etrs <- add_etr_quantiles(
  household_etrs,
  quantile_col = "Eq_ETR_Exp",
  quantile_weight_col = "AdultEquivalentsWeight",
  quantile_probs = quantile_probs,
  group_by = "ETR_Type"
)

# Add income quantiles
household_etrs[, Eq_DI := H_DI / Household_MOECD_Eq_Factor]
household_etrs <- add_etr_quantiles(
  household_etrs,
  quantile_col = "Eq_DI",
  quantile_weight_col = "AdultEquivalentsWeight",
  quantile_probs = quantile_probs,
  group_by = "ETR_Type"
)

# Save
fwrite(household_etrs, "data/HES19_analysis_input_gst_household_etrs.csv.gz")

# Plot to check
average_exp_eatrs <- household_etrs[, .(
  Mean = sum(AdultEquivalentsWeight*ETR_Value)/sum(AdultEquivalentsWeight),
  Median = TAWApost::weighted_quantile(ETR_Value, AdultEquivalentsWeight, 0.5)
), by = .(ETR_Type, Eq_ETR_Exp_AdultEquivalentsWeight_quantile, Exclusions)] %>%
  melt(id.vars = c("ETR_Type", "Eq_ETR_Exp_AdultEquivalentsWeight_quantile", "Exclusions"))

ggplot(average_exp_eatrs, aes(x = Eq_ETR_Exp_AdultEquivalentsWeight_quantile, y = value, linetype = Exclusions, colour = variable, group = interaction(variable, Exclusions))) +
  geom_hline(yintercept = 100*0.15/(1 + 0.15), colour = "red") +
  geom_line() +
  facet_grid(~ETR_Type) +
  theme_minimal()

