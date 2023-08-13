library(data.table)
library(magrittr)

# Load data
dt_people <- fread("TAWA Output Assigned AS/HES18_TY18_SQ.csv")
dt_people_no_acc_income <- fread("TAWA Output Assigned AS/HES18_TY18_SQ_without_ACC_income.csv")

# Merge the "without ACC income" columns for taxable income and tax
dt_people <- merge(
  dt_people,
  dt_people_no_acc_income[, .(
    snz_hes_uid,
    P_Income_Taxable_no_ACC_income = P_Income_Taxable,
    P_Income_TaxPayable_no_ACC_income = P_Income_TaxPayable
  )],
  by = "snz_hes_uid", all = TRUE
)

# Remove Replicate column (bug with "2 - assignAS.R")
dt_people[, Replicate := NULL]

# Add PIE income and tax from IDI
pie_income <- fread("data/HES18_pie_income.csv")
pie_data_cols <- setdiff(names(pie_income), "snz_hes_uid")
dt_people <- merge(dt_people, pie_income, by = "snz_hes_uid", all.x = TRUE)
# Set NA's to zero and round the rest
dt_people[
  , (pie_data_cols) := lapply(.SD, function(x) {
    ifelse(is.na(x), 0, round(x))
  }),
  .SDcols = pie_data_cols
]

# Add net worth
people_wealth <- fread("data/people_wealth.csv")
wealth_data_cols <- setdiff(names(people_wealth), "snz_hes_uid")
dt_people <- merge(dt_people, people_wealth, by = "snz_hes_uid", all.x = TRUE)
# Set NA's to zero
dt_people[
  , (wealth_data_cols) := lapply(.SD, function(x) {
    ifelse(is.na(x), 0, x)
  }),
  .SDcols = wealth_data_cols
]

# Add investment gains
people_investment_capital_gains <- fread("data/investment_gains.csv")
investment_data_cols <- setdiff(names(people_investment_capital_gains), "snz_hes_uid")
dt_people <- merge(dt_people, people_investment_capital_gains, by = "snz_hes_uid", all.x = TRUE)
# Set NA's to zero
dt_people[
  , (investment_data_cols) := lapply(.SD, function(x) {
    ifelse(is.na(x), 0, x)
  }),
  .SDcols = investment_data_cols
]

# Add detailed <household> housing costs - in particular we need rates
housing_costs <- fread("data/housing_costs.csv")
housing_data_cols <- setdiff(names(housing_costs), "snz_hes_hhld_uid")
dt_people <- merge(dt_people, housing_costs, by = "snz_hes_hhld_uid", all.x = TRUE)
# Set NA's to zero
dt_people[
  , (housing_data_cols) := lapply(.SD, function(x) {
    ifelse(is.na(x), 0, x)
  }),
  .SDcols = housing_data_cols
]

#### Apportion household rates to individuals based on their share of household assets ####
# Note that if an individual has zero assets then they get zero rates

# Name some individual asset categories
dt_people[, ":="(
  P_Investment_Property = Other_Residential + Non_Residential + Vacant_Land,
  P_OwnerOccupied_Property = Owner_Occupied
)]

dt_people[, ":="(
  H_OwnerOccupied_Property = sum(P_OwnerOccupied_Property),
  H_Investment_Property = sum(P_Investment_Property)
), by = "H_ID"]

dt_people[, ":="(
  P_owner_occupied_prop = P_OwnerOccupied_Property / H_OwnerOccupied_Property,
  P_investment_prop = P_Investment_Property / H_Investment_Property
)]

dt_people[is.na(P_owner_occupied_prop), P_owner_occupied_prop := 0]
dt_people[is.na(P_investment_prop), P_investment_prop := 0]

dt_people[, ":="(
  P_HousingCosts_Rates_PrimaryProp = P_owner_occupied_prop*H_HousingCosts_Rates_PrimaryProp,
  P_HousingCosts_Rates_AdditionalProp = P_investment_prop*H_HousingCosts_Rates_AdditionalProp,
  #
  P_HousingCosts_MortgageInterest = P_owner_occupied_prop*H_HousingCosts_MortgageInterest,
  P_HousingCosts_Insurance = P_owner_occupied_prop*H_HousingCosts_Insurance,
  P_HousingCosts_BodyCorp = P_owner_occupied_prop*H_HousingCosts_BodyCorp,
  P_HousingCosts_Other = P_owner_occupied_prop*H_HousingCosts_Other
)]

# Check totals match
full_primary <- dt_people[
  , lapply(.SD, first),
  .SDcols = c("H_HousingCosts_Rates_PrimaryProp"),
  by = H_ID
][, lapply(.SD, sum), .SDcols = !"H_ID"]

has_assets_primary <- dt_people[
  H_OwnerOccupied_Property != 0, lapply(.SD, first),
  .SDcols = c("H_HousingCosts_Rates_PrimaryProp"),
  by = H_ID
  ][, lapply(.SD, sum), .SDcols = !"H_ID"]

apportioned_primary <- dt_people[
  , lapply(.SD, sum),
  .SDcols = c("P_HousingCosts_Rates_PrimaryProp")
]
assertthat::are_equal(
  has_assets_primary$H_HousingCosts_Rates_PrimaryProp,
  apportioned_primary$P_HousingCosts_Rates_PrimaryProp
)

full_additional <- dt_people[
  , lapply(.SD, first),
  .SDcols = c("H_HousingCosts_Rates_AdditionalProp"),
  by = H_ID
  ][, lapply(.SD, sum), .SDcols = !"H_ID"]

has_assets_additional <- dt_people[
  H_Investment_Property != 0, lapply(.SD, first),
  .SDcols = c("H_HousingCosts_Rates_AdditionalProp"),
  by = H_ID
  ][, lapply(.SD, sum), .SDcols = !"H_ID"]

apportioned_additional <- dt_people[
  , lapply(.SD, sum),
  .SDcols = c("P_HousingCosts_Rates_AdditionalProp")
]
assertthat::are_equal(
  has_assets_additional$H_HousingCosts_Rates_PrimaryProp,
  apportioned_additional$P_HousingCosts_Rates_PrimaryProp
)
# Add TA
dt_hes_to_ta <- fread("data/HES18_concordance_hes_to_ta.csv")
dt_people <- merge(dt_people, dt_hes_to_ta, by = "snz_hes_hhld_uid", all.x = TRUE)
assertthat::assert_that(dt_people[is.na(ta_code), .N] == 0)

# Add number of bedrooms
dt_bedrooms <- fread("data/HES18_hh_bedrooms.csv")
# Enfore bedrooms to be between 1-5. Assume unknown is equal to 1
dt_bedrooms[is.na(num_bedrooms), num_bedrooms := 1]
dt_bedrooms[num_bedrooms > 5, num_bedrooms := 5]
assertthat::assert_that(dt_bedrooms[, all(num_bedrooms %between% c(1, 5))])

dt_people <- merge(dt_people, dt_bedrooms, by = "snz_hes_hhld_uid", all.x = TRUE)
assertthat::assert_that(dt_people[is.na(num_bedrooms), .N] == 0)

# Add HPI (SPAR) growth by TA
dt_hpi_growth_rates <- fread("data/hpi_growth_rates.csv")
dt_people <- merge(
  dt_people,
  dt_hpi_growth_rates,
  by = c("ta_code", "num_bedrooms"),
  all.x = TRUE
)
dt_people[is.na(hpi_short_term_growth_rate), hpi_short_term_growth_rate := 0]
dt_people[is.na(hpi_long_term_growth_rate), hpi_long_term_growth_rate := 0]

# Calculate owner-occupied gains
dt_people[, Short_Run_Owner_Occupied_Gain := hpi_short_term_growth_rate*Owner_Occupied]
dt_people[, Long_Run_Owner_Occupied_Gain := hpi_long_term_growth_rate*Owner_Occupied]

# plot_data <- rbind(
#   dt_people[, .(variable = "short_run_owner-occupied_housing", value = Short_Run_Owner_Occupied_Gain, Owner_Occupied)],
#   dt_people[, .(variable = "long_run_owner-occupied_housing", value = Long_Run_Owner_Occupied_Gain, Owner_Occupied)]
# )
# 
# ggplot(
#   plot_data[Owner_Occupied != 0], aes(x = value, colour = variable)
# ) +
#   geom_density() +
#   coord_cartesian(xlim = c(-1e4, 1e5))
# 
# ggsave("plots/housing_gains.png")


# Name some household asset categories
dt_people[, ":="(
  H_OwnerOccupied_Property = sum(Owner_Occupied)
), by = "H_ID"]


### Housing costs to convert gross imputed rent to net imputed rent ####
dt_people[, ":="(
  Imputed_Rent_Housing_Costs = (
    P_HousingCosts_MortgageInterest +
    P_HousingCosts_Insurance +
    P_HousingCosts_BodyCorp +
    P_HousingCosts_Other
  )
)]

#### Add imputed rents from average rent ####
dt_rents <- fread("data/rents_2018.csv.gz")
dt_people <- merge(
  dt_people, dt_rents[, .(ta_code, num_bedrooms, Rent_GeoMean)],
  by = c("ta_code", "num_bedrooms"), all.x = TRUE
)
dt_people[, H_AverageRent_Imputed_Rent := 0] # initialise with zeros
dt_people[
  H_OwnerOccupied_Property > 0,
  H_AverageRent_Imputed_Rent := Rent_GeoMean # a very small number are NA
]
dt_people[H_OwnerOccupied_Property > 0 & is.na(Rent_GeoMean), .N]

dt_people[, P_AverageRent_Imputed_Rent := P_owner_occupied_prop*H_AverageRent_Imputed_Rent]

#### Add imputed rents from average yield ####
dt_yields <- fread("data/yields_2018.csv.gz")
dt_people <- merge(
  dt_people, dt_yields[, .(ta_code, num_bedrooms, Rent_Yield_GeoMean)],
  by = c("ta_code", "num_bedrooms"), all.x = TRUE
)
dt_people[, H_AverageYield_Imputed_Rent := 0] # initialise with zeros
dt_people[
  H_OwnerOccupied_Property > 0,
  H_AverageYield_Imputed_Rent := Rent_Yield_GeoMean*H_OwnerOccupied_Property/52
]
dt_people[H_OwnerOccupied_Property > 0 & is.na(Rent_Yield_GeoMean), .N]

dt_people[, P_AverageYield_Imputed_Rent := P_owner_occupied_prop*H_AverageYield_Imputed_Rent]

# Format column order for output
setcolorder(dt_people, c("snz_hes_hhld_uid", "snz_hes_uid", "H_ID", "F_ID", "P_ID"))

fwrite(dt_people, "data/HES18_analysis_input_people.csv.gz")
