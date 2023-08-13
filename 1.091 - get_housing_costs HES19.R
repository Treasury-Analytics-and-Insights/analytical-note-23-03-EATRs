exp_query <- "
SELECT [snz_hes_hhld_uid]
, [NZHEC]
, [DVAmount]
FROM [IDI_Adhoc].[clean_read_HES].[hes_expenditure_1819]
"

hes_expenditure <- survey2tawa::get_idi_table(exp_query)

Housing_Cost_IDI <- copy(hes_expenditure)

##########################
# Housing cost categories
##########################
Housing_Cost_IDI[, Class := "H_AllOtherCosts"]

# Rent
Housing_Cost_IDI[NZHEC %in% c("04.1.01.1.0.01", "04.1.01.1.0.02"), 
                 Class := "H_HousingCosts_Rent"]
Housing_Cost_IDI[NZHEC %in% c("04.1.02.0.0.01", "04.1.02.0.0.02",  "04.1.02.0.0.99"), 
                 Class := "H_HousingCosts_RentAdditional"]

Housing_Cost_IDI[NZHEC %in% c("04.1.01.1.0.03"),  
                 Class := "H_HousingCosts_Rent_AdditionalProp"]

# Mortgage
Housing_Cost_IDI[NZHEC %in% c("04.2.01.2.0.01", "04.2.01.2.0.02"), 
                 Class := "H_HousingCosts_MortgagePrincipal"]
Housing_Cost_IDI[NZHEC %in% c("13.1.01.0.1.01", "13.1.01.0.1.02",
                              "13.1.01.0.2.01", "13.1.01.0.2.02"), 
                 Class := "H_HousingCosts_MortgageInterest"]

Housing_Cost_IDI[NZHEC %in% c("04.2.01.2.0.03", "04.2.01.2.0.04"), 
                 Class := "H_HousingCosts_MortgagePrincipal_AdditionalProp"]
Housing_Cost_IDI[NZHEC %in% c("13.1.01.0.1.03", "13.1.01.0.1.04", 
                              "13.1.01.0.2.03", "13.1.01.0.2.04"), 
                 Class := "H_HousingCosts_MortgageInterest_AdditionalProp"]

Housing_Cost_IDI[NZHEC %in% c("11.5.01.0.1.01", "11.5.01.0.1.02"), 
                 Class := "H_HousingCosts_MortgageFees"]

# Insurance
Housing_Cost_IDI[NZHEC %in% c("11.4.02.0.0.00", "11.4.06.0.0.01", 
                              "11.4.06.0.0.03","11.4.06.0.0.04", "11.4.06.0.0.99"), 
                 Class := "H_HousingCosts_Insurance"]

# Rates
Housing_Cost_IDI[NZHEC %in% c("04.4.03.1.0.01", # local rates
                              "04.4.03.1.0.02", # regional rates
                              "04.4.03.1.0.05", # combined local + regional rates
                              "04.4.03.2.0.99", # other payments to local authorities
                              "04.4.01.0.0.00"), # water rates
                 Class := "H_HousingCosts_Rates_PrimaryProp"]

Housing_Cost_IDI[NZHEC %in% c("04.4.03.1.0.03", # local rates
                              "04.4.03.1.0.04", # regional rates
                              "04.4.03.1.0.06"), # combined local + regional rates
                 Class := "H_HousingCosts_Rates_AdditionalProp"]

# Other
Housing_Cost_IDI[NZHEC %in% c("04.6.00.0.0.01"),
                 Class := "H_HousingCosts_BodyCorp"]

Housing_Cost_IDI[NZHEC %in% c("04.6.00.0.0.02"), # combinations of rates, mortgage, insurance, maintenance
                 Class := "H_HousingCosts_Combination"]

Housing_Cost_IDI[NZHEC %in% c("04.6.00.0.0.99"), 
                 Class := "H_HousingCosts_Other"]

Housing_Cost_IDI[NZHEC == "88.8.88.8.8.03", 
                 Class := "H_HousingCosts_Unknown"]

# Convert to wide output format
housing_costs_long <- Housing_Cost_IDI[, .(Value = sum(DVAmount)), by = .(snz_hes_hhld_uid, Class)]
housing_costs <- dcast(housing_costs_long, ... ~ Class, value.var = "Value", fill = 0)

fwrite(housing_costs, "data/HES19_housing_costs.csv")