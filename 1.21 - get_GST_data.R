library(data.table)
library(magrittr)
library(ggplot2)

# load hes19 exp & merge on weights
hes19_exp_all <- fread("data/hes19_exp.csv")
hes19_exp_weights <- fread("data/hes19_exp_weights.csv")

hes19_exp <- merge(
  hes19_exp_all[, .(snz_hes_hhld_uid, NZHEC, Value = DVAmount)],
  hes19_exp_weights[, .(snz_hes_hhld_uid, Weight = FinalWgtExp)],
  by = "snz_hes_hhld_uid"
)

# By default, include all expenditure
hes19_exp[, Exclude_82 := FALSE]
hes19_exp[, Exclude_83 := FALSE]

#### Food ####
# All GST-inclusive
hes19_exp[NZHEC %like% "^01", ":="(Exp_Class = "Food", GST = TRUE)]

#### Alcohol & Tobacco ####
# - Exclude illegal drugs
hes19_exp[NZHEC %like% "^02", ":="(Exp_Class = "Alcohol & Tobacco", GST = TRUE)]
hes19_exp[NZHEC %like% "^02.3", ":="(Exp_Class = "Alcohol & Tobacco", GST = FALSE)] # Exempt illegal drugs

#### Clothing & Footwear ####
# All GST chargeable
hes19_exp[NZHEC %like% "^03", ":="(Exp_Class = "Clothing & Footwear", GST = TRUE)]

#### Housing and househould utilities ####
# Rent is <generally> GST-exclusive
hes19_exp[NZHEC %like% "^04.1.01", ":="(Exp_Class = "Actual rentals for housing", GST = FALSE, Exclude_82 = TRUE)] # Exempt

# particular rent-like variables are GST-chargeable
hes19_exp[NZHEC %like% "^04.1.01.2", ":="(Exp_Class = "Educational accommodation", GST = TRUE, Exclude_82 = TRUE)] #  include for GST
# Bond payments do not include GST
hes19_exp[NZHEC == "04.1.02.0.0.01", ":="(Exp_Class = "Bond payments", GST = FALSE, Exclude_82 = TRUE)] #  Exempt
# Other administration payments and charges do include GST
hes19_exp[NZHEC %like% "^04.1.02.0.0.02|^04.1.02.0.0.99", ":="(
  Exp_Class = "Payments connected with renting", GST = TRUE, Exclude_82 = TRUE
)] #  include

# Home ownership - it depends
# New housing - it depends
new_housing_exclude <- c(
  "04.2.01.1.0.01", # Purchase-contract price of new private dwelling with land
  "04.2.01.1.0.02", # Purchase-contract price of new private dwelling without land
  "04.2.01.1.0.03", # Purchase-contract price of pre-owned private dwelling with land
  "04.2.01.1.0.04", # Purchase-contract price of pre-owned private dwelling without land
  "04.2.01.1.0.05", # Purchase-contract price of other buildings with land
  "04.2.01.1.0.06", # Purchase-contract price of other buildings without land
  "04.2.01.1.0.07" # Purchase-contract price of land only
)
new_housing_include <- c(
  "04.2.01.1.0.08", # Site-preparation and construction payments for new dwellings
  "04.2.01.1.0.09" # Contract payments for construction of new buildings
)
hes19_exp[NZHEC %in% new_housing_exclude, ":="(Exp_Class = "New housing", GST = FALSE, Exclude_82 = TRUE)]
hes19_exp[NZHEC %in% new_housing_include, ":="(Exp_Class = "New housing", GST = TRUE, Exclude_82 = TRUE)]

# Exempt mortgage principal
hes19_exp[NZHEC %like% "^04.2.01.2", ":="(Exp_Class = "Mortgage principal", GST = FALSE, Exclude_82 = TRUE)] # Exempt

# Repairs & maintenance; Rates & general fees are GST-chargeable
hes19_exp[NZHEC %like% "^04.2.02|^04.2.03|^04.3", ":="(Exp_Class = "House repairs & maintenance", GST = TRUE, Exclude_82 = TRUE)]
hes19_exp[NZHEC %like% "^04.4|^04.6", ":="(Exp_Class = "Housing Rates & Fees", GST = TRUE, Exclude_82 = TRUE)]
# Body corporate fees are exempt
hes19_exp[NZHEC == "04.6.00.0.0.01", ":="(Exp_Class = "Body corporate fees", GST = FALSE, Exclude_82 = TRUE)] # Exempt

# Utilities are GST-chargeable
hes19_exp[NZHEC %like% "^04.5", ":="(Exp_Class = "Utilities", GST = TRUE)]

#### Household contents and services ####
# All GST chargeable (second-hand categories debatable)
hes19_exp[NZHEC %like% "^05", ":="(Exp_Class = "Household contents and services", GST = TRUE)]

#### Health ####
# All GST chargeable
hes19_exp[NZHEC %like% "^06", ":="(Exp_Class = "Health", GST = TRUE)]

#### Transport ####
# All GST chargeable (second-hand categories debatable)
hes19_exp[NZHEC %like% "^07.1.01", ":="(Exp_Class = "Purchase of new motor cars", GST = TRUE, Exclude_83 = TRUE)] # From EATR 83
hes19_exp[NZHEC %like% "^07.1.02", ":="(Exp_Class = "Purchase of second-hand motor cars", GST = TRUE, Exclude_83 = TRUE)] # From EATR 83
hes19_exp[NZHEC %like% "^07.1.03", ":="(Exp_Class = "Purchase of motorcycles", GST = TRUE, Exclude_83 = TRUE)] # From EATR 83
hes19_exp[NZHEC %like% "^07.2.02|^07.2.03", ":="(Exp_Class = "Fuel", GST = TRUE)]
# 07.3.04	International air transport
hes19_exp[NZHEC %like% "^07.3.04", ":="(Exp_Class = "International air transport", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^07" & is.na(Exp_Class), ":="(Exp_Class = "Other Transport", GST = TRUE)]

#### Communication ####
# All GST chargeable
hes19_exp[NZHEC %like% "^08", ":="(Exp_Class = "Communication", GST = TRUE)]

#### Recreation and culture ####
# All GST chargeable, except overseas flights and accommodation
hes19_exp[NZHEC %like% "^09", ":="(Exp_Class = "Recreation and culture", GST = TRUE)]
hes19_exp[NZHEC %like% "^09.6.00.2|^09.9.00.2", ":="(Exp_Class = "Overseas flights and accommodation", GST = FALSE)]


#### Education ####
# All GST chargeable (may be some exceptions)
hes19_exp[NZHEC %like% "^10", ":="(Exp_Class = "Education", GST = TRUE)]

# We want to exclude life insurance and financial services
hes19_exp[NZHEC %like% "^11", ":="(Exp_Class = "Miscellaneous", GST = TRUE)]
# 11.4	Insurance
hes19_exp[NZHEC %like% "^11.4", ":="(Exp_Class = "Insurance", GST = TRUE, Exclude_82 = TRUE)]
# 11.4.06.0.0.06	Combined life, health and accident insurance
hes19_exp[NZHEC %like% "^11.4.01|^11.4.06.0.0.06", ":="(Exp_Class = "Life Insurance", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^11.5", ":="(Exp_Class = "Credit services", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^11.6.02.0.0.02", ":="(Exp_Class = "Brokerage fees", GST = FALSE)] # Exempt

# hes19_exp[NZHEC %like% "^13", Exp_Class := "Other"]
hes19_exp[NZHEC %like% "^13", ":="(Exp_Class = "Other", GST = FALSE, Exclude_82 = TRUE)]
hes19_exp[NZHEC %like% "^13.1.01", ":="(Exp_Class = "Mortgage interest", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^13.1.02|^13.1.03|^13.1.04", ":="(Exp_Class = "Other interest", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^13.2|^13.3", ":="(Exp_Class = "Savings & Donations", GST = FALSE)] # Exempt
hes19_exp[NZHEC %like% "^13.4", ":="(Exp_Class = "Fines", GST = FALSE)] # Exempt


hes19_exp[NZHEC %like% "^14", ":="(Exp_Class = "Sales, trade-ins and refunds", GST = TRUE)] # Include negative expenditure & negative GST
hes19_exp[NZHEC %like% "^14.7.04", ":="(Exp_Class = "Returns from games of chance", GST = FALSE, Exclude_82 = TRUE)] # Include negative expenditure & negative GST
hes19_exp[NZHEC %like% "^88.8.88|^99", ":="(Exp_Class = "Out of scope", GST = FALSE, Exclude_82 = TRUE)] # Exempt

# Check that all values have been classified
assertthat::are_equal(hes19_exp[is.na(GST), .N], 0)

## There are a very small number of records with negative value & GST inclusive
## Don't worry about these - keep them in
# assertthat::are_equal(hes19_exp[GST == TRUE & Value < 0, .N], 0)

# Output
fwrite(hes19_exp, "data/HES19_expenditure.csv.gz")

# Check totals
vehicle_classes <- c(
  "Purchase of new motor cars",
  "Purchase of second-hand motor cars",
  "Purchase of motorcycles"
)
hes19_gst <- hes19_exp[
  , .(
    # No exclusions applied
    H_Total_Exp_81 = sum(Value),
    H_GST_Exp_81 = sum((GST == TRUE)*Value),
    # EATR 82 exclusions applied
    H_Total_Exp_82 = sum((Exclude_82 == FALSE)*Value),
    H_GST_Exp_82 = sum((Exclude_82 == FALSE & GST == TRUE)*Value),
    # EATR 82+83 exclusions applied
    H_Total_Exp_83 = sum((Exclude_82 == FALSE & Exclude_83 == FALSE)*Value),
    H_GST_Exp_83 = sum((Exclude_82 == FALSE & Exclude_83 == FALSE & GST == TRUE)*Value),
    #
    H_Exp_Food = sum((Exp_Class == "Food")*Value),
    #
    H_Exp_Rent = sum((Exp_Class == "Actual rentals for housing")*Value),
    #
    H_Exp_PurchaseOfVehicles = sum((Exp_Class %in% vehicle_classes)*Value)
  ), by = snz_hes_hhld_uid
]

# hes19_gst[, H_Exp_Total_GST_Payable := (0.15/(1 + 0.15)*H_Exp_Total_GST)]
# hes19_gst[, H_Exp_Base_GST_Payable := (0.15/(1 + 0.15)*H_Exp_Base_GST)]

# Inspect totals
hes19_gst[, lapply(.SD, function(x) sum(x) %>% scales::dollar()), .SDcols = !"snz_hes_hhld_uid"]
# # Inspect rough aggregates (unweighted) of EATRs
# hes19_gst[, .(
#   Macro_EATR_81 = sum(H_Exp_Total_GST_Payable) / sum(H_Exp_Total),
#   Micro_EATR_81 = .SD[H_Exp_Total != 0, mean(H_Exp_Total_GST_Payable / H_Exp_Total)],
#   Macro_EATR_82 = sum(H_Exp_Base_GST_Payable) / sum(H_Exp_Base),
#   Micro_EATR_82 = .SD[H_Exp_Base != 0, mean(H_Exp_Base_GST_Payable / H_Exp_Base)]
# )][, lapply(.SD, scales::percent, accuracy = 0.01)]

# Output
fwrite(hes19_gst, "data/HES19_expenditure_gst.csv.gz")
