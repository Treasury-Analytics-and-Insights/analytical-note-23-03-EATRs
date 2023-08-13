TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")

# Imputation with mice based on exploratory work in TAR 237/Luke2/2022-06-01_imputed_data.Rmd
# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/mice_3.11.0.tar.gz", repos = NULL)
library(mice)
# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/miceadds_3.10-28.tar.gz", repos = NULL)
library(miceadds)

library(data.table)
library(magrittr)
library(ggplot2)
source("src/get_hh_fam_static.R")

hes19_gst <- fread("data/HES19_expenditure_gst.csv.gz")
hes19_gst <- hes19_gst[, .(
  snz_hes_hhld_uid,
  H_Expenditure_GST_Payable = 0.15/(1 + 0.15)*H_GST_Exp_83 # All exclusions applied
)]

tawa_paths <- list(
  "HES18" = "TAWA Output Assigned AS/HES18_TY18_SQ.csv",
  "HES19" = "TAWA Output Assigned AS/HES19_TY19_SQ.csv"
)

# Load HES18
hes18_people <- fread(tawa_paths[["HES18"]])
hes18_weights_path <- TAWApost::get_weights_path(
  weights_dir = WEIGHTS_DIR, survey = "HES18", tax_year = 18
)
hes18_weights <- TAWApost::load_weights(hes18_weights_path, num_replicates = 0)
hes18_people <- merge(hes18_people, hes18_weights[, .(H_ID, Weight)], by = "H_ID")
hes18_households <- get_household_static(hes18_people)

# Merge income
hes18_hh_extra <- hes18_people[, .(
  H_Income_Disposable = sum(P_Income_Disposable),
  H_Income_SelfEmployed = sum(P_Income_SelfEmployed),
  H_Income_StudentAllowance = sum(P_Income_StudentAllowance),
  H_Income_WageSalary = sum(P_Income_WageSalary),
  H_Benefits_Accommodation_Abated = sum(P_Benefits_Accommodation_Abated),
  H_Benefits_JSS_Amount_Abated = sum(P_Benefits_JSS_Amount_Abated),
  H_Benefits_SLP_Amount_Abated = sum(P_Benefits_SLP_Amount_Abated),
  H_Benefits_SPS_Amount_Abated = sum(P_Benefits_SPS_Amount_Abated),
  H_Super_Amount_Gross = sum(P_Super_Amount_Gross),
  H_FamilyAssistance_FTC_Abated = sum(P_FamilyAssistance_FTC_Abated),
  H_FamilyAssistance_IWTC_Abated = sum(P_FamilyAssistance_IWTC_Abated),
  H_FamilyAssistance_BestStart = sum(P_FamilyAssistance_BestStart),
  H_Students = sum(P_Attributes_FullOrPartTimeEducation > 0),
  H_MaxAge = max(P_Attributes_Age)
), by = snz_hes_hhld_uid]
hes18_households <- merge(hes18_households, hes18_hh_extra, by = "snz_hes_hhld_uid")

# Merge on housing costs
hes18_housing_costs <- fread("data/housing_costs.csv")
hes18_households <- merge(hes18_households, hes18_housing_costs, by = "snz_hes_hhld_uid", all.x = TRUE)
hes18_households[
  , names(hes18_housing_costs) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
  .SDcols = names(hes18_housing_costs)
]
hes18_households[, ":="(
  H_HousingCosts_Mortgage = (
    H_HousingCosts_MortgageInterest +
      H_HousingCosts_MortgageInterest_AdditionalProp +
      H_HousingCosts_MortgagePrincipal +
      H_HousingCosts_MortgagePrincipal_AdditionalProp +
      H_HousingCosts_MortgageFees
  )
)]
hes18_households[H_Tenure_Rented == TRUE | H_HousingCosts_Rent > 0, H_Tenure_Type := "Paying rent"]
hes18_households[H_Tenure_Owned == TRUE & H_HousingCosts_Mortgage > 0, H_Tenure_Type := "Paying mortgage"]
hes18_households[H_Tenure_Owned == TRUE & H_HousingCosts_Mortgage == 0, H_Tenure_Type := "Owned outright"]
hes18_households[is.na(H_Tenure_Type) & H_HousingCosts_Rates_PrimaryProp > 0, H_Tenure_Type := "Owned outright"]

# Check there are not many NA's, drop them
logwarn(sprintf("Dropping %s households with unknown Tenure Type", hes18_households[is.na(H_Tenure_Type), .N]))
hes18_households <- hes18_households[!is.na(H_Tenure_Type)]

# Load HES19 - use expenditure weights! (defined in src/TAWApost_setup.R)
hes19_people <- fread(tawa_paths[["HES19"]])
hes19_weights_path <- TAWApost::get_weights_path(
  weights_dir = EXP_WEIGHTS_DIR, survey = "HES19", tax_year = 19
)
hes19_weights <- TAWApost::load_weights(hes19_weights_path, num_replicates = 0)
hes19_people <- merge(hes19_people, hes19_weights[, .(H_ID, Weight)], by = "H_ID")
hes19_households <- get_household_static(hes19_people)

# Merge income
hes19_hh_extra <- hes19_people[, .(
  H_Income_Disposable = sum(P_Income_Disposable),
  H_Income_SelfEmployed = sum(P_Income_SelfEmployed),
  H_Income_StudentAllowance = sum(P_Income_StudentAllowance),
  H_Income_WageSalary = sum(P_Income_WageSalary),
  H_Benefits_Accommodation_Abated = sum(P_Benefits_Accommodation_Abated),
  H_Benefits_JSS_Amount_Abated = sum(P_Benefits_JSS_Amount_Abated),
  H_Benefits_SLP_Amount_Abated = sum(P_Benefits_SLP_Amount_Abated),
  H_Benefits_SPS_Amount_Abated = sum(P_Benefits_SPS_Amount_Abated),
  H_Super_Amount_Gross = sum(P_Super_Amount_Gross),
  H_FamilyAssistance_FTC_Abated = sum(P_FamilyAssistance_FTC_Abated),
  H_FamilyAssistance_IWTC_Abated = sum(P_FamilyAssistance_IWTC_Abated),
  H_FamilyAssistance_BestStart = sum(P_FamilyAssistance_BestStart),
  H_Students = sum(P_Attributes_FullOrPartTimeEducation > 0),
  H_MaxAge = max(P_Attributes_Age)
), by = snz_hes_hhld_uid]
hes19_households <- merge(hes19_households, hes19_hh_extra, by = "snz_hes_hhld_uid")

# Merge on housing costs
hes19_housing_costs <- fread("data/HES19_housing_costs.csv")
hes19_households <- merge(hes19_households, hes19_housing_costs, by = "snz_hes_hhld_uid", all.x = TRUE)
hes19_households[
  , names(hes19_housing_costs) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
  .SDcols = names(hes19_housing_costs)
]
hes19_households[, ":="(
  H_HousingCosts_Mortgage = (
    H_HousingCosts_MortgageInterest +
      H_HousingCosts_MortgageInterest_AdditionalProp +
      H_HousingCosts_MortgagePrincipal +
      H_HousingCosts_MortgagePrincipal_AdditionalProp +
      H_HousingCosts_MortgageFees
  )
)]
hes19_households[H_Tenure_Rented == TRUE | H_HousingCosts_Rent > 0, H_Tenure_Type := "Paying rent"]
hes19_households[H_Tenure_Owned == TRUE & H_HousingCosts_Mortgage > 0, H_Tenure_Type := "Paying mortgage"]
hes19_households[H_Tenure_Owned == TRUE & H_HousingCosts_Mortgage == 0, H_Tenure_Type := "Owned outright"]
hes19_households[is.na(H_Tenure_Type) & H_HousingCosts_Rates_PrimaryProp > 0, H_Tenure_Type := "Owned outright"]

# Check there are not many NA's, drop them
logwarn(sprintf("Dropping %s households with unknown Tenure Type", hes19_households[is.na(H_Tenure_Type), .N]))
hes19_households <- hes19_households[!is.na(H_Tenure_Type)]

# Merge GST onto HES19
hes19_households <- merge(hes19_households, hes19_gst, by = "snz_hes_hhld_uid", all.x = TRUE)
hes19_households[
  , names(hes19_gst) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
  .SDcols = names(hes19_gst)
]

ggplot(hes19_households, aes(x = H_Expenditure_GST_Payable)) +
  geom_density() +
  facet_grid(H_Tenure_Type~.) +
  coord_cartesian(xlim = c(0, 10e3))

################################
household_dt <- rbindlist(
  list("HES18" = hes18_households, "HES19" = hes19_households),
  idcol = "Survey", fill = TRUE
)

# imputation_data <- household_dt[
#   , .SD, .SDcols = !c(
#     # "snz_hes_hhld_uid", "H_ID", "Weight",
#     # "Survey",
#     "H_Tenure_Type", "H_Tenure_Rented", "H_Expenditure_Total",
#     "H_HousingCosts_Unknown"
#   )
# ]
imputation_data <- household_dt[, .SD, .SDcols = c(
  # H_ID,
  # snz_hes_hhld_uid,
  # Weight,
  # Survey,
  # H_Expenditure_GST_Payable,
  # #
  # H_HousingCosts_Rent,
  # H_HousingCosts_Insurance,
  # H_Income_Disposable,
  # H_Income_SelfEmployed,
  # H_Income_WageSalary,
  # H_HousingCosts_MortgagePrincipal
  "Survey",
  "snz_hes_hhld_uid",
  "H_ID",
  "Weight",
  "H_Adults",
  "H_Children",
  "H_Families",
  # "H_People",
  "H_Adult_Earners",
  "H_Tenure_Owned",
  # "H_Tenure_Rented",
  # "Household_MOECD_Eq_Factor",
  "H_Income_Disposable",
  "H_Income_SelfEmployed",
  "H_Income_StudentAllowance",
  "H_Income_WageSalary",
  "H_Benefits_Accommodation_Abated",
  "H_Benefits_JSS_Amount_Abated",
  "H_Benefits_SLP_Amount_Abated",
  "H_Benefits_SPS_Amount_Abated",
  "H_Super_Amount_Gross",
  "H_FamilyAssistance_FTC_Abated",
  "H_FamilyAssistance_IWTC_Abated",
  "H_FamilyAssistance_BestStart",
  "H_Students",
  "H_MaxAge",
  # "H_AllOtherCosts",
  "H_HousingCosts_BodyCorp",
  "H_HousingCosts_Insurance",
  "H_HousingCosts_MortgageFees",
  "H_HousingCosts_MortgageInterest",
  "H_HousingCosts_MortgageInterest_AdditionalProp",
  "H_HousingCosts_MortgagePrincipal",
  "H_HousingCosts_MortgagePrincipal_AdditionalProp",
  "H_HousingCosts_Other",
  "H_HousingCosts_Rates_AdditionalProp",
  "H_HousingCosts_Rates_PrimaryProp",
  "H_HousingCosts_Rent",
  # "H_HousingCosts_RentAdditional",
  "H_HousingCosts_Rent_AdditionalProp",
  # "H_HousingCosts_Unknown",
  # "H_HousingCosts_Mortgage",
  # "H_Tenure_Type",
  # "H_Expenditure_Total",
  "H_Expenditure_GST_Payable"
)]
weight_data <- household_dt[, Weight]
first_imputation <- mice(imputation_data, maxit = 0)

predM <- first_imputation$predictorMatrix
predM[, c("snz_hes_hhld_uid")] <- 0
predM[, c("H_ID")] <- 0
predM[, c("Weight")] <- 0
predM[, c("Survey")] <- 0

meth <- first_imputation$method
# meth[meth == "pmm"] <- "weighted.pmm"

second_imputation <- mice(
  imputation_data, m = 4, maxit = 16,
  predictorMatrix = predM,
  method = meth,
  seed = 34567,
  imputationWeights = weight_data
)

# Check convergence
plot(second_imputation)

dt_imputed_expenditure <- setDT(complete(second_imputation, action = "long"))[
  # , .(Imputation = .imp, Survey, H_ID, H_Expenditure_GST_Payable)
]
dt_imputed_expenditure <- dt_imputed_expenditure[Survey == "HES18" | (Survey == "HES19" & .imp == 1)]
dt_imputed_expenditure[, Survey := factor(Survey, levels = c("HES19", "HES18"))]

dt_imputed_expenditure[, Zero_Exp := H_Expenditure_GST_Payable == 0]

dt_imputed_expenditure[H_Adults == 1 & H_Children == 0, HH_Type := "Single no children"]
dt_imputed_expenditure[H_Adults == 1 & H_Children > 0, HH_Type := "Sole parent"]
dt_imputed_expenditure[H_Adults == 2 & H_Children == 0, HH_Type := "Couple no children"]
dt_imputed_expenditure[H_Adults == 2 & H_Children > 0, HH_Type := "Couple parents"]
dt_imputed_expenditure[H_Adults > 2 & H_Children > 0, HH_Type := "Multi-family parents"]
dt_imputed_expenditure[H_Adults > 2 & H_Children == 0, HH_Type := "Multi-family no children"]

hh_type_levels <- c(
  "Single no children",
  "Sole parent",
  "Couple no children",
  "Couple parents",
  "Multi-family parents",
  "Multi-family no children"
)
dt_imputed_expenditure[
  , HH_Type := factor(HH_Type, levels = hh_type_levels)
]

dt_imputed_expenditure <- dt_imputed_expenditure[!is.na(HH_Type)]

dt_imputed_expenditure[, H_Has_Super := H_Super_Amount_Gross > 0]

age_bands <- c(seq(15, 75, 10), 120)
age_band_labels <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
dt_imputed_expenditure[, max_age_band := cut(H_MaxAge, age_bands, labels = age_band_labels, include.lowest = TRUE, right = FALSE)]

fwrite(dt_imputed_expenditure, "data/HES18_imputed_gst_full_dataset.csv.gz")

dt_imputed_expenditure[Survey == "HES18", Imputation_Weight := Weight / uniqueN(.imp)]
dt_imputed_expenditure[Survey == "HES19", Imputation_Weight := Weight]

# Overall density plot
ggplot(
  dt_imputed_expenditure[H_Expenditure_GST_Payable != 0],
  aes(x = H_Expenditure_GST_Payable, colour = interaction(Survey, factor(.imp)), weight = Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))

# Density plot by household type and Super
ggplot(
  dt_imputed_expenditure[Zero_Exp == FALSE],
  aes(x = H_Expenditure_GST_Payable, colour = interaction(Survey, factor(.imp)), weight = Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  facet_grid(H_Has_Super~HH_Type, scales = "free") +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))

# Density plot by Age and household type
ggplot(
  dt_imputed_expenditure[Zero_Exp == FALSE],
  aes(x = H_Expenditure_GST_Payable, colour = interaction(Survey, factor(.imp)), weight = Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  facet_grid(max_age_band~HH_Type, scales = "free") +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))


# Scatter plot by age and household type
ggplot(
  dt_imputed_expenditure[.imp == 1 & H_Expenditure_GST_Payable != 0],
  aes(x = H_Income_Disposable, y = H_Expenditure_GST_Payable, colour = Survey)
) + geom_point(alpha = 0.1) +
  facet_grid(max_age_band~HH_Type, scales = "free") +
  coord_cartesian(xlim = c(0, 200e3), ylim = c(0, 25e3)) +
  theme_minimal()

########### Combine imputations and average weights
# Overall density plot - imputations are combined with averaged weights
ggplot(
  dt_imputed_expenditure[H_Expenditure_GST_Payable != 0],
  aes(x = H_Expenditure_GST_Payable, colour = Survey, weight = Imputation_Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))

# Density plot by household type and Super
ggplot(
  dt_imputed_expenditure[Zero_Exp == FALSE],
  aes(x = H_Expenditure_GST_Payable, colour = Survey, weight = Imputation_Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  facet_grid(H_Has_Super~HH_Type, scales = "free") +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))

# Density plot by Age and household type
ggplot(
  dt_imputed_expenditure[Zero_Exp == FALSE],
  aes(x = H_Expenditure_GST_Payable, colour = Survey, weight = Imputation_Weight)
) +
  # geom_histogram(bins = 100, position = "dodge") +
  geom_density() +
  facet_grid(max_age_band~HH_Type, scales = "free") +
  scale_colour_manual(values = c("black", RColorBrewer::brewer.pal(5, "Dark2"))) +
  coord_cartesian(xlim = c(0, 50e3))


# Scatter plot by age and household type
ggplot(
  dt_imputed_expenditure[H_Expenditure_GST_Payable != 0],
  aes(x = H_Income_Disposable, y = H_Expenditure_GST_Payable, colour = Survey)
) + geom_point(alpha = 0.1) +
  facet_grid(max_age_band~HH_Type, scales = "free") +
  coord_cartesian(xlim = c(0, 200e3), ylim = c(0, 25e3)) +
  theme_minimal()

######### Output

dt_imputed_expenditure_out <- dt_imputed_expenditure[
  Survey == "HES18",
  .(Imputation = .imp, snz_hes_hhld_uid, H_Expenditure_GST_Payable)
]
fwrite(dt_imputed_expenditure_out, "data/HES18_imputed_gst.csv.gz")
