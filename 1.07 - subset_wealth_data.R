library(data.table)
library(magrittr)

# Extract required wealth data, apply capital gains
people_wealth_all <- fread("data/HES18_wealth_data.csv")

people_wealth <- people_wealth_all[, .(
  snz_hes_uid,
  Net_Worth, Assets, Liabilities,
  
  # Owner-occupied property - Assets only
  Owner_Occupied = (
    # Household
    Assets_PropertyOwnerOccupied +
    # Trust
    Assets_Trust_PropertyOwnerOccupied
  ),
  
  # Other residential property - Assets only
  Other_Residential = (
    # Household
    Assets_PropertyOtherResidential +
    # Trust
    Assets_Trust_PropertyOtherResidential
  ),
  
  # Non-residential property - Assets only
  Non_Residential = (
    # Household
    Assets_PropertyOtherNonResidential +
    # Trust
    Assets_Trust_PropertyOtherNonResidential
  ),
  
  # Vacant land - Assets only
  Vacant_Land = (
    Assets_PropertyLandOnly
  ),
  
  # Kiwisaver assets
  Kiwisaver_Assets = (
    Assets_PensionFunds_KiwiSaver
  ),
  
  # Other superannuation funds
  Other_Superannuation_Funds = (
    Assets_PensionFunds_SocialInsurance +
    Assets_PensionFunds_NonKiwiSaver
  ),
  
  # Investment funds
  Investment_Funds = (
    Assets_InvestmentFunds
  ),
  
  # Listed equities
  Listed_Equities = (
    # Household
    Assets_Shares_Listed +
    Assets_OtherFinancial +
    # Trust
    Assets_Trust_Financial
  ),
  
  # Unlisted equities
  Unlisted_Equities = (
    # Household
    Assets_Shares_UnListed +
    # Business
    (Assets_Business_UnListed_Property - Liabilities_Business_UnListed_Property) +
    (Assets_Business_UnListed_NonProperty - Liabilities_Business_UnListed_NonProperty)
  ),
  
  # Unincorporated business
  Unincorporated_Bus = (
    # Business
    (Assets_Business_UnIncorporated_Property - Liabilities_Business_UnIncorporated_Property) +
    (Assets_Business_UnIncorporated_NonProperty - Liabilities_Business_UnIncorporated_NonProperty) +
    # Trust
    (Assets_Trust_Business - Liabilities_Trust_Business)
  )
)]

fwrite(people_wealth, "data/people_wealth.csv")

people_wealth_long <- melt(
  people_wealth[, .SD, .SDcols = !c("Net_Worth", "Assets", "Liabilities")],
  id.vars = c("snz_hes_uid"),
  variable.name = "Asset_Class", value.name = "Value"
)
