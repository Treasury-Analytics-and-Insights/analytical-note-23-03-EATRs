get_family_static <- function(dt_people) {
  dt_fam <- dt_people[, .(
    F_Has_Super = any(P_Super_Amount_Gross > 0),
    F_Has_Children = any(P_Attributes_Dependent == TRUE),
    F_Has_Students = any(P_Attributes_FullOrPartTimeEducation > 0),
    F_Has_StudentAllowance = any(P_Income_StudentAllowance > 0),
    F_Has_SLP = any(P_Benefits_SLP_Amount_Abated > 0),
    F_Has_JSS = any(P_Benefits_JSS_Amount_Abated > 0),
    F_Has_CoreBenefit = any(P_Benefits_CoreBenefits_AbatedAmount > 0),
    F_People_GTE_14 = sum(P_Attributes_Age >= 14),
    F_People_LT_14 = sum(P_Attributes_Age < 14),
    F_Adults = sum(P_Attributes_Dependent == FALSE),
    F_Children = sum(P_Attributes_Dependent == TRUE),
    F_People = .N,
    F_Adult_Earners = sum((P_Attributes_Dependent == FALSE) & (P_Income_WageSalary != 0 | P_Income_SelfEmployed != 0)),
    #
    F_DI = sum(P_Income_Disposable),
    F_TI = sum(P_Income_Taxable),
    F_PrivateTaxable = sum(P_Income_PrivateTaxable),
    #
    F_posttaxincome = sum(
      P_Income_Taxable - P_Income_TaxPayable - P_ACC_LevyPayable +
        (
          P_Benefits_All_NonTaxable + P_FamilyAssistance_Total + P_TaxCredit_IETC
        )
    ),
    F_nontaxable = sum(P_Income_NonTaxable),
    F_privatenontaxable = sum(P_Income_NonTaxable - P_Benefits_All_NonTaxable),
    F_benefitsnontaxable = sum(P_Benefits_All_NonTaxable)
    #
  ), by = .(F_ID, H_ID)]
  dt_fam[, ":="(
    Family_MOECD_Eq_Factor = 1 + 0.5 * (F_People_GTE_14 - 1) + 0.3 * F_People_LT_14,
    F_People_GTE_14 = NULL,
    F_People_LT_14 = NULL
  )]
  
  return(dt_fam)
}

get_family_static_wealth_extra <- function(dt_people, dt_fam) {
  dt_fam_static_wealth_extra <- dt_people[, .(
    # Wealth
    F_Net_Worth = sum(Net_Worth),
    F_Assets = sum(Assets),
    F_Liabilities = sum(Liabilities),
    F_Owner_Occupied = sum(Owner_Occupied),
    F_Investment_Property = sum(Other_Residential + Non_Residential + Vacant_Land), # extra helper
    F_Other_Residential = sum(Other_Residential),
    F_Non_Residential = sum(Non_Residential),
    F_Vacant_Land = sum(Vacant_Land),
    F_Kiwisaver_Assets = sum(Kiwisaver_Assets),
    F_Other_Superannuation_Funds = sum(Other_Superannuation_Funds),
    F_Investment_Funds = sum(Investment_Funds),
    F_Listed_Equities = sum(Listed_Equities),
    F_Unlisted_Equities = sum(Unlisted_Equities),
    F_Unincorporated_Bus = sum(Unincorporated_Bus),
    # Rates - household level
    H_HousingCosts_Rates_AdditionalProp = H_HousingCosts_Rates_AdditionalProp[1],
    H_HousingCosts_Rates_PrimaryProp = H_HousingCosts_Rates_PrimaryProp[1],
    F_HousingCosts_Rates_AdditionalProp = sum(P_HousingCosts_Rates_AdditionalProp),
    F_HousingCosts_Rates_PrimaryProp = sum(P_HousingCosts_Rates_PrimaryProp),
    # Imputed Rents - household level
    H_AverageRent_Imputed_Rent = H_AverageRent_Imputed_Rent[1],
    H_AverageYield_Imputed_Rent = H_AverageYield_Imputed_Rent[1]
  ), by = .(F_ID, H_ID)]
  dt_fam <- merge(
    dt_fam, dt_fam_static_wealth_extra, by = c("F_ID", "H_ID"), all = TRUE
  )
  dt_fam[, ":="(
    Eq_Net_Worth = F_Net_Worth / Family_MOECD_Eq_Factor
  )]
  return(dt_fam)
}

get_household_static <- function(dt_people, weight_col = "Weight") {
  dt_hh <- dt_people[, .(
    Weight = first(get(weight_col)),
    H_Has_Super = any(P_Super_Amount_Gross > 0),
    H_Has_Students = any(P_Attributes_FullOrPartTimeEducation > 0),
    H_Has_StudentAllowance = any(P_Income_StudentAllowance > 0),
    H_Has_SLP = any(P_Benefits_SLP_Amount_Abated > 0),
    H_Has_JSS = any(P_Benefits_JSS_Amount_Abated > 0),
    H_Has_CoreBenefit = any(P_Benefits_CoreBenefits_AbatedAmount > 0),
    H_People_GTE_14 = sum(P_Attributes_Age >= 14),
    H_People_LT_14 = sum(P_Attributes_Age < 14),
    H_Adults = sum(P_Attributes_Dependent == FALSE),
    H_Children = sum(P_Attributes_Dependent == TRUE),
    H_Families = max(F_ID) - min(F_ID) + 1, # much faster than uniqueN(F_ID)
    H_People = .N,
    H_Adult_Earners = sum((P_Attributes_Dependent == FALSE) & (P_Income_PrivateTaxable > 0)),
    H_Tenure_Owned = H_Tenure_Owned[1],
    H_Tenure_Rented = H_Tenure_Rented[1],
    H_DI = sum(P_Income_Disposable)
  ), by = .(H_ID, snz_hes_hhld_uid)]
  dt_hh[, ":="(
    Household_MOECD_Eq_Factor = 1 + 0.5 * (H_People_GTE_14 - 1) + 0.3 * H_People_LT_14,
    H_People_GTE_14 = NULL,
    H_People_LT_14 = NULL
  )]
  return(dt_hh)
}
