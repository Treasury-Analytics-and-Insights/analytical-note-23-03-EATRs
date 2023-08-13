add_etr_income_cols <- function(dt_people) {
  
  short_run_inflation <- 1.1/100
  long_run_inflation <- 1.73/100
  
  # Get household equivalisation factors
  dt_hh_static <- get_household_static(dt_people)
  dt_people <- merge(
    dt_people,
    dt_hh_static[, .(H_ID, Household_MOECD_Eq_Factor)],
    by = "H_ID"
  )
  
  dt_people[, ":="(
    P_Transfers = (
      P_Benefits_All_Taxable + P_Benefits_All_NonTaxable +
        P_FamilyAssistance_Total
    ),
    P_Transfers_NonTaxable = (
      P_Benefits_All_NonTaxable + P_FamilyAssistance_Total
    ),
    # Note that Gross PIE (income - tax) == Net PIE (income - tax)
    P_Tax_PIE_Gross = P_Tax_PIE_Net + P_TaxCredits_PIE,
    P_Income_PIE_Gross = P_Income_PIE_Net + P_TaxCredits_PIE,
    P_Income_Taxable_ACC = P_Income_Taxable - P_Income_Taxable_no_ACC_income
  )]
  
  # Note: PIE income is not included in TAWA's P_Income_Taxable variable
  # and PIE tax is not included in TAWA's P_Income_TaxPayable
  
  # ETR 1 - personal taxable income (including taxable transfers); income tax
  dt_people[, ":="(
    ETR_1_Tax = P_Income_TaxPayable_no_ACC_income,
    ETR_1_Income = P_Income_Taxable_no_ACC_income
  )]
  dt_people[, ":="(
    ETR_1_DI = ETR_1_Income - ETR_1_Tax,
    ETR_1_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_1_TaxComponent_2 = NA,
    ETR_1_TaxComponent_3 = NA,
    ETR_1_TaxComponent_4 = NA,
    ETR_1_TaxComponent_5 = NA,
    ETR_1_TaxComponent_6 = NA,
    ETR_1_TaxComponent_9 = NA
  )]
  
  # ETR 2 - transfers as negative tax
  # Also add non-taxable transfers to the income base, for symmetry with numerator
  dt_people[, ":="(
    ETR_2_Tax = ETR_1_Tax - P_Transfers,
    ETR_2_Income = ETR_1_Income + P_Transfers_NonTaxable
  )]
  dt_people[, ":="(
    ETR_2_DI = ETR_2_Income - (ETR_2_Tax + P_Transfers),
    ETR_2_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_2_TaxComponent_2 = -1L*P_Transfers,
    ETR_2_TaxComponent_3 = NA,
    ETR_2_TaxComponent_4 = NA,
    ETR_2_TaxComponent_5 = NA,
    ETR_2_TaxComponent_6 = NA,
    ETR_2_TaxComponent_9 = NA
  )]
  
  # ETR 3 - PIE income and PIE tax
  dt_people[, ":="(
    ETR_3_Tax = ETR_2_Tax + P_Tax_PIE_Gross,
    ETR_3_Income = ETR_2_Income + P_Income_PIE_Gross
  )]
  dt_people[, ":="(
    ETR_3_DI = ETR_3_Income - (ETR_3_Tax + P_Transfers),
    ETR_3_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_3_TaxComponent_2 = -1L*P_Transfers,
    ETR_3_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_3_TaxComponent_4 = NA,
    ETR_3_TaxComponent_5 = NA,
    ETR_3_TaxComponent_6 = NA,
    ETR_3_TaxComponent_9 = NA
  )]
  
  # ETR 4 - ACC income and ACC Levy
  dt_people[, ":="(
    ETR_4_Tax = ETR_3_Tax + P_ACC_LevyPayable,
    ETR_4_Income = ETR_3_Income + P_Income_Taxable_ACC
  )]
  dt_people[, ":="(
    ETR_4_DI = ETR_4_Income - (ETR_4_Tax + P_Transfers),
    ETR_4_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_4_TaxComponent_2 = -1L*P_Transfers,
    ETR_4_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_4_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_4_TaxComponent_5 = NA,
    ETR_4_TaxComponent_6 = NA,
    ETR_4_TaxComponent_9 = NA
  )]
  
  # ETR 5 - Capital gains on investments, rates on investment property
  # variant 1: short-run capital gain
  dt_people[, ":="(
    ETR_51_Tax = ETR_4_Tax + P_HousingCosts_Rates_AdditionalProp,
    ETR_51_Income = ETR_4_Income + Short_Run_Investment_Gain
  )]
  dt_people[, ":="(
    ETR_51_DI = ETR_51_Income - (ETR_51_Tax + P_Transfers),
    ETR_51_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_51_TaxComponent_2 = -1L*P_Transfers,
    ETR_51_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_51_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_51_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_51_TaxComponent_6 = NA,
    ETR_51_TaxComponent_9 = NA
  )]
  # ETR 5 - Capital gains on investments, rates on investment property
  # variant 2: long-run capital gain
  dt_people[, ":="(
    ETR_52_Tax = ETR_4_Tax + P_HousingCosts_Rates_AdditionalProp,
    ETR_52_Income = ETR_4_Income + Long_Run_Investment_Gain
  )]
  dt_people[, ":="(
    ETR_52_DI = ETR_52_Income - (ETR_52_Tax + P_Transfers),
    ETR_52_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_52_TaxComponent_2 = -1L*P_Transfers,
    ETR_52_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_52_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_52_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_52_TaxComponent_6 = NA,
    ETR_52_TaxComponent_9 = NA
  )]
  
  # ETR 5 - Capital gains on investments, rates on investment property
  # variant 3: short-run capital gain with short-run inflation adjustment
  dt_people[, ":="(
    ETR_53_Tax = ETR_51_Tax,
    ETR_53_Income = ETR_51_Income - short_run_inflation*Investment_Value
  )]
  dt_people[, ":="(
    ETR_53_DI = ETR_53_Income - (ETR_53_Tax + P_Transfers),
    ETR_53_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_53_TaxComponent_2 = -1L*P_Transfers,
    ETR_53_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_53_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_53_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_53_TaxComponent_6 = NA,
    ETR_53_TaxComponent_9 = NA
  )]
  # ETR 5 - Capital gains on investments, rates on investment property
  # variant 4: long-run capital gain with long-run inflation adjustment
  dt_people[, ":="(
    ETR_54_Tax = ETR_52_Tax,
    ETR_54_Income = ETR_52_Income - long_run_inflation*Investment_Value
  )]
  dt_people[, ":="(
    ETR_54_DI = ETR_54_Income - (ETR_54_Tax + P_Transfers),
    ETR_54_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_54_TaxComponent_2 = -1L*P_Transfers,
    ETR_54_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_54_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_54_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_54_TaxComponent_6 = NA,
    ETR_54_TaxComponent_9 = NA
  )]
  
  # ETR 6 - Capital gains on investments & owner-occupied housing, rates on all property
  # variant 1: short-run capital gain
  dt_people[, ":="(
    ETR_61_Tax = ETR_51_Tax + P_HousingCosts_Rates_PrimaryProp,
    ETR_61_Income = ETR_51_Income + Short_Run_Owner_Occupied_Gain
  )]
  dt_people[, ":="(
    ETR_61_DI = ETR_61_Income - (ETR_61_Tax + P_Transfers),
    ETR_61_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_61_TaxComponent_2 = -1L*P_Transfers,
    ETR_61_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_61_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_61_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_61_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_61_TaxComponent_9 = NA
  )]
  
  # ETR 6 - Capital gains on investments & owner-occupied housing, rates on all property
  # variant 2: long-run capital gain
  dt_people[, ":="(
    ETR_62_Tax = ETR_52_Tax + P_HousingCosts_Rates_PrimaryProp,
    ETR_62_Income = ETR_52_Income + Long_Run_Owner_Occupied_Gain
  )]
  dt_people[, ":="(
    ETR_62_DI = ETR_62_Income - (ETR_62_Tax + P_Transfers),
    ETR_62_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_62_TaxComponent_2 = -1L*P_Transfers,
    ETR_62_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_62_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_62_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_62_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_62_TaxComponent_9 = NA
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # Use short-run capital gains from ETR 61
  # variant 1: average rent in the area
  dt_people[, ":="(
    ETR_71_Tax = ETR_61_Tax,
    ETR_71_Income = ETR_61_Income + P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
  )]
  dt_people[, ":="(
    ETR_71_DI = ETR_71_Income - (ETR_71_Tax + P_Transfers),
    ETR_71_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_71_TaxComponent_2 = -1L*P_Transfers,
    ETR_71_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_71_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_71_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_71_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_71_TaxComponent_9 = NA
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 2: average rent in the area, long-run capital gains from ETR 62
  dt_people[, ":="(
    ETR_72_Tax = ETR_62_Tax,
    ETR_72_Income = ETR_62_Income + P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
  )]
  dt_people[, ":="(
    ETR_72_DI = ETR_72_Income - (ETR_72_Tax + P_Transfers),
    ETR_72_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_72_TaxComponent_2 = -1L*P_Transfers,
    ETR_72_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_72_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_72_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_72_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_72_TaxComponent_9 = NA
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 3: average yield in the area, short-run capital gains from ETR 61
  dt_people[, ":="(
    ETR_73_Tax = ETR_61_Tax,
    ETR_73_Income = ETR_61_Income + P_AverageYield_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
  )]
  dt_people[, ":="(
    ETR_73_DI = ETR_73_Income - (ETR_73_Tax + P_Transfers),
    ETR_73_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_73_TaxComponent_2 = -1L*P_Transfers,
    ETR_73_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_73_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_73_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_73_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_73_TaxComponent_9 = NA
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 4: average yield in the area, long-run capital gains from ETR 62
  dt_people[, ":="(
    ETR_74_Tax = ETR_62_Tax,
    ETR_74_Income = ETR_62_Income + P_AverageYield_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
  )]
  dt_people[, ":="(
    ETR_74_DI = ETR_74_Income - (ETR_74_Tax + P_Transfers),
    ETR_74_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
    ETR_74_TaxComponent_2 = -1L*P_Transfers,
    ETR_74_TaxComponent_3 = P_Tax_PIE_Gross,
    ETR_74_TaxComponent_4 = P_ACC_LevyPayable,
    ETR_74_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
    ETR_74_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
    ETR_74_TaxComponent_9 = NA
  )]
  
  
  #### ETR 7 - Without Transfers ####
  
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # Use short-run capital gains from ETR 61
  # variant 5: average rent in the area
  dt_people[, ":="(
    ETR_75_Tax = ETR_71_Tax + P_Transfers,
    ETR_75_Income = ETR_71_Income
  )]
  dt_people[, ":="(
    ETR_75_DI = ETR_71_DI,
    ETR_75_TaxComponent_1 = ETR_71_TaxComponent_1,
    ETR_75_TaxComponent_2 = 0,
    ETR_75_TaxComponent_3 = ETR_71_TaxComponent_3,
    ETR_75_TaxComponent_4 = ETR_71_TaxComponent_4,
    ETR_75_TaxComponent_5 = ETR_71_TaxComponent_5,
    ETR_75_TaxComponent_6 = ETR_71_TaxComponent_6,
    ETR_75_TaxComponent_9 = ETR_71_TaxComponent_9
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 6: average rent in the area, long-run capital gains from ETR 62
  dt_people[, ":="(
    ETR_76_Tax = ETR_72_Tax + P_Transfers,
    ETR_76_Income = ETR_72_Income
  )]
  dt_people[, ":="(
    ETR_76_DI = ETR_71_DI,
    ETR_76_TaxComponent_1 = ETR_72_TaxComponent_1,
    ETR_76_TaxComponent_2 = 0,
    ETR_76_TaxComponent_3 = ETR_72_TaxComponent_3,
    ETR_76_TaxComponent_4 = ETR_72_TaxComponent_4,
    ETR_76_TaxComponent_5 = ETR_72_TaxComponent_5,
    ETR_76_TaxComponent_6 = ETR_72_TaxComponent_6,
    ETR_76_TaxComponent_9 = ETR_72_TaxComponent_9
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 7: average yield in the area, short-run capital gains from ETR 61
  dt_people[, ":="(
    ETR_77_Tax = ETR_73_Tax + P_Transfers,
    ETR_77_Income = ETR_73_Income
  )]
  dt_people[, ":="(
    ETR_77_DI = ETR_71_DI,
    ETR_77_TaxComponent_1 = ETR_73_TaxComponent_1,
    ETR_77_TaxComponent_2 = 0,
    ETR_77_TaxComponent_3 = ETR_73_TaxComponent_3,
    ETR_77_TaxComponent_4 = ETR_73_TaxComponent_4,
    ETR_77_TaxComponent_5 = ETR_73_TaxComponent_5,
    ETR_77_TaxComponent_6 = ETR_73_TaxComponent_6,
    ETR_77_TaxComponent_9 = ETR_73_TaxComponent_9
  )]
  
  # ETR 7 - Imputed rent on owner-occupied housing
  # variant 8: average yield in the area, long-run capital gains from ETR 62
  dt_people[, ":="(
    ETR_78_Tax = ETR_74_Tax + P_Transfers,
    ETR_78_Income = ETR_74_Income
  )]
  dt_people[, ":="(
    ETR_78_DI = ETR_71_DI,
    ETR_78_TaxComponent_1 = ETR_74_TaxComponent_1,
    ETR_78_TaxComponent_2 = 0,
    ETR_78_TaxComponent_3 = ETR_74_TaxComponent_3,
    ETR_78_TaxComponent_4 = ETR_74_TaxComponent_4,
    ETR_78_TaxComponent_5 = ETR_74_TaxComponent_5,
    ETR_78_TaxComponent_6 = ETR_74_TaxComponent_6,
    ETR_78_TaxComponent_9 = ETR_74_TaxComponent_9
  )]
  
  
  # ETR 9 - GST (imputed from HES19 onto HES18)
  # Long or short -run capital gains
  # Average rent
  # without / with inflation adjustment to net worth
  if ("P_Expenditure_GST_Payable" %in% names(dt_people)) {
    # Short-run capital gains on investment and owner-occupied property
    dt_people[, ":="(
      ETR_91_Tax = ETR_4_Tax +
        P_HousingCosts_Rates_AdditionalProp +
        P_HousingCosts_Rates_PrimaryProp +
        P_Expenditure_GST_Payable,
      ETR_91_Income = ETR_4_Income +
        Short_Run_Investment_Gain +
        Short_Run_Owner_Occupied_Gain +
        P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
    )]
    dt_people[, ":="(
      ETR_91_DI = ETR_91_Income - (ETR_91_Tax + P_Transfers),
      ETR_91_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
      ETR_91_TaxComponent_2 = -1L*P_Transfers,
      ETR_91_TaxComponent_3 = P_Tax_PIE_Gross,
      ETR_91_TaxComponent_4 = P_ACC_LevyPayable,
      ETR_91_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
      ETR_91_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
      ETR_91_TaxComponent_9 = P_Expenditure_GST_Payable
    )]
    
    # Long-run capital gains on investment and owner-occupied property
    dt_people[, ":="(
      ETR_92_Tax = ETR_4_Tax +
        P_HousingCosts_Rates_AdditionalProp +
        P_HousingCosts_Rates_PrimaryProp +
        P_Expenditure_GST_Payable,
      ETR_92_Income = ETR_4_Income +
        Long_Run_Investment_Gain +
        Long_Run_Owner_Occupied_Gain +
        P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
    )]
    dt_people[, ":="(
      ETR_92_DI = ETR_92_Income - (ETR_92_Tax + P_Transfers),
      ETR_92_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
      ETR_92_TaxComponent_2 = -1L*P_Transfers,
      ETR_92_TaxComponent_3 = P_Tax_PIE_Gross,
      ETR_92_TaxComponent_4 = P_ACC_LevyPayable,
      ETR_92_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
      ETR_92_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
      ETR_92_TaxComponent_9 = P_Expenditure_GST_Payable
    )]
    
    # Short-run capital gains, short-run inflation adjustment to net worth
    dt_people[, ":="(
      ETR_93_Tax = ETR_4_Tax +
        P_HousingCosts_Rates_AdditionalProp +
        P_HousingCosts_Rates_PrimaryProp +
        P_Expenditure_GST_Payable,
      ETR_93_Income = ETR_4_Income +
        Short_Run_Investment_Gain - short_run_inflation*Net_Worth +
        Short_Run_Owner_Occupied_Gain +
        P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
    )]
    dt_people[, ":="(
      ETR_93_DI = ETR_93_Income - (ETR_93_Tax + P_Transfers),
      ETR_93_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
      ETR_93_TaxComponent_2 = -1L*P_Transfers,
      ETR_93_TaxComponent_3 = P_Tax_PIE_Gross,
      ETR_93_TaxComponent_4 = P_ACC_LevyPayable,
      ETR_93_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
      ETR_93_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
      ETR_93_TaxComponent_9 = P_Expenditure_GST_Payable
    )]
    
    # Long-run capital gains, long-run inflation adjustment to net worth
    dt_people[, ":="(
      ETR_94_Tax = ETR_4_Tax +
        P_HousingCosts_Rates_AdditionalProp +
        P_HousingCosts_Rates_PrimaryProp +
        P_Expenditure_GST_Payable,
      ETR_94_Income = ETR_4_Income +
        Long_Run_Investment_Gain - long_run_inflation*Net_Worth +
        Long_Run_Owner_Occupied_Gain +
        P_AverageRent_Imputed_Rent*52 - Imputed_Rent_Housing_Costs
    )]
    dt_people[, ":="(
      ETR_94_DI = ETR_94_Income - (ETR_94_Tax + P_Transfers),
      ETR_94_TaxComponent_1 = P_Income_TaxPayable_no_ACC_income,
      ETR_94_TaxComponent_2 = -1L*P_Transfers,
      ETR_94_TaxComponent_3 = P_Tax_PIE_Gross,
      ETR_94_TaxComponent_4 = P_ACC_LevyPayable,
      ETR_94_TaxComponent_5 = P_HousingCosts_Rates_AdditionalProp,
      ETR_94_TaxComponent_6 = P_HousingCosts_Rates_PrimaryProp,
      ETR_94_TaxComponent_9 = P_Expenditure_GST_Payable
    )]
  }
  
  # ETR 9 - variants without Transfers
  # Long or short -run capital gains
  # Average rent
  # without / with inflation adjustment to net worth
  if ("P_Expenditure_GST_Payable" %in% names(dt_people)) {
    # Short-run capital gains on investment and owner-occupied property
    dt_people[, ":="(
      ETR_95_Tax = ETR_91_Tax + P_Transfers,
      ETR_95_Income = ETR_91_Income
    )]
    dt_people[, ":="(
      ETR_95_DI = ETR_91_DI,
      ETR_95_TaxComponent_1 = ETR_91_TaxComponent_1,
      ETR_95_TaxComponent_2 = 0,
      ETR_95_TaxComponent_3 = ETR_91_TaxComponent_3,
      ETR_95_TaxComponent_4 = ETR_91_TaxComponent_4,
      ETR_95_TaxComponent_5 = ETR_91_TaxComponent_5,
      ETR_95_TaxComponent_6 = ETR_91_TaxComponent_6,
      ETR_95_TaxComponent_9 = ETR_91_TaxComponent_9
    )]
    
    # Long-run capital gains on investment and owner-occupied property
    dt_people[, ":="(
      ETR_96_Tax = ETR_92_Tax + P_Transfers,
      ETR_96_Income = ETR_92_Income
    )]
    dt_people[, ":="(
      ETR_96_DI = ETR_92_DI,
      ETR_96_TaxComponent_1 = ETR_92_TaxComponent_1,
      ETR_96_TaxComponent_2 = 0,
      ETR_96_TaxComponent_3 = ETR_92_TaxComponent_3,
      ETR_96_TaxComponent_4 = ETR_92_TaxComponent_4,
      ETR_96_TaxComponent_5 = ETR_92_TaxComponent_5,
      ETR_96_TaxComponent_6 = ETR_92_TaxComponent_6,
      ETR_96_TaxComponent_9 = ETR_92_TaxComponent_9
    )]
    
    # Short-run capital gains, short-run inflation adjustment to net worth
    dt_people[, ":="(
      ETR_97_Tax = ETR_93_Tax + P_Transfers,
      ETR_97_Income = ETR_93_Income
    )]
    dt_people[, ":="(
      ETR_97_DI = ETR_93_DI,
      ETR_97_TaxComponent_1 = ETR_93_TaxComponent_1,
      ETR_97_TaxComponent_2 = 0,
      ETR_97_TaxComponent_3 = ETR_93_TaxComponent_3,
      ETR_97_TaxComponent_4 = ETR_93_TaxComponent_4,
      ETR_97_TaxComponent_5 = ETR_93_TaxComponent_5,
      ETR_97_TaxComponent_6 = ETR_93_TaxComponent_6,
      ETR_97_TaxComponent_9 = ETR_93_TaxComponent_9
    )]
    
    # Long-run capital gains, long-run inflation adjustment to net worth
    dt_people[, ":="(
      ETR_98_Tax = ETR_94_Tax + P_Transfers,
      ETR_98_Income = ETR_94_Income
    )]
    dt_people[, ":="(
      ETR_98_DI = ETR_94_DI,
      ETR_98_TaxComponent_1 = ETR_94_TaxComponent_1,
      ETR_98_TaxComponent_2 = 0,
      ETR_98_TaxComponent_3 = ETR_94_TaxComponent_3,
      ETR_98_TaxComponent_4 = ETR_94_TaxComponent_4,
      ETR_98_TaxComponent_5 = ETR_94_TaxComponent_5,
      ETR_98_TaxComponent_6 = ETR_94_TaxComponent_6,
      ETR_98_TaxComponent_9 = ETR_94_TaxComponent_9
    )]
  }
  
  # Cast all ETR columns to double so that melts do not complain
  etr_cols <- names(dt_people) %>% .[. %like% "ETR_"]
  dt_people[, (etr_cols) := lapply(.SD, as.double), .SDcols = etr_cols]
  
  return(invisible(dt_people))
}