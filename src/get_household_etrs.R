get_household_etrs <- function(dt_people, weight_col) {
  household_etrs <- get_etrs(dt_people, weight_col, aggregate_by = "H_ID")
  
  # Merge on weight, household equivalisation factor, household size
  dt_hh_static <- get_household_static(dt_people)
  household_etrs <- merge(household_etrs, dt_hh_static, by = "H_ID")
  
  # Household equivalised income
  household_etrs[, Eq_ETR_Income := ETR_Income / Household_MOECD_Eq_Factor]
  household_etrs[, Eq_ETR_Net_Income := (ETR_Income - ETR_Tax) / Household_MOECD_Eq_Factor]
  
  # Household weights
  household_etrs[, PeopleWeight := H_People*Weight]
  household_etrs[, Equivalised_People_Weight := Household_MOECD_Eq_Factor*Weight]
  
  # Household wealth
  hh_wealth <- dt_people[, .(Net_Worth = sum(Net_Worth)), by = H_ID]
  household_etrs <- merge(household_etrs, hh_wealth, by = "H_ID", all = TRUE)
  
  # Household types
  household_etrs[H_Adults == 1 & H_Children == 0, Household_Type := "Single no children"]
  household_etrs[H_Adults == 1 & H_Children > 0, Household_Type := "Sole parent"]
  household_etrs[H_Adults == 2 & H_Children == 0, Household_Type := "Couple no children"]
  household_etrs[H_Adults == 2 & H_Children > 0, Household_Type := "Couple parents"]
  
  # Super is a special case that overrides other household types
  household_etrs[H_Adults == 1 & H_Has_Super, Household_Type := "Single Super"]
  household_etrs[H_Adults == 2 & H_Has_Super, Household_Type := "Couple Super"]
  
  # Multi-family household is a special case that overrides other household types
  household_etrs[H_Families > 1, Household_Type := "Multi-family"]
  
  household_types <- c(
    "Single no children", "Sole parent", "Single Super",
    "Couple no children", "Couple parents", "Couple Super",
    "Multi-family"
  )
  household_etrs[, Household_Type := factor(Household_Type, levels = c(household_types))]
  
  # Family ages
  hh_ages <- dt_people[, .(H_Max_Age = max(P_Attributes_Age)), by = "H_ID"]
  household_etrs <- merge(household_etrs, hh_ages, by = "H_ID")
  age_bands <- c(seq(15, 75, 10), 120)
  household_etrs[, max_age_band := cut(H_Max_Age, age_bands, include.lowest = TRUE)]
  
  return(household_etrs)
}