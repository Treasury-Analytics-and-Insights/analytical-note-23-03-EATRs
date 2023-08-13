get_family_etrs <- function(dt_people, weight_col) {
  family_etrs <- get_etrs(dt_people, weight_col, aggregate_by = "F_ID")
  family_etrs <- add_family_constants(dt_people, family_etrs)
  
  # Family equivalised income
  family_etrs[, Eq_ETR_Income := ETR_Income / Family_MOECD_Eq_Factor]
  family_etrs[, Eq_ETR_Net_Income := (ETR_Income - ETR_Tax) / Family_MOECD_Eq_Factor]
  
  return(family_etrs)
}

add_family_constants <- function(dt_people, family_etrs) {
  # Merge on family equivalisation factor, family size
  dt_fam_static <- get_family_static(dt_people)
  family_etrs <- merge(family_etrs, dt_fam_static, by = c("F_ID", "H_ID"))
  
  # Merge on weight, household equivalisation factor, household size
  dt_hh_static <- get_household_static(dt_people)
  family_etrs <- merge(family_etrs, dt_hh_static, by = "H_ID")
  
  # Family weights
  family_etrs[, PeopleWeight := F_People*Weight]
  family_etrs[, AdultEquivalentsWeight := Family_MOECD_Eq_Factor*Weight]
  
  # Family types
  family_etrs[F_Adults == 1 & F_Children == 0, Family_Type := "Single no children"]
  family_etrs[F_Adults == 1 & F_Children > 0, Family_Type := "Sole parent"]
  family_etrs[F_Adults == 2 & F_Children == 0 & F_Adult_Earners == 0, Family_Type := "Couple no children, no emp"]
  family_etrs[F_Adults == 2 & F_Children == 0 & F_Adult_Earners == 1, Family_Type := "Couple no children, 1 emp"]
  family_etrs[F_Adults == 2 & F_Children == 0 & F_Adult_Earners == 2, Family_Type := "Couple no children, 2 emp"]
  family_etrs[F_Adults == 2 & F_Children > 0 & F_Adult_Earners == 0, Family_Type := "Couple parents, no emp"]
  family_etrs[F_Adults == 2 & F_Children > 0 & F_Adult_Earners == 1, Family_Type := "Couple parents, 1 emp"]
  family_etrs[F_Adults == 2 & F_Children > 0 & F_Adult_Earners == 2, Family_Type := "Couple parents, 2 emp"]
  
  # Super is a special case that overrides other family types
  family_etrs[F_Adults == 1 & F_Has_Super, Family_Type := "Single Super"]
  family_etrs[F_Adults == 2 & F_Has_Super & F_Adult_Earners == 0, Family_Type := "Couple Super, no emp"]
  family_etrs[F_Adults == 2 & F_Has_Super & F_Adult_Earners == 1, Family_Type := "Couple Super, 1 emp"]
  family_etrs[F_Adults == 2 & F_Has_Super & F_Adult_Earners == 2, Family_Type := "Couple Super, 2 emp"]
  
  
  family_types <- c(
    "Single no children", "Sole parent", "Single Super",
    # "Couple no children",
    "Couple no children, no emp",
    "Couple no children, 1 emp",
    "Couple no children, 2 emp",
    # "Couple parents",
    "Couple parents, no emp",
    "Couple parents, 1 emp",
    "Couple parents, 2 emp",
    # "Couple Super",
    "Couple Super, no emp",
    "Couple Super, 1 emp",
    "Couple Super, 2 emp"
  )
  family_etrs[, Family_Type := factor(Family_Type, levels = c(family_types))]
  
  family_etrs[, Family_Category := as.character(Family_Type)]
  family_etrs[
    Family_Type %like% "Single" | Family_Type %like% "Sole", Family_Category := "Single"
    ]
  family_etrs[Family_Type %like% "Couple", Family_Category := "Couple"]
  family_etrs[Family_Type %like% "Single Super", Family_Category := "Single Super"]
  family_etrs[Family_Type %like% "Couple Super", Family_Category := "Couple Super"]
  family_etrs[, Family_Category := factor(Family_Category, levels = c("Single", "Couple", "Single Super", "Couple Super"))]
  
  family_etrs[H_Families == 1, Multi_Family_Household := FALSE]
  family_etrs[H_Families > 1, Multi_Family_Household := TRUE]
  
  # Family ages
  fam_ages <- dt_people[, .(F_Max_Age = max(P_Attributes_Age)), by = "F_ID"]
  family_etrs <- merge(family_etrs, fam_ages, by = "F_ID")
  age_bands <- c(seq(15, 75, 10), 120)
  age_band_labels <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
  family_etrs[, max_age_band := cut(F_Max_Age, age_bands, labels = age_band_labels, include.lowest = TRUE, right = FALSE)]
  
  return(family_etrs)
}

add_household_constants <- function(dt_people, household_etrs) {
  
  # Merge on weight, household equivalisation factor, household size
  dt_hh_static <- get_household_static(dt_people)
  household_etrs <- merge(household_etrs, dt_hh_static, by = "H_ID")
  
  # Household weights
  household_etrs[, PeopleWeight := H_People*Weight]
  household_etrs[, AdultEquivalentsWeight := Household_MOECD_Eq_Factor*Weight]
  
  # # Household types
  # household_etrs[H_Adults == 1 & H_Children == 0, Household_Type := "Single no children"]
  # household_etrs[H_Adults == 1 & H_Children > 0, Household_Type := "Sole parent"]
  # household_etrs[H_Adults == 2 & H_Children == 0 & H_Adult_Earners == 0, Household_Type := "Couple no children, no emp"]
  # household_etrs[H_Adults == 2 & H_Children == 0 & H_Adult_Earners == 1, Household_Type := "Couple no children, 1 emp"]
  # household_etrs[H_Adults == 2 & H_Children == 0 & H_Adult_Earners == 2, Household_Type := "Couple no children, 2 emp"]
  # household_etrs[H_Adults == 2 & H_Children > 0 & H_Adult_Earners == 0, Household_Type := "Couple parents, no emp"]
  # household_etrs[H_Adults == 2 & H_Children > 0 & H_Adult_Earners == 1, Household_Type := "Couple parents, 1 emp"]
  # household_etrs[H_Adults == 2 & H_Children > 0 & H_Adult_Earners == 2, Household_Type := "Couple parents, 2 emp"]
  # 
  # # Super is a special case that overrides other household types
  # household_etrs[H_Adults == 1 & H_Has_Super, Household_Type := "Single Super"]
  # household_etrs[H_Adults == 2 & H_Has_Super & H_Adult_Earners == 0, Household_Type := "Couple Super, no emp"]
  # household_etrs[H_Adults == 2 & H_Has_Super & H_Adult_Earners == 1, Household_Type := "Couple Super, 1 emp"]
  # household_etrs[H_Adults == 2 & H_Has_Super & H_Adult_Earners == 2, Household_Type := "Couple Super, 2 emp"]
  
  
  # family_types <- c(
  #   "Single no children", "Sole parent", "Single Super",
  #   # "Couple no children",
  #   "Couple no children, no emp",
  #   "Couple no children, 1 emp",
  #   "Couple no children, 2 emp",
  #   # "Couple parents",
  #   "Couple parents, no emp",
  #   "Couple parents, 1 emp",
  #   "Couple parents, 2 emp",
  #   # "Couple Super",
  #   "Couple Super, no emp",
  #   "Couple Super, 1 emp",
  #   "Couple Super, 2 emp"
  # )
  # household_etrs[, Family_Type := factor(Family_Type, levels = c(family_types))]
  # 
  # household_etrs[, Family_Category := as.character(Family_Type)]
  # household_etrs[
  #   Family_Type %like% "Single" | Family_Type %like% "Sole", Family_Category := "Single"
  #   ]
  # household_etrs[Family_Type %like% "Couple", Family_Category := "Couple"]
  # household_etrs[Family_Type %like% "Single Super", Family_Category := "Single Super"]
  # household_etrs[Family_Type %like% "Couple Super", Family_Category := "Couple Super"]
  # household_etrs[, Family_Category := factor(Family_Category, levels = c("Single", "Couple", "Single Super", "Couple Super"))]
  
  household_etrs[H_Families == 1, Multi_Family_Household := FALSE]
  household_etrs[H_Families > 1, Multi_Family_Household := TRUE]
  
  # Family ages
  hh_ages <- dt_people[, .(H_Max_Age = max(P_Attributes_Age)), by = "H_ID"]
  household_etrs <- merge(household_etrs, hh_ages, by = "H_ID")
  age_bands <- c(seq(15, 75, 10), 120)
  age_band_labels <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
  household_etrs[, max_age_band := cut(H_Max_Age, age_bands, labels = age_band_labels, include.lowest = TRUE, right = FALSE)]
  
  return(household_etrs)
}