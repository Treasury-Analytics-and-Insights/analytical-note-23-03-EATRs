# E.g.
# xaxis = quantile_col,
# xfacet = Family_Category,
# subgroups = c(has_children, adult_earners)
# extra_groups = Family_Type
get_etrs_by_group <- function(
  dt_etrs, xaxis, xfacet, subgroups, extra_groups, weight_col
) {
  groups <- c(xfacet, subgroups, extra_groups)
  
  average_family_etrs_by_group <- get_average_etrs(
    dt_etrs,
    weight_col = etr_weight_col,
    extra_group_by = c(xaxis, groups),
    calc_box_and_whiskers = TRUE
  )
  
  # All F_Has_Children, All F_Adult_Earners, All Family_Type
  average_family_etrs_by_xfacet <- get_average_etrs(
    dt_etrs,
    weight_col = etr_weight_col,
    extra_group_by = c(xaxis, xfacet),
    calc_box_and_whiskers = TRUE
  )
  average_family_etrs_by_xfacet[, setdiff(groups, xfacet) := "All"]
  
  # All Family_Category, All Family_Type
  average_family_etrs_by_subgroups <- get_average_etrs(
    dt_etrs,
    weight_col = etr_weight_col,
    extra_group_by = c(xaxis, subgroups),
    calc_box_and_whiskers = TRUE
  )
  average_family_etrs_by_subgroups[, setdiff(groups, subgroups) := "All"]
  
  # All everything
  average_family_etrs_by_xaxis <- get_average_etrs(
    dt_etrs,
    weight_col = etr_weight_col,
    extra_group_by = xaxis,
    calc_box_and_whiskers = TRUE
  )
  average_family_etrs_by_xaxis[, (groups) := "All"]
  
  average_family_etrs_by_income_and_family_type <- rbindlist(list(
    average_family_etrs_by_group,
    average_family_etrs_by_xfacet,
    average_family_etrs_by_subgroups,
    average_family_etrs_by_xaxis
  ), use.names = TRUE)
  
  average_family_etrs_by_income_and_family_type[, Rounding_Rule := "EATR_Percentage"]
  
  return(average_family_etrs_by_income_and_family_type)
}
