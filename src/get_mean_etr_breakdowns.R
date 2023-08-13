get_mean_etr_breakdowns <- function(family_etrs, weight_col, extra_group_by) {
  tax_cols <- c(
    "ETR_Tax",
    names(family_etrs) %>% .[. %like% "ETR_TaxComponent_[1-9]{1,2}"]
  )
  id_cols <- c("ETR_Type", weight_col, extra_group_by, "ETR_Income")
  
  family_etrs_breakdown_data <- family_etrs[
    , .SD, .SDcols = c(id_cols, tax_cols)
  ]
  family_etrs_breakdown_data[
    , (tax_cols) := lapply(.SD, as.double), .SDcols = tax_cols
  ]
  
  family_etrs_breakdown_data_long <- melt(
    family_etrs_breakdown_data,
    id.vars = id_cols,
    value.name = "ETR_TaxComponent", variable.name = "Variable"
  )
  
  family_etrs_breakdown_long <- family_etrs_breakdown_data_long[
    , .(
      Value = sum(
        get(weight_col)*ETR_TaxComponent/ETR_Income, na.rm = TRUE
      ) / sum(get(weight_col)) * 100,
      Population = sum(get(weight_col)),
      Sample = .N
    ),
    by = c("ETR_Type", extra_group_by, "Variable")
  ]
  
  return(family_etrs_breakdown_long)
}