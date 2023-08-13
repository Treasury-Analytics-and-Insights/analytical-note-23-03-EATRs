get_etrs <- function(dt_people, weight_col, aggregate_by) {
  if (aggregate_by == "P_ID") {
    by_cols <- c("H_ID", "F_ID", "P_ID")
  } else if (aggregate_by == "F_ID") {
    by_cols <- c("H_ID", "F_ID")
  } else if (aggregate_by == "H_ID") {
    by_cols <- c("H_ID")
  } else {
    error_msg <- "Can only aggregate by P_ID, F_ID, or H_ID"
    logerror(error_msg)
    stop(error_msg)
  }
  
  # Use all available income and tax ETR columns
  income_cols <- names(dt_people) %>% .[. %like% "ETR_.*[0-9]_Income$"]
  tax_cols <- names(dt_people) %>% .[. %like% "ETR_.*[0-9]_Tax$"]
  di_cols <- names(dt_people) %>% .[. %like% "ETR_.*[0-9]_DI$"]
  
  # Use negative look-behind regex to find ETR numbers preceded by "_"
  etr_types <- income_cols %>% stringr::str_extract("(?<=_).*[0-9]")
  
  # # Have to manually set this at the moment!
  # etr_tax_components <- c(1, 2, 3, 4, 5, 6, 8)
  
  # Infer the tax components from the column names
  # regex: negative look-behind for "TaxComponent_"; then
  # extract the 1-digit or 2-digit number ending the string;
  # take unique components
  etr_tax_components <- names(dt_people) %>%
    .[. %like% "TaxComponent"] %>%
    stringr::str_extract("(?!<TaxComponent_)[0-9]{1,2}$") %>%
    unique()
  
  # Now we melt down the various ETRs into long format
  measure_cols <- list(
    "ETR_Income" = income_cols,
    "ETR_Tax" = tax_cols,
    "ETR_DI" = di_cols
  )
  for (ii in etr_tax_components) {
    tax_component_cols <- list(
      names(dt_people) %>% .[. %like% sprintf("ETR_.*[0-9]_TaxComponent_%s$", ii)]
    )
    names(tax_component_cols) <- sprintf("ETR_TaxComponent_%s", ii)
    measure_cols <- c(measure_cols, tax_component_cols)
  }
  data_cols <- unlist(measure_cols)
  
  dt_etr <- melt(
    dt_people[, .SD, .SDcols = c(by_cols, data_cols)],
    measure.vars = measure_cols, variable.name = "ETR_Type"
  )
  dt_etr[, ETR_Type := etr_types[ETR_Type]]
  
  # Aggregate to the desired unit
  dt_etr_agg <- dt_etr[
    , lapply(.SD, sum), .SDcols = c(
      "ETR_Income", "ETR_Tax", "ETR_DI",
      paste0("ETR_TaxComponent_", etr_tax_components)
    ),
    by = c("ETR_Type", by_cols)
  ]
  
  # Actual calculation of Effective Tax Rate.
  # If the numerator is zero, i.e. zero tax was paid, set to zero regardless
  # Otherwise, if the denominator is zero, the ETR is undefined, so set it to NA.
  dt_etr_agg[, ETR_Value := fifelse(
    # ETR_Income == 0, NA_real_, ETR_Tax / ETR_Income * 100
    ETR_Tax == 0, 0, fifelse(
      ETR_Income == 0, NA_real_, ETR_Tax / ETR_Income * 100
    )
  )]
  
  return(dt_etr_agg)
}