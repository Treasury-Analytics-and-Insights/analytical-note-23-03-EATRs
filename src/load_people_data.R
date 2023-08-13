load_people_data <- function(tawa_output_path, wealth_output_path, wealth_output_cols) {
  dt_people <- merge(
    fread(tawa_output_path),
    TAWApost::read_replicate_weights(
      REPLICATES_DIR, surveys = "HES18", tax_years = 18,
      replicate_labels = 0
    ),
    by = "H_ID"
  )
  dt_wealth_people <- fread(wealth_output_path)
  
  wealth_merge_cols <- c("H_ID", "P_Attributes_PersonNumberInHES")
  dt_wealth_people <- merge(
    dt_wealth_people,
    dt_people[, .SD, .SDcols = c(wealth_merge_cols, "F_ID", "P_ID")],
    by = wealth_merge_cols, all = TRUE
  )
  dt_wealth_people[, P_Attributes_PersonNumberInHES := NULL]
  setcolorder(dt_wealth_people, c("H_ID", "F_ID", "P_ID"))
  
  # Replace any NA's with zero
  wealth_cols <- setdiff(names(dt_wealth_people), c("H_ID", "F_ID", "P_ID"))
  dt_wealth_people[
    , (wealth_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
    .SDcols = wealth_cols
  ]
  
  # Finally, merge desired wealth columns (e.g. net worth) onto people
  wealth_output_cols <- c("Net_Worth")
  people_wealth <- dt_wealth_people[, .SD, .SDcols = c("P_ID", wealth_output_cols)]
  dt_people <- merge(dt_people, people_wealth, by = "P_ID", all = TRUE)
  
  return(dt_people)
}