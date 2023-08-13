get_mean_etr <- function(
  dt, weight_col, lower_quantile, upper_quantile, group_by
) {
  TOL <- 1e-10
  
  # These are column names, so ensure they are of character type
  lower_quantile <- as.character(lower_quantile)
  upper_quantile <- as.character(upper_quantile)
  
  dt[, lower_boundary := get(lower_quantile)]
  dt[, upper_boundary := get(upper_quantile)]
  
  # Special case where the two bounds should be equal,
  # but may not be due to numerical precision
  dt[abs(upper_boundary - lower_boundary) < TOL, upper_boundary := lower_boundary]
  
  dt2 <- dt[
    ETR_Value >= lower_boundary - TOL & ETR_Value <= upper_boundary + TOL,
    .SD, .SDcols = c("ETR_Value", weight_col, group_by)
  ]
  setnames(dt2, weight_col, "Weight")
  
  mean_etr <- dt2[
    , .(
      Value = sum(Weight*ETR_Value),
      Population = sum(Weight),
      Sample = .N
    ),
    by = group_by
  ]
  mean_etr[, Value := Value / Population]
  return(mean_etr)
}
get_average_etrs <- function(
  dt,
  weight_col = "Weight",
  extra_group_by = NULL,
  calc_box_and_whiskers = FALSE
) {
  group_by <- unique(c("ETR_Type", extra_group_by))
  mean_eatrs <- dt[, .(
    Value = weighted.mean(ETR_Value, get(weight_col)),
    Population = sum(Weight),
    Sample = 1.0*.N
  ), by = group_by]
  
  median_eatrs <- dt[, .(
    Quantile = 0.5,
    Value = reldist::wtd.quantile(x = ETR_Value, q = 0.5, weight = get(weight_col)),
    Population = sum(Weight),
    Sample = 1.0*.N
  ), by = group_by]
  
  average_eatrs <- rbindlist(list(
    "Mean" = mean_eatrs,
    "Median" = median_eatrs
  ), idcol = "Average_Type", fill = TRUE)
  
  if (calc_box_and_whiskers) {
    box_and_whisker_quantiles <- c(0, 0.25, 0.75, 1) # we already calculated 0.5
    quantiles <- dt[, .(
      Quantile = box_and_whisker_quantiles,
      Quantile_Value = reldist::wtd.quantile(x = ETR_Value, q = box_and_whisker_quantiles, weight = .SD[[1]])
    ), .SDcols = weight_col, by = group_by]
    
    # We already calculated the median; add it on to the rest
    median_quantile <- median_eatrs[
      , c(.(Quantile = 0.5, Quantile_Value = Value), .SD), .SDcols = group_by
    ]
    quantiles <- rbind(quantiles, median_quantile)
    
    quantiles <- dcast(
      quantiles, ... ~ Quantile, value.var = "Quantile_Value"
    )
    
    dt <- merge(dt, quantiles, by = group_by)
    
    box_and_whisker_eatrs <- rbindlist(list(
      "Lower_Whisker" = get_mean_etr(dt, weight_col, lower_quantile = 0, upper_quantile = 0.25, group_by),
      "Lower_Box" = get_mean_etr(dt, weight_col, lower_quantile = 0.25, upper_quantile = 0.5, group_by),
      "Upper_Box" = get_mean_etr(dt, weight_col, lower_quantile = 0.5, upper_quantile = 0.75, group_by),
      "Upper_Whisker" = get_mean_etr(dt, weight_col, lower_quantile = 0.75, upper_quantile = 1, group_by)
    ), idcol = "Average_Type")
    
    average_eatrs <- rbind(average_eatrs, box_and_whisker_eatrs, fill = TRUE)
  }
  
  setnames(average_eatrs, "Value", "Average")
  
  # Calculate changes
  change_by <- c("Average_Type", extra_group_by)
  setorderv(average_eatrs, group_by)
  average_eatrs[, Change_In_Average := c(NA, diff(Average)), by = change_by]
  
  average_eatrs <- melt(
    average_eatrs, measure.vars = c("Average", "Change_In_Average"),
    variable.name = "Variable", value.name = "Value"
  )
  
  # Subset away changes for the first ETR, since these are NA by definition
  average_eatrs <- average_eatrs[
    !(ETR_Type == first(ETR_Type) & Variable == "Change_In_Average")
  ]
  
  return(average_eatrs)
}
