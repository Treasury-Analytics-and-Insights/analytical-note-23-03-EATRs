get_mean_etr_breakdowns_by_trimmed_group <- function(
  dt_etrs, xaxis, xfacet, subgroups, extra_groups, weight_col, trim_quantiles
) {
  groups <- c(xfacet, subgroups, extra_groups)
  # Initialise, in case they should be empty
  mean_etr_breakdown_by_group <- data.table()
  mean_etr_breakdown_by_xfacet <- data.table()
  mean_etr_breakdown_by_subgroups <- data.table()
  
  if (!is.null(groups)) {
    dt_etrs[
      , trimmed_quantile := TAWApost::quantiles(ETR_Value, get(weight_col), trim_quantiles),
      by = c("ETR_Type", xaxis, groups)
    ]
    mean_etr_breakdown_by_group <- get_mean_etr_breakdowns(
      dt_etrs[trimmed_quantile == 2],
      weight_col = etr_weight_col,
      extra_group_by = c(xaxis, groups)
    )
  }
  
  if (!is.null(xfacet)) {
    # All F_Has_Children, All F_Adult_Earners, All Family_Type
    dt_etrs[
      , trimmed_quantile := TAWApost::quantiles(ETR_Value, get(weight_col), trim_quantiles),
      by = c("ETR_Type", xaxis, xfacet)
    ]
    mean_etr_breakdown_by_xfacet <- get_mean_etr_breakdowns(
      dt_etrs[trimmed_quantile == 2],
      weight_col = etr_weight_col,
      extra_group_by = c(xaxis, xfacet)
    )
    mean_etr_breakdown_by_xfacet[, setdiff(groups, xfacet) := "All"]
  }
  
  if (!is.null(subgroups)) {
    # All Family_Category, All Family_Type
    dt_etrs[
      , trimmed_quantile := TAWApost::quantiles(ETR_Value, get(weight_col), trim_quantiles),
      by = c("ETR_Type", xaxis, subgroups)
    ]
    mean_etr_breakdown_by_subgroups <- get_mean_etr_breakdowns(
      dt_etrs[trimmed_quantile == 2],
      weight_col = etr_weight_col,
      extra_group_by = c(xaxis, subgroups)
    )
    mean_etr_breakdown_by_subgroups[, setdiff(groups, subgroups) := "All"]
  }
  
  # All everything
  dt_etrs[
    , trimmed_quantile := TAWApost::quantiles(ETR_Value, get(weight_col), trim_quantiles),
    by = c("ETR_Type", xaxis)
  ]
  mean_etr_breakdown_by_xaxis <- get_mean_etr_breakdowns(
    dt_etrs[trimmed_quantile == 2],
    weight_col = etr_weight_col,
    extra_group_by = xaxis
  )
  mean_etr_breakdown_by_xaxis[, (groups) := "All"]
  
  mean_etr_breakdown_by_all_groups <- rbindlist(list(
    mean_etr_breakdown_by_group,
    mean_etr_breakdown_by_xfacet,
    mean_etr_breakdown_by_subgroups,
    mean_etr_breakdown_by_xaxis
  ), use.names = TRUE)
  
  mean_etr_breakdown_by_all_groups[, Rounding_Rule := "EATR_Percentage"]
  
  return(mean_etr_breakdown_by_all_groups)
}