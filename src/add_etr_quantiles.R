add_etr_quantiles <- function(
  dt_etrs,
  quantile_col,
  quantile_weight_col,
  quantile_probs,
  group_by = "ETR_Type"
) {
  etr_quantile <- paste0(quantile_col, "_", quantile_weight_col, "_quantile")
  dt_etrs[
    , (etr_quantile) := TAWApost::quantiles(
      x = get(quantile_col),
      weights = get(quantile_weight_col),
      probs = quantile_probs
    ) %>% factor(levels = seq_along(quantile_probs), labels = quantile_probs),
    by = group_by
  ]
  return(invisible(dt_etrs))
}