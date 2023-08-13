TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/get_mean_etr_breakdowns.R")
source("src/get_mean_etr_breakdowns_by_group.R")
source("src/get_mean_etr_breakdowns_by_trimmed_group.R")

etrs_list <- list(
  c(1, 2, 3, 4),
  c(51, 52, 53, 54),
  c(61, 62, 71, 72, 73, 74),
  c(75, 76, 77, 78),
  c(91, 92, 93, 94),
  c(95, 96, 97, 98)
)

trim_quantiles <- c(0, 0.45, 0.55, 1)
trim_extreme_quantiles <- c(0, 0.01, 0.99, 1)

# Load families dataset
all_family_etrs <- rbind(
  fread("data/HES18_analysis_input_family_etrs.csv.gz"),
  fread("data/HES18_imputed_gst_family_etrs.csv.gz"),
  fill = TRUE
)

for (etrs in etrs_list) {
  loginfo("Calculating family etr breakdowns for %s", etrs)
  # Subset to particular etrs
  family_etrs <- all_family_etrs[ETR_Type %in% etrs]
  
  # Require that all etrs are same level
  assertthat::are_equal(all(nchar(etrs) == nchar(etrs[1])), TRUE)
  
  # Drop tax components we don't need - bit messy I know
  if (nchar(etrs[1]) %in% c(1, 2)) {
    max_component <- last(etrs) %>% substr(0,1) %>% as.numeric()
  } else {
    max_component <- 9
  }
  
  loginfo("Subsetting to max component %s", max_component)
  
  tax_components <- names(family_etrs) %>% .[. %like% "TaxComponent"]
  tax_component_indices <- stringr::str_extract(
    tax_components, "(?<=_)[1-9]{1,2}$"
  ) %>% as.numeric()
  keep_tax_components <- paste0(
    "ETR_TaxComponent_",
    tax_component_indices[tax_component_indices <= max_component]
  )
  drop_tax_components <- setdiff(tax_components, keep_tax_components)
  family_etrs <- family_etrs[, .SD, .SDcols = !drop_tax_components]
  
  etr_weight_col <- "AdultEquivalentsWeight"
  
  xfacet <- "Family_Category"
  # subgroups <- c("F_Has_Children", "F_Adult_Earners")
  # extra_groups <- c("Family_Type")
  subgroups <- c()
  extra_groups <- c()
  
  #### Overall population ####
  
  # Overall populations
  xaxis <- NULL
  family_etr_breakdowns <- get_mean_etr_breakdowns_by_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col
  )
  # Overall populations, trimmed off the upper and lower quartiles
  family_etr_breakdowns_trimmed <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_quantiles
  )
  # Overall populations, trimmed off the extreme upper and lower quartiles
  family_etr_breakdowns_trimmed_extremes <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_extreme_quantiles
  )
  
  # By age band
  xaxis <- "max_age_band"
  family_etr_breakdowns_by_age <- get_mean_etr_breakdowns_by_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col
  )
  # By age band, trimmed off the upper and lower quartiles
  family_etr_breakdowns_by_age_trimmed <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_quantiles
  )
  # By age band, trimmed off the extreme upper and lower quartiles
  family_etr_breakdowns_by_age_trimmed_extremes <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_extreme_quantiles
  )
  
  # By income quantile
  xaxis <- "Eq_ETR_Income_AdultEquivalentsWeight_quantile"
  family_etr_breakdowns_by_income <- get_mean_etr_breakdowns_by_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col
  )
  # By income quantile, trimmed off the upper and lower quartiles
  family_etr_breakdowns_by_income_trimmed <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_quantiles
  )
  # By income quantile, trimmed off the extreme upper and lower quartiles
  family_etr_breakdowns_by_income_trimmed_extremes <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_extreme_quantiles
  )
  
  # By wealth quantile
  xaxis <- "Eq_Net_Worth_AdultEquivalentsWeight_quantile"
  family_etr_breakdowns_by_wealth <- get_mean_etr_breakdowns_by_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col
  )
  # By wealth quantile, trimmed off the upper and lower quartiles
  family_etr_breakdowns_by_wealth_trimmed <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_quantiles
  )
  # By wealth quantile, trimmed off the extreme upper and lower quartiles
  family_etr_breakdowns_by_wealth_trimmed_extremes <- get_mean_etr_breakdowns_by_trimmed_group(
    family_etrs, xaxis, xfacet, subgroups, extra_groups, etr_weight_col,
    trim_extreme_quantiles
  )
  
  # Raw
  raw_output <- rbindlist(list(
    "Family_EATR_breakdowns_all" = family_etr_breakdowns,
    "Family_EATR_breakdowns_by_age" = family_etr_breakdowns_by_age,
    "Family_EATR_breakdowns_by_income_and_family" = family_etr_breakdowns_by_income,
    "Family_EATR_breakdowns_by_wealth_and_family" = family_etr_breakdowns_by_wealth,
    "Family_EATR_breakdowns_all_trimmed" = family_etr_breakdowns_trimmed,
    "Family_EATR_breakdowns_by_age_trimmed" = family_etr_breakdowns_by_age_trimmed,
    "Family_EATR_breakdowns_by_income_and_family_trimmed" = family_etr_breakdowns_by_income_trimmed,
    "Family_EATR_breakdowns_by_wealth_and_family_trimmed" = family_etr_breakdowns_by_wealth_trimmed,
    "Family_EATR_breakdowns_all_trimmed_extremes" = family_etr_breakdowns_trimmed_extremes,
    "Family_EATR_breakdowns_by_age_trimmed_extremes" = family_etr_breakdowns_by_age_trimmed_extremes,
    "Family_EATR_breakdowns_by_income_and_family_trimmed_extremes" = family_etr_breakdowns_by_income_trimmed_extremes,
    "Family_EATR_breakdowns_by_wealth_and_family_trimmed_extremes" = family_etr_breakdowns_by_wealth_trimmed_extremes
  ), idcol = "Table", fill = TRUE)
  
  raw_output[, Rounding_Rule := "EATR_Percentage"]
  
  output_path <- file.path(
    sprintf(
      "data/HES18_eatr_breakdowns_raw_%s.csv.gz",
      paste(etrs, collapse = "-")
    )
  )
  fwrite(raw_output, output_path)
}