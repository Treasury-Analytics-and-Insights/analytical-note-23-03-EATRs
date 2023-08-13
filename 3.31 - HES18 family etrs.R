TAR_settings_path <- "TAR_settings.yaml"
source("src/TAWApost_setup.R")
source("src/get_average_etrs.R")
source("src/plot_average_etrs.R")
source("src/get_etrs_by_group.R")

etrs_list <- list(
  c(1, 2, 3, 4),
  c(51, 52, 53, 54),
  c(61, 62, 71, 72, 73, 74),
  c(75, 76, 77, 78),
  c(91, 92, 93, 94),
  c(95, 96, 97, 98)
)

# Load families dataset - EATR 1, 2, 3, 4, 5, 6, 7 + variants
all_family_etrs <- rbind(
  fread("data/HES18_analysis_input_family_etrs.csv.gz"),
  fread("data/HES18_imputed_gst_family_etrs.csv.gz"),
  fill = TRUE
)

for (etrs in etrs_list) {
  loginfo("Calculating average family etrs for %s", etrs)
  # Subset to particular etrs
  family_etrs <- all_family_etrs[ETR_Type %in% etrs]
  
  etr_weight_col <- "AdultEquivalentsWeight"
  
  xfacet <- "Family_Category"
  subgroups <- c("F_Has_Children", "F_Adult_Earners")
  extra_groups <- c("Family_Type")
  
  # Look at whole-population averages
  average_family_etrs <- get_etrs_by_group(
    family_etrs,
    xaxis = NULL,
    xfacet = xfacet,
    subgroups = subgroups,
    extra_groups = extra_groups,
    weight_col = etr_weight_col
  )
  
  #### Look at averages by age band
  average_family_etrs_by_max_age_band <- get_etrs_by_group(
    family_etrs,
    xaxis = "max_age_band",
    xfacet = xfacet,
    subgroups = subgroups,
    extra_groups = extra_groups,
    weight_col = etr_weight_col
  )
  
  #### Look at averages by income & family type
  average_family_etrs_by_income_and_family_type <- get_etrs_by_group(
    family_etrs,
    xaxis = "Eq_ETR_Income_AdultEquivalentsWeight_quantile",
    xfacet = xfacet,
    subgroups = subgroups,
    extra_groups = extra_groups,
    weight_col = etr_weight_col
  )
  
  #### Look at averages by wealth & family type
  average_family_etrs_by_wealth_and_family_type <- get_etrs_by_group(
    family_etrs,
    xaxis = "Eq_Net_Worth_AdultEquivalentsWeight_quantile",
    xfacet = xfacet,
    subgroups = subgroups,
    extra_groups = extra_groups,
    weight_col = etr_weight_col
  )
  
  # Save raw output
  raw_output <- rbindlist(list(
    "Family_EATRs_all" = average_family_etrs,
    "Family_EATRs_by_age" = average_family_etrs_by_max_age_band,
    "Family_EATRs_by_income_and_family" = average_family_etrs_by_income_and_family_type,
    "Family_EATRs_by_wealth_and_family" = average_family_etrs_by_wealth_and_family_type
  ), idcol = "Table", fill = TRUE)
  
  output_path <- file.path(
    sprintf(
      "data/HES18_analysis_raw_output_%s.csv.gz",
      paste(etrs, collapse = "-")
    )
  )
  fwrite(raw_output, output_path)
}