library(data.table)
library(magrittr)
library(ggplot2)
source("src/get_IDI_helpers.R")

# Concordance between snz_hes_hhld_uid and meshblock
meshblock_query <- "
SELECT [snz_hes_hhld_uid]
      ,[DVMeshblock] as [meshblock_code]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_householddem_1819]
"
dt_meshblock <- read_sql_table(meshblock_query, "IDI_Adhoc")
dt_meshblock[, meshblock_code := as.numeric(meshblock_code)]

# Concordance between meshblock and TA
ta_query <- "
SELECT DISTINCT [meshblock_year_code]
      ,[meshblock_code]
      ,[ta_code]
      ,[ta_name_text]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[meshblock_all]
"
dt_ta_all <- read_sql_table(ta_query, "IDI_Metadata")
dt_ta_all[, meshblock_code := as.numeric(meshblock_code)]
dt_ta_all[, ta_code := as.numeric(ta_code)]

# # Remove duplicates by taking the most recent ta (by meshblock_year_code),
# # for each au
# setorder(dt_ta_all, meshblock_year_code, au_code, ta_code)
# dt_ta <- dt_ta_all[, .(ta_code = last(ta_code)), by = au_code]
# dt_ta[, num_ta_codes := .N, by = au_code]
dt_ta <- dt_ta_all[meshblock_year_code == 2018]

# Manually edit some stubborn ones (Auckland) to match ta's in the HUD data
# ta_code             ta_name_text
# 1:       4          Rodney District
# 2:       5         North Shore City
# 3:       6           Waitakere City
# 4:       7            Auckland City
# 5:      10        Franklin District
# 6:       8             Manukau City
dt_ta[ta_code %in% c(4, 5, 6, 7, 8, 10), ta_code := 76] # These are all Auckland

dt_meshblock_to_ta <- merge(
  dt_meshblock[, .(snz_hes_hhld_uid, meshblock_code)],
  dt_ta[, .(meshblock_code, ta_code)],
  by = "meshblock_code",
  all.x = TRUE
)
dt_hes_to_ta <- dt_meshblock_to_ta[, .(snz_hes_hhld_uid, ta_code)]
fwrite(dt_hes_to_ta, "data/HES19_concordance_hes_to_ta.csv")

# num of bedrooms
bedrooms_query <- "
SELECT [snz_hes_hhld_uid]
      ,[Number_Of_Bedrooms] as [num_bedrooms]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_householddem_1819]
"
bedrooms_dt <- read_sql_table(bedrooms_query, "IDI_Adhoc")
fwrite(bedrooms_dt, "data/HES19_hh_bedrooms.csv")
