source("src/get_IDI_helpers.R")

household_wealth_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[wealth_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_wealth]"

IDI_household_wealth <- read_sql_table(household_wealth_SQL_query)
fwrite(IDI_household_wealth, "data/IDI_household_wealth.csv")

business_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[business_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_business]"

IDI_business_wealth <- read_sql_table(business_SQL_query)
fwrite(IDI_business_wealth, "data/IDI_business_wealth.csv")

trust_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[trust_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_trust]"

IDI_trust_wealth <- read_sql_table(trust_SQL_query)
fwrite(IDI_trust_wealth, "data/IDI_trust_wealth.csv")
