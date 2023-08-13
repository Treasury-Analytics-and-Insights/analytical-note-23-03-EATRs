library(data.table)
library(magrittr)
source("src/get_IDI_helpers.R")

hes19_exp_query <- "
SELECT [snz_hes_uid]
      ,[snz_hes_psu_uid]
      ,[snz_hes_strat_uid]
      ,[snz_hes_hhld_uid]
      ,[NZHEC]
      ,[DVID_Code]
      ,[HES_Index]
      ,[Module]
      ,[DVAmount]
      ,[PivotNo]
      ,[DiaryDay]
      ,[DiaryType]
      ,[JointWith]
      ,[StoreType]
      ,[PaidOnline]
      ,[MortCompAssign]
      ,[DVLender]
      ,[DVOutstandingBalance]
      ,[DVLoanNo]
      ,[DVFirstOrOther]
      ,[PropertyType]
      ,[DwellNewOrOld]
      ,[ValIncBusFarm]
      ,[DVInterestRate]
      ,[Interest_Imputed]
      ,[Item_Imputed]
      ,[PeriodCovered]
      ,[DVWksFactor]
      ,[DVMonthOfPayment]
  FROM [IDI_Adhoc].[clean_read_HES].[HES_expenditure_1819]
"

hes19_exp_weights_query <- "
SELECT [snz_hes_hhld_uid]
      ,[FinalWgtExp]
  FROM [IDI_Adhoc].[clean_read_HES].[HES_weightsexp_1819]
"

hes19_exp <- read_sql_table(hes19_exp_query, "IDI_Adhoc")
hes19_exp_weights <- read_sql_table(hes19_exp_weights_query, "IDI_Adhoc")

fwrite(hes19_exp, "data/hes19_exp.csv")
fwrite(hes19_exp_weights, "data/hes19_exp_weights.csv")
