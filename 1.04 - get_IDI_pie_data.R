source("src/get_IDI_helpers.R")
library(ggplot2)
library(lubridate)

pie_query <- "
SELECT [snz_pie_ird_uid]
      ,[snz_investors_ird_uid]
      ,[fstrinvestorcertidentifier]
      ,[fcurrateyearend]
      ,[fblnratechanged]
      ,[fcurunitsheldyearend]
      ,[fdtmperiod]
      ,[fcurtotalincomeloss]
      ,[fcurtotaltaxcredits]
      ,[fcurnettaxpaid]
      ,[fcurtaxableincomelowmid]
      ,[fcurtaxpaidlowmid]
      ,[fcurtotaltaxcreditslowmid]
      ,[fcurzeroexitedtaxableincome]
      ,[fcurtotalforeigntaxcredits]
      ,[fcurtotalothertaxcredits]
      ,[fcurtotalictaxcredits]
      ,[fcurtotalrwttaxcredits]
      ,[fcurtaxpaymentexitedinvestors]
      ,[fstrcountry]
      ,[fstrnfidta]
  FROM [IDI_Adhoc].[clean_read_IR].[PIE_2018_2020]
  WHERE snz_investors_ird_uid IN (
    SELECT DISTINCT snz_ird_uid FROM [IDI_Sandpit].[DL-MAA2015-27].[HES18_LINK_202110_INTLAG]
  )
"

IDI_pie <- read_sql_table(pie_query)

linking_table_query <- "
SELECT [uid] as snz_hes_uid
      ,[age]
      ,[birth_year]
      ,[birth_month]
      ,[interview_date]
      ,[wgt]
      ,[snz_ird_uid]
      ,[snz_msd_uid]
  FROM [IDI_Sandpit].[DL-MAA2015-27].[HES18_LINK_202110_INTLAG]
"
linking_table <- read_sql_table(linking_table_query)

IDI_pie[, ":="(
  snz_ird_uid = snz_investors_ird_uid,
  PIE_return_date = paste0("TY", year(as.Date(fdtmperiod, "%Y-%m-%d")) - 2000)
)]

IDI_pie <- merge(
  linking_table[, .(snz_hes_uid, snz_ird_uid, interview_date)],
  IDI_pie,
  by = "snz_ird_uid"
)

hes18_IDI_pie <- IDI_pie[, .(
  P_Income_PIE_Net = sum(fcurtotalincomeloss),
  P_Tax_PIE_Net = sum(fcurnettaxpaid),
  P_TaxCredits_PIE = sum(fcurtotaltaxcredits)
), keyby = .(snz_hes_uid, PIE_return_date, interview_date)]

hes18_IDI_pie_wide <- dcast(
  melt(hes18_IDI_pie, id.vars = c("snz_hes_uid", "PIE_return_date", "interview_date")),
  ... ~ PIE_return_date, value.var = "value",
  fill = 0 # If no record exists assume it is zero
)

# "Impute" missing tax year 2017 using tax year 2018
hes18_IDI_pie_wide[, TY17 := TY18]
setcolorder(hes18_IDI_pie_wide, c("snz_hes_uid", "interview_date", "variable", "TY17", "TY18", "TY19"))
# Delete unused tax year 2020
hes18_IDI_pie_wide[, TY20 := NULL]

TY17_interval <- interval(ymd("2016-03-01"), ymd("2017-03-31"))
TY18_interval <- interval(ymd("2017-03-01"), ymd("2018-03-31"))
TY19_interval <- interval(ymd("2018-03-01"), ymd("2019-03-31"))

hes18_IDI_pie_wide[, ":="(
  Months_TY17 = intersect(interval(interview_date - years(1), interview_date), TY17_interval) %/% months(1),
  Months_TY18 = intersect(interval(interview_date - years(1), interview_date), TY18_interval) %/% months(1),
  Months_TY19 = intersect(interval(interview_date - years(1), interview_date), TY19_interval) %/% months(1)
)]
hes18_IDI_pie_wide[is.na(Months_TY17), Months_TY17 := 0]
hes18_IDI_pie_wide[is.na(Months_TY18), Months_TY18 := 0]
hes18_IDI_pie_wide[is.na(Months_TY19), Months_TY19 := 0]

test_all_add_to_12months <- hes18_IDI_pie_wide[, all(Months_TY17 + Months_TY18 + Months_TY19 == 12)]
assertthat::are_equal(test_all_add_to_12months, TRUE)

hes18_IDI_pie_wide[, value := TY17/12*Months_TY17 + TY18/12*Months_TY18 + TY19/12*Months_TY19]

hes18_IDI_pie_out <- dcast(
  hes18_IDI_pie_wide[, .(snz_hes_uid, variable, value)],
  ... ~ variable, value.var = "value"
)

fwrite(hes18_IDI_pie_out, "data/HES18_pie_income.csv")

# ggplot(
#   hes18_IDI_pie, aes(x = fcurtotalincomeloss, y = fcurnettaxpaid)
# ) + geom_point() +
#   facet_grid(fdtmperiod~.)
# 
# plot_data <- hes18_IDI_pie[, .(Sample = uniqueN(snz_ird_uid)), by = snz_pie_ird_uid]
# plot_data[, Sample_Percentage := Sample / sum(Sample)]
# setorder(plot_data, Sample)
# plot_data[, colour_index := 1:.N]
# 
# plot_data <- merge(hes18_IDI_pie, plot_data, by = "snz_pie_ird_uid")
# ggplot(
#   plot_data, aes(x = fcurtotalincomeloss, y = fcurnettaxpaid, colour = colour_index)
# ) + geom_point() +
#   facet_grid(fdtmperiod~.)
# 
# ggplot(
#   hes18_IDI_pie[abs(fcurtotalincomeloss) > 10], aes(x = fcurtotalincomeloss, fill = fdtmperiod)
# ) +
#   # geom_freqpoly() +
#   geom_histogram() +
#   facet_grid(fdtmperiod~.) +
#   scale_x_continuous(limits = c(-5e3, 5e3))
# 
# # hes18_eie_raw <- fread("~/project_dir/Projects/HES21 Data Prep/HES18/26 DB_202110_INTLAG/Data/eie_raw.csv")
