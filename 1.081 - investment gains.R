library(data.table)
library(magrittr)

people_wealth <- fread("data/people_wealth.csv")

people_wealth_long <- melt(
  people_wealth[, .SD, .SDcols = !c("Net_Worth", "Assets", "Liabilities")],
  id.vars = c("snz_hes_uid"),
  variable.name = "Asset_Class", value.name = "Value"
)

# Calculate investment gains
dt_cg <- fread("data/capital_gains_rates.csv")
dt_cg[, Asset_Class := snakecase::to_any_case(
  Asset_Class, case = "upper_camel", sep_out = "_"
)]

people_wealth_long <- merge(people_wealth_long, dt_cg, by = "Asset_Class")

people_wealth_long[, Short_Run_Capital_Gain := Value*Short_Run_Rate]
people_wealth_long[, Long_Run_Capital_Gain := Value*Long_Run_Rate]

people_investment_capital_gains <- people_wealth_long[
  Asset_Class != "Owner_Occupied",
  .(
    Investment_Value = sum(Value),
    Short_Run_Investment_Gain = sum(Short_Run_Capital_Gain),
    Long_Run_Investment_Gain = sum(Long_Run_Capital_Gain)
  ),
  by = .(snz_hes_uid)
  ]

fwrite(people_investment_capital_gains, "data/investment_gains.csv")
