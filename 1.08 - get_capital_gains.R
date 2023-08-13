library(data.table)
library(magrittr)

dt_cg <- openxlsx::read.xlsx(
  "inputs/Combined capital gains data.xlsx", sheet = "SUMMARY", startRow = 4
) %>% setDT()

# Drop Owner-occupied housing, it is TA specific
dt_cg <- dt_cg[2:.N]

setnames(dt_cg, c("Asset_Class", "Short_Run_Rate", "Long_Run_Rate", "Source"))

dt_cg[, Short_Run_Rate := round(as.numeric(Short_Run_Rate), 4)]
dt_cg[, Long_Run_Rate := round(as.numeric(Long_Run_Rate), 4)]

fwrite(dt_cg, "data/capital_gains_rates.csv")
