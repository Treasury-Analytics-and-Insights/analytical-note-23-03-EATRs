library(data.table)
library(magrittr)
library(ggplot2)
source("src/get_IDI_helpers.R")

dt_rents_all <- openxlsx::read.xlsx(
  "inputs/Combined capital gains data.xlsx", sheet = "Rents"
) %>% setDT()
dt_rents <- dt_rents_all[, .(
  ta_code = TA, TA_Name,
  num_bedrooms = Bedrooms,
  Year = as.numeric(Year_Month),
  Sample = Rent_How_Many,
  Rent_GeoMean = round(Rent_GeoMean)
)]

# dt_rents_national_av <- dt_rents[
#   , .(
#     Rent_GeoMean_National = exp(
#       1/sum(Sample)*sum(Sample*log(Rent_GeoMean))
#     )
#   ),
#   keyby = .(num_bedrooms, Year)
# ]
# 
# ta_codes <- dt_rents[, unique(ta_code)]
# num_bedrooms <- dt_rents[, unique(num_bedrooms)]
# years <- dt_rents[, seq(min(Year), max(Year))]
# 
# dt_rents_approx <- expand.grid(
#   Year = years, ta_code = ta_codes, num_bedrooms = num_bedrooms
# ) %>% setDT()
# 
# dt_rents_approx <- merge(
#   dt_rents_approx,
#   dt_rents[, .(Year, ta_code, num_bedrooms, Rent_GeoMean)],
#   by = c("Year", "ta_code", "num_bedrooms"),
#   all = TRUE
# )
# dt_rents_approx <- merge(
#   dt_rents_approx,
#   dt_rents_national_av,
#   by = c("Year", "num_bedrooms"),
#   all = TRUE
# )

dt_rents_2018 <- dt_rents[Year == 2018]
fwrite(dt_rents_2018, "data/rents_2018.csv.gz")

dt_rents_2019 <- dt_rents[Year == 2019]
fwrite(dt_rents_2019, "data/rents_2019.csv.gz")

dt_yields_all <- openxlsx::read.xlsx(
  "inputs/Combined capital gains data.xlsx", sheet = "Rental Yields"
) %>% setDT()
dt_yields <- dt_yields_all[, .(
  ta_code = TA, TA_Name,
  num_bedrooms = Bedrooms,
  Year = as.numeric(Year_Month),
  Sample = Rents_How_Many,
  Rent_Yield_GeoMean = round(Rent_Yield_GeoMean, 1)/100
)]

dt_yields_2018 <- dt_yields[Year == 2018]
fwrite(dt_yields_2018, "data/yields_2018.csv.gz")

dt_yields_2019 <- dt_yields[Year == 2019]
fwrite(dt_yields_2019, "data/yields_2019.csv.gz")
