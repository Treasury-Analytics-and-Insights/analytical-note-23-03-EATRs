library(data.table)
library(magrittr)
library(ggplot2)
source("src/get_IDI_helpers.R")

# people_wealth <- fread("data/people_wealth.csv")
# people_wealth_owner_occupied <- people_wealth[, .(snz_hes_uid, Owner_Occupied)]

# Calculate HPIs by TA
dt_hpi_all <- openxlsx::read.xlsx(
  "inputs/Combined capital gains data.xlsx", sheet = "HPIs"
) %>% setDT()

dt_hpi <- dt_hpi_all[, .(
  ta_code = TA, TA_Name,
  num_bedrooms = Bedrooms,
  Year = as.numeric(Year_Month),
  SPAR = as.numeric(SPAR),
  Sample = How_Many_Sales_Used
)]
setorder(dt_hpi, ta_code, num_bedrooms, Year)

# There are some NA's - interpolate
# dt_hpi[, SPAR_approx := zoo::na.approx(SPAR), by = .(ta_code, num_bedrooms)]

ta_codes <- dt_hpi[, unique(ta_code)]
num_bedrooms <- dt_hpi[, unique(num_bedrooms)]
years <- dt_hpi[, seq(min(Year), max(Year))]

dt_hpi_approx <- expand.grid(
  Year = years, ta_code = ta_codes, num_bedrooms = num_bedrooms
) %>% setDT()

dt_hpi_approx <- merge(
  dt_hpi_approx,
  dt_hpi[, .(Year, ta_code, num_bedrooms, SPAR)],
  by = c("Year", "ta_code", "num_bedrooms"),
  all = TRUE
)

for (ta in ta_codes) {
  for (rooms in num_bedrooms) {
    hpi_data <- dt_hpi_approx[ta_code == ta & num_bedrooms == rooms, .(Year, SPAR)]
    if (any(is.na(hpi_data$SPAR)) & !all(is.na(hpi_data$SPAR))) {
      hpi_data_na <- hpi_data[is.na(SPAR)]
      hpi_lm <- lm(SPAR~Year, hpi_data)
      # Replace NA's with the predicted values
      dt_hpi_approx[ta_code == ta & num_bedrooms == rooms & is.na(SPAR), SPAR_approx := predict(hpi_lm, .SD)]
      # Keep non-NA's
      dt_hpi_approx[ta_code == ta & num_bedrooms == rooms & !is.na(SPAR), SPAR_approx := SPAR]
    } else {
      dt_hpi_approx[ta_code == ta & num_bedrooms == rooms, SPAR_approx := SPAR]
    }
  }
}

dt_hpi_out <- dt_hpi_approx[, .(Year, ta_code, num_bedrooms, SPAR = SPAR_approx)]

dt_hpi_out[, hpi_short_term_growth_rate := SPAR / shift(SPAR) - 1, by = .(ta_code, num_bedrooms)]

dt_hpi_short_term <- dt_hpi_out[Year == 2018, .(ta_code, num_bedrooms, hpi_short_term_growth_rate)]

dt_hpi_long_term <- dt_hpi_out[
  Year %between% c(2009, 2018),
  .(hpi_long_term_growth_rate = prod(1 + hpi_short_term_growth_rate) ^ (1 / .N) - 1),
  by = .(ta_code, num_bedrooms)
]

dt_hpi_growth_rates <- merge(
  dt_hpi_short_term, dt_hpi_long_term,
  by = c("ta_code", "num_bedrooms"), all = TRUE
)

fwrite(dt_hpi_growth_rates, "data/hpi_growth_rates.csv")

# dt_hpi[, HPI := SPAR / first(SPAR), by = .(TA, Bedrooms)]
# ggplot(
#   dt_hpi,
#   aes(x = Year, y = HPI, colour = factor(TA))
# ) +
#   geom_line() +
#   facet_grid(Bedrooms~.)
# 
# 
# ggplot(
#   dt_hpi_growth_rates,
#   aes(x = num_bedrooms, y = geometric_mean_growth_rate, colour = factor(ta_code))
# ) +
#   geom_point()
