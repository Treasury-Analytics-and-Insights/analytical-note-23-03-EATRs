source("src/TAWArun_setup.R")
library(data.table)
library(magrittr)

hes18_dataprep_path <- "~/project_dir/Projects/HES21 Data Prep/HES18/26 DB_202110_INTLAG/Data/R_tawa_hes18.csv"

hes18_dataprep <- fread(hes18_dataprep_path)
# Sum up over months
hes18_acc_income <- hes18_dataprep[, .(P_Income_ACC = sum(EIE_CLM)), by = snz_hes_uid]

fwrite(hes18_acc_income, "data/TAWA_acc_income.csv")

TAWA_db <- TAWArun::read_TAWA_db(DB_PATH, dry_run = FALSE)

# Modify the input database
hes18_dataprep_path <- "~/project_dir/Projects/HES21 Data Prep/HES18/26 DB_202110_INTLAG/Data/R_tawa_hes18.csv"
hes18_dataprep <- fread(hes18_dataprep_path)
hes18_acc_income_even <- hes18_dataprep[, .(snz_hes_uid, Period = 2*month, P_Income_ACC = 0.5*EIE_CLM)]
hes18_acc_income_odd <- copy(hes18_acc_income_even)
hes18_acc_income_odd[, Period := Period - 1]

hes18_acc_income <- rbind(hes18_acc_income_even, hes18_acc_income_odd)
setorder(hes18_acc_income, snz_hes_uid, Period)

TAWA_db <- merge(TAWA_db, hes18_acc_income, by = c("snz_hes_uid", "Period"))
TAWA_db[, P_Income_Totals_OtherTaxable := P_Income_Totals_OtherTaxable - P_Income_ACC]
fwrite(TAWA_db, "data/HES18_TAWA_db_without_ACC_income.csv.gz")
