 

# Prepare environment -----------------------------------------------------
rm(list = ls()); gc()
source('r/helper_functions.R')

# Identify stratified sample ----------------------------------------------

# sample of sp_ids provided by Romy
# Read in data from stratified subset
stratSample <- fread('E:/CEA_DATA/demographics/stratified_sample.csv'
                     , header = TRUE
                     , colClasses = (PREM_ID = "character")) # data table with prem id

# export prem_id for filtering in Teradata
write.table(x = stratSample[, .SD, .SDcols = 'PREM_ID'], file = "E:/CEA_DATA/demographics/sample_prem_id.txt", row.names = FALSE, quote = FALSE, col.names = FALSE)

# switch to Teradata to import prem_id and export demographic information from cust e fzn





