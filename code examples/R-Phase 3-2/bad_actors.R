# bad actor function: identify duplicate SAs in a year before being sent to billing calculator
library(purrr)
library(data.table)
library(dplyr)
library(stringr)
# for this version it will read the tou csv file
# will eventually find bad actors

BILL_LOCATION <- '../CEA_DATA/billing_data/electric'

x <- dir(BILL_LOCATION, recursive = TRUE, pattern = 'results', full.names = TRUE)[3]

bills_org <- map(.x = x, .f = function(x){
  d = fread(x
            , header = TRUE, sep = 'auto'
            , check.names = TRUE
            , stringsAsFactors = TRUE
            , integer64 = 'character'
            # , select = SELECT_COLS
            , verbose = FALSE)}) %>% rbindlist()

bills_org[,.N]
rbindlist(bills_new)[,.N]
bill_names <- read.table(file = x, header = TRUE, nrows = 5, sep = ';')
bills_new <- map(.x = x, .f = function(x){
  d = fread(x
            , header = TRUE, sep = 'auto'
            , check.names = TRUE
            , stringsAsFactors = TRUE
            , integer64 = 'character'
            # , select = SELECT_COLS
            , verbose = FALSE)%>%
    as.data.frame() %>% 
    # mutate(sample_number = str_extract(x,'\\d+')) %>% 
    as.data.table()
  # sp = d[,.N, by = .(prem_id, sa_id, sample, bill_dt, percentile)][N ==1][,unique(prem_id)]
  sp=as.character(d[,.N, by = .(sp_id, nrml_yyyymm)][N ==1][,unique(sp_id)])
  d = d[sp_id %in% c(sp)]})
  # sa = d[,.N, by = .(bill_dt, prem_id, percentile)][N!=1][,unique(prem_id)]
  # d = d[!prem_id %in% c(sa)]

getwd()
SAMPLE_SIZE <- 7000
OUT_PATH <- paste0('E:/CEA_DATA/billing_data/electric/',SAMPLE_SIZE)
OUT_PATH <- paste0('../CEA_DATA/billing_data/electric/',SAMPLE_SIZE,'/')

BILL_TOU_PATH <- paste0(OUT_PATH,'tiered_billing_results_tou.csv')

write.table(bills_new, BILL_TOU_PATH, row.names = FALSE, quote = FALSE, sep = ';')
