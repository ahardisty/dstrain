PACKAGES <- c('ggplot2', 
              'data.table',
              'tidyr',
              # 'plyr',
              'dplyr', 
              'randomForest', 
              'earth', 
              'lubridate',
              'rattle',
              'RCurl',
              'rpart',
              'rpart.plot',
              'haven',
              'stringr',
              'broom',
              'purrr',
              'readxl')

# columns for manipulating interval data
TRAIN_YEAR <- 2014
TEST_YEAR <- 2015
LAG_YEARS <- 5
LOWER <- paste0(TEST_YEAR-5,'-01-01')
UPPER <- paste0(TEST_YEAR,'-12-31')
CLIMATE_ZONES <- c('T','W')
LAG_TERMS <- c(paste0('LAG_',1:5))
# weather_wide[,c(lag_terms) := shift(.SD, 1:length(lag_terms),0, 'lag')
#              , .SDcols = 'temperature', by = key(weather_wide)]

MOD_COLS <- c('TEMP_NORMAL')
BASE_COLS <- c('CONS_USG')
WEA_COLS <- c('date', 'opr_area_cd', 'model_name', 'temp_actual', 'temp_normal', 'temp')
CHAR_COLS <- c('start_date','stop_date', 'usage date', 'daily_end')
FCTR_COLS <- c('sp_id', 'sa_id', 'prem_id')
# MONTH_LEVELS <- c('Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','O'))

# source function and data formatting scripts
source('r/helper_functions.R')
# Load packages
sapply(PACKAGES, get_packages)


# SCRIPT ENHANCEMENTS -----------------------------------------------------


# Prepare usage simulations for bill calculation --------------------------
# start_time <- proc.time()
OUT_LOC <-  '../CEA_DATA/billing_data/input/'

MONTH_COLS <- 1:12 # used to create BILL_CYCLE variables and for full 12 month GBC
RENUM_COLS <- 1:2 # used to calculate truncated GBC
MON_NUM_1D <- match(c('Dec','Mar'), month.abb) # month numbers with one digit
MON_NUM_2D <- sprintf('%02d', match(c('Dec','Mar'), month.abb)) # month numbers with two digits
BILL_CYCLE_MODEL <- flatten_chr(map2(list(TEST_YEAR, TRAIN_YEAR), list(MON_NUM_2D),paste0)) # bill cycle with two digits

NARROW_COLS <- c('SA_ID', 'PREM_ID', 'ACCT_ID', 'CSCHED', 'CATFLAG','LNUMUNIT', 'NUMUNIT') # index columns - not cast
WIDE_COLS <- c('CSCH__','THM__','TR1__','TR2__','DAY__', 'LNUMUNIT','NUMUNIT', 'PDT__','CDT__', 'BILL_CYCLE','EMP', 'BILL_CYCLE') # variable columns

ID_COLS <- c('SA_ID', 'PREM_ID', 'ACCT_ID') # variables that narrow customers - used in GBC

WIDE_COLS_GBC <- make_cols(WIDE_COLS, RENUM_COLS, paste0)

PROG_COLS <- c('BPP','SPP','CARE') # program participation columns used for bill analysis
BILL_COLS <- c('BILL_CYCLE') # bill cycle columns used for bil merging and analysis
CALC_COLS <- c('BASAMT', 'BPP', 'SPP', 'CARE', 'BILL_CYCLE') # variables used to calculate and analyze consumption - not part of GBC
CAT_COLS <- c('CATFLAG') # wide transportation type indicator cast by RENUM_COLS and used in GBC
UNIT_COLS <- c('LNUMUNIT','NUMUNIT') # narrow number of units for CARE non-CARE cast by RENUM_COLS and used in GBC
GBC_COLS <- c(ID_COLS, WIDE_COLS_GBC, CAT_COLS, UNIT_COLS) # columns required for Gas Billing Calculator (GBC)

# required variables to read from billing determinant template that map with predicted and actual usage
BD_COLS <- c('SERVICE AGREEMENT ID' # SA_ID: required
                   , 'PREMISE ID' # PREM_ID: required
                   , 'ACCOUNT ID' # ACCT_ID: required
                   , 'COMPRESSED RATE SCHED' # CSCH__: required
                   , 'NUMBER OF DAYS' # DAY: required
                   , 'PRIOR READ DATE' # PDT__: required
                   , 'CURRENT READ DATE' # CDT__: required
                   , 'CORE-TRANSPORT FLAG' # CATFLAG; required, rceconcile
                   , 'RATE SCHEDULE (CDX)' # CSCHED: required
                   , 'NUMBER OF CARE UNITS' # LNUMUNIT; required, rceconcile 
                   , 'NUMBER OF UNITS' # NUMUNIT; required
                   , 'EMPLOYEE CODE' # EMP; required
                   , 'BASELINE AMOUNT' # BASAMT :required for calculation
                   , 'BALANCE PAYMENT PLAN INDICATOR' # BPP: not required
                   , 'SPECIAL PAY PLAN INDICATOR' # SPP: not required
                   , 'CARE INDICATOR' # CARE: not required
                   , 'BILL CYCLE') # BILL_CYCLE: not required

# rename billing determinant variable to match GBC variable names
GBC_RENAME <- c('SA_ID' # required
                , 'PREM_ID' # required
                , 'ACCT_ID' # required
                , 'CSCH__' # required
                , 'DAY__' # required
                , 'PDT__' # required
                , 'CDT__' # required
                , 'CATFLAG' # required
                , 'CSCHED' # required
                , 'LNUMUNIT' # required
                , 'NUMUNIT' # required
                , 'EMP' # required
                , 'BASAMT' # required
                , 'BPP' # not required
                , 'SPP' # not required
                , 'CARE' # not required
                , 'BILL_CYCLE') # not required

# merge columns from billing determinant to calculate tiers
BILL_MERGE <- c('customer','year',GBC_RENAME) # add customer variable for easy indexing
  
KEEP_COLS <- c('customer', 'year', 'train_indicator',  'model_name', 'month', 'month_name'
               , 'usage_type','THM__', 'CURRENT_BILL')

# read in sample data and determine variable types ------------------------

# train, test and baseline predicted and actual gas usage to be tiered for billing

usage_cols <- col_types(dir = '../CEA_DATA/model_predictions', pat = '.txt', n = 5, fctr_cols = 'customer')

# billing determinant baseline amounts data for tier calculations
bd_cols <-col_types('../CEA_DATA/billing_data/', pat = '.billing', n = 5, sep ='\t'
                            , fctr_cols = c('BILL CYCLE','SMARTMETER STATUS CODE','SERVICE AGREEMENT ID'
                                            ,'ACCOUNT ID','PREMISE ID','SERVICE POINT ID','UNIQUE SERVICE AGREEMENT ID'))
  
# actual GBC format to verify variable names and data types
gbc_example <- as.data.table(read_sas('../CEA_DATA/billing_data/w_res2015.sas7bdat')
                            ,b7cat = NULL)[1:10]

# read and format actual data --------------------------------------------------------

# train, test and baseline predicted and actual gas usage to be tiered for billing
usage_data <-rbindlist(map(usage_cols[['files']], function(x){ # read pivot data into list of data tables
    fread(x, header = TRUE
          , strip.white = TRUE
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , verbose = FALSE
          , colClasses = usage_cols[['classes']]
          , na.strings = '?')
  }[season == 'winter', season := 'W',])) # change winter variable in read

usage_data[,BILL_CYCLE := paste0(year, sprintf('%02d', month)),] # create bill cycle variable
usage_data <- usage_data[BILL_CYCLE %in% BILL_CYCLE_MODEL,] # limit to Dec and Mar 
unique_customers <- as.character(usage_data[,unique(customer)]) # find unique customers 
usage_data[,date:=as.IDate(date, format = '%Y-%m-%d')] # correct date format


# read in billing determinants
  bill_dtrm <- rbindlist(map(bd_cols[['files']], function(x){ # read pivot data into list of data tables
    fread(x, header = TRUE
          , strip.white = TRUE
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , verbose = FALSE
          , select = c(BD_COLS)
          , colClasses = bd_cols[['classes']]
          , na.strings = '?')
  }[,customer := paste0(`PREMISE ID`,'_',`ACCOUNT ID`) # create customer variable
    ][,`CORE-TRANSPORT FLAG`:=ifelse(`CORE-TRANSPORT FLAG` == 'BUNDLED', 1,0)])) # change transport variable to binary from character

  setcolorder(bill_dtrm, c('customer',BD_COLS)) # change column order to rename to match GBC_RENAME
  setnames(bill_dtrm, old = colnames(bill_dtrm), new = c('customer', GBC_RENAME))
  bill_dtrm[,c('CDT__','PDT__') := lapply(.SD, function(x) as.IDate(x, format = '%m/%d/%Y'))
            , .SDcols = c('CDT__','PDT__')]
         
  ###create function called limit_season###
  bill_dtrm <- bill_dtrm[BILL_CYCLE %in% BILL_CYCLE_MODEL,] # limit to bill cycles in Dec and Mar
  bill_dtrm[, `:=` (year = str_sub(BILL_CYCLE,1,4) # create bill year
                    , month = str_sub(BILL_CYCLE,5,6))] # create bill month
  
  bill_dtrm[,train_indicator := ifelse(year == TRAIN_YEAR,'TRAIN' # assign TRAIN / TEST variable
                                       , ifelse(year == TEST_YEAR,'TEST','out of sample')),]
  
  setkey(bill_dtrm, customer, BILL_CYCLE) # set key for merge
  set_zero(bill_dtrm)


  # read in tariff schedule
  # gas_rates <- data.table(read_excel("../CEA_DATA/billing_data/gas_tariff_14-15.xlsx", sheet = 2)[1:5])
  
# Aggregate usage and Merge with billing determinants ---------------------------------------------------------
  
  setkey(usage_data, train_indicator, customer, acct_id, month, model_name
         , climate_zone_cd, month_name, BILL_CYCLE) # set key for merge and aggregation
  
  usage_data <- usage_data[, lapply(.SD, sum) # sum usage to BILL_CYCLE
                           , by = key(usage_data)
                           ,.SDcols = c('.pred','usage_actual')]
  
  usage_data_mlt <- melt.data.table(usage_data # melt to long to tier predicted and actual usage; change variable to THM__
                                          , measure.vars = c('.pred','usage_actual')
                                          , variable.name = paste0('usage_type')
                                          , value.name = 'THM__')
  
setkey(usage_data_mlt, customer, BILL_CYCLE) # set key to merge 
setkey(bill_dtrm, customer, BILL_CYCLE) # set key to merge 

billing_merge <- merge(x = usage_data_mlt,
                         y = bill_dtrm[,(BILL_MERGE), with = FALSE], allow.cartesian = TRUE)
  
billing_merge[,`:=` ('TR1__' = tier(THM__, BASAMT, 0, 1), # Determine billing tier amounts
                   'TR2__' = tier(THM__, BASAMT, 1, 100))
                ][, month_group :=.GRP, by = .(month) # rename by month order insteand of month number
                    ][,model_index:=tolower(paste0(train_indicator,'_',year,'_'
                                                   ,gsub(pattern = '\\.pred','predicted',x = usage_type)))]

# Prep for SAS billing ----------------------------------------------------

MODEL_INDEX <-c('train_2014_usage_actual'
                , 'test_2015_usage_actual'
                , 'test_2015_predicted'
                , 'baseline_2015_predicted')


billing_merge <- subset(billing_merge, model_index %in% c(MODEL_INDEX))

FORM <- customer + ACCT_ID + SA_ID + model_index + model_name + climate_zone_cd + usage_type ~ month_group
FORM <- PREM_ID + ACCT_ID + SA_ID + model_index  ~ month_group
billing_merge_wide <- dcast.data.table(data = billing_merge
                                       , formula = FORM
                                       , value.var = c(WIDE_COLS)
                                       , drop = TRUE,sep = '')
billing_merge_wide[,lapply(.SD, function(x) sum(is.na(x))),]

billing_merge_wide[is.na(PDT__1), PDT__1:= as.IDate('1900-01-01')]
billing_merge_wide[is.na(PDT__2), PDT__2:= as.IDate('1900-01-01')]
billing_merge_wide[is.na(CDT__1), CDT__1:= as.IDate('1900-01-01')]
billing_merge_wide[is.na(CDT__2), CDT__2:= as.IDate('1900-01-01')]
billing_merge_wide[is.na(CSCH__1), CSCH__1:= '0']
billing_merge_wide[is.na(CSCH__2), CSCH__2:= '0']
billing_merge_wide[is.na(EMP1), EMP1:= '0']
billing_merge_wide[is.na(EMP2), EMP2:= '0']
billing_merge_wide[is.na(BILL_CYCLE1), BILL_CYCLE1:= '0']
billing_merge_wide[is.na(BILL_CYCLE2), BILL_CYCLE2:= '0']

set_zero(billing_merge_wide) # set zero

# billing_merge_wide[,lapply(.SD, function(x) sum(is.na(x))), by = .(model_index)] check for na

PATH <- paste0(OUT_LOC,names(TIERED_USAGE),'.csv') # create file path
walk2(billing_merge_wide %>% split(.$model_index), 
      PATH, write.csv, row.names = FALSE) # print out files using split



# PATH1 <- paste0('images/',names(plots_sm),'.png')
# 
# PATH2 <- flatten_chr(map2(list(paste0('images/',names(plots_sm))), c('.pdf','.png'), paste0))
# 
# PATH3 <- map2(list(paste0('images/',names(plots_sm))), c('.pdf','.png'), paste0)

# experimental ------------------------------------------------------------

# subset and print without split

PATH <- paste0(OUT_LOC,names(TIERED_USAGE),'.csv') # create file path
?subset()

split(billing_merge_wide$model_index)
split(x = billing_merge_wide, billing_merge_wide$model_index)
subset(x = billing_merge_wide, model_index == MODEL_INDEX[1])

walk2(billing_merge_wide %>% split(.$model_index), 
      PATH, write.csv, row.names = FALSE) # print out files using split

MODEL_INDEX
sub_var <- 'model_index'
model_subset <- function(DT, mod_index){
  subset(x = DT, model_index == mod_index)
  }


dt <- data.table(mtcars)[,.(cyl, mpg)]
myfunc <- function(dt, v){
  v2=deparse(substitute(v))
  dt[,v2, with = F]
}

myfunc <- function(dt, sub_var){
  v2=deparse(substitute(sub_var))
  mod = unique(dt[,v2, with = F])
  mod
  # subset(dt, sub_var == 'baseline_2015_predicted')
}[,model_index]

md <- myfunc(billing_merge_wide, model_index)
str(md)

md[,model_index]

model_subset(DT = billing_merge_wide, mod_index = MODEL_INDEX[2])
model_subset(DT = billing_merge_wide, sub_var = 'model_index')
  
  subset(dt, a == 'a')

# norm <- billing_list %>% 
#   map(~dcast.data.table(.,formula = FORM
#                         , value.var = c(WIDE_COLS)
#                         , drop = TRUE,sep = '')
#   )

# norm_2 <- norm %>% 
#   map(rbindlist(.) # set zero
#   )

# map(.x = norm_2, )

# num_cols <- colnames(norm_one)[which(norm_one[,lapply(.SD, is.numeric)] == TRUE)]
# date_cols <- colnames(norm_one)[which(norm_one[,lapply(.SD, is.Date)] == TRUE)]
# factor_cols <- colnames(norm_one)[which(norm_one[,lapply(.SD, is.factor)] == TRUE)]
# char_cols <- colnames(norm_one)[which(norm_one[,lapply(.SD, is.character)] == TRUE)]



# walk2(PATH2, full_plots, ggsave, width = 11, height = 8.5)  # works
# walk2(PATH3, full_plots, ggsave, width = 11, height = 8.5)  # fail
# pwalk(list(filename = PATH2, plot = full_plots), ggsave, width = 11, height = 8.5) # works