# Prepare usage simulations for bill calculation --------------------------
library(dplyr)
library(data.table)

MONTH_COLS <- 1:12
MON_NUM <- sprintf('%02d', match(c('Nov','Dec','Jan','Feb','Mar'), month.abb))
WIN_MONTHS <- flatten_chr(map2(list(TEST_YEAR, TRAIN_YEAR), list(MON_NUM),paste0))
CAST_COLS <- c(make_cols(x = c('CSCH__','THM__', 'TR1__','TR2__','DAY__','PDT__', 'CDT__','BASAMT'
                               ,'EMP__','REV__','REV__S','REV__W'), MONTH_COLS, paste0), 'LNUMUNIT','NUMUNIT'
               , 'CSCHED', 'CATFLAG')
BASE_COLS <- make_cols(x = c('BASAMT'), MONTH_COLS, paste0)
BASE_COLS <- make_cols(x = c('BASAMT'), MONTH_COLS, paste0)
CDT_COLS <- make_cols(x = c('CDT__'), MONTH_COLS, paste0)
PDT_COLS <- make_cols(x = c('PDT__'), MONTH_COLS, paste0)
KEEP_COLS <- c('SA_ID','PREM_ID','ACCT_ID','customer','CSCH','THM','CATFLAG','CSCHED')
FORMULA <- c('PDT__','CDT__')

FORM <- KEEP_COLS
write.csv(x = BILL_COLS, file = '../../Documents/narrow.csv',row.names = FALSE)

BILL_COLS <- c('SERVICE AGREEMENT ID' # SA_ID: required
                   , 'PREMISE ID' # PREM_ID: required
                   , 'ACCOUNT ID' # ACCT_ID: required
                   , 'COMPRESSED RATE SCHED' # CSCH__: required
                   , 'USAGE' # THM: required, reconcile 
                   , 'NUMBER OF DAYS' # DAY: required
                   , 'PRIOR READ DATE' # PDT__: required
                   , 'CURRENT READ DATE' # CDT__: required
                   , 'SMARTMETER STATUS CODE' # CATFLAG; required, rceconcile
                   , 'RATE SCHEDULE (CDX)' # CSCHED: required
                   , 'NUMBER OF SUMMER DAYS' # LNUMUNIT; required, rceconcile 
                   , 'NUMBER OF UNITS' # NUMUNIT; required
                   , 'EMPLOYEE CODE' # EMP; required
                   , 'BASELINE AMOUNT' # BASAMT :required for calculation
                   , 'BALANCE PAYMENT PLAN INDICATOR' # BPP
                   , 'SPECIAL PAY PLAN INDICATOR' # SPP
                   , 'CARE INDICATOR' # CARE: not required
                   , 'BILL CYCLE') # BILL_CYCLE: not required

BILL_RENAME <- c(
  'SA_ID'
  , 'PREM_ID'
  , 'ACCT_ID'
  , 'CSCH_' ##
  , 'THM' # THIS NEEDS TO BE RECONCILED
  , 'DAY' #: 
  , 'PDT_' # 
  , 'CDT_' #
  , 'CATFLAG' # THIS NEEDS TO BE RECONCILED
  , 'CSCHED' # :required
  , 'LNUMUNIT' # THIS NEEDS TO BE RECONCILED
  , 'NUMUNIT'
  , 'EMP'
  , 'BASAMT'
  , 'BPP'
  , 'SPP' ##
  , 'CARE'
  , 'BILL_CYCLE') # not required

BILL_MERGE <- c('customer',BILL_RENAME)

NUM_PATTERN <-  '[^A-Z]+'

format_bills <- function(){
  
  usage_cols <- col_types(dir = '../CEA_DATA/model_output/', pat = '.txt', n = 10, fctr_cols = 'customer')
  
  billing_cols <-col_types('../CEA_DATA/billing_data/', pat = '.txt', n = 5, sep ='\t'
                            , fctr_cols = c('BILL CYCLE','SMARTMETER STATUS CODE','SERVICE AGREEMENT ID'
                                            ,'ACCOUNT ID','PREMISE ID','SERVICE POINT ID','UNIQUE SERVICE AGREEMENT ID'))
  
  template_cols <- as.data.table(read_sas('../CEA_DATA/billing_data/w_res2015.sas7bdat')
                            ,b7cat = NULL)
  

  # Read in billing input and format
  usage <- map(usage_cols[['files']], function(x){ # read pivot data into list of data tables
    fread(x, header = TRUE
          , strip.white = TRUE
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , verbose = FALSE
          , colClasses = usage_cols[['classes']]
          , na.strings = '?')
  })
  
  names(usage) <- c('BASELINE','PREDICTIONS','TRAIN')
  
  # load train, test and baseline data
  usage_train_test <- rbindlist(usage[c("PREDICTIONS","TRAIN","BASELINE")])
  usage_train_test[season == 'winter', season := 'W',]
  
  # usage_baseline <- rbindlist(usage[c("BASELINE")])
  # find unique customers 
  unique_customers <- usage_train_test[,unique(customer)]
  
  # read in billing determinants
  bill_dtrm <- map(billing_cols[['files']], function(x){ # read pivot data into list of data tables
    fread(x, header = TRUE
          , strip.white = TRUE
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , verbose = FALSE
          , select = c(BILL_COLS)
          , colClasses = baseline_cols[['classes']]
          , na.strings = '?')
  })
  
  bill_dtrm <- rbindlist(bill_dtrm)
  setcolorder(bill_dtrm, BILL_COLS)
  setnames(bill_dtrm, old = colnames(bill_dtrm), new = BILL_RENAME)

  # baseline[, c('PREM_ID','ACCT_ID','SA_ID') := lapply(.SD, function(x) factor(gsub(pattern = ' '
  #                                                                               , replacement = '0', x = sprintf('%010s', x)))), .SDcols = c('PREM_ID','ACCT_ID','SA_ID')]
  # create customer variable
  bill_dtrm[,customer := paste0(PREM_ID,'_',ACCT_ID)]
  
  # set key for filter and variable creation
  setkey(bill_dtrm, customer, PDT_)
  bill_dtrm <- bill_dtrm[customer %in% unique_customers,] # limit to sample
  bill_dtrm[,c('CDT_','PDT_') := lapply(.SD, function(x)
    lubridate::round_date(as.IDate(x, format = '%m/%d/%Y'), unit = 'month'))
    , .SDcols = c('CDT_','PDT_')]
  
  # add time variables and limit to winter
  bill_dtrm[,date := PDT_,]
  bill_dtrm <- add_time(dt = bill_dtrm)
  bill_dtrm <- bill_dtrm[season == 'winter' & year %in% c(TRAIN_YEAR, TEST_YEAR),]
  bill_dtrm[,date := NULL]
  
  bill_dtrm[,train_indicator := ifelse(year == TRAIN_YEAR,'TRAIN', ifelse(year == TEST_YEAR,'TEST',
                                                                           'out of sample')),]
  
  setkey(bill_dtrm, customer, month, year) # set key for merge
  t(head(bill_dtrm[BILL_RENAME], 1))

# Merge ---------------------------------------------------------
  
  setkey(usage_train_test, customer)
  
  # aggregate predictions
  bill_dtrm_cust <- bill_dtrm[,unique(customer)]
  usage_train_test <- usage_train_test[customer %in% bill_dtrm_cust,]
  usage_train_test[,date:=as.IDate(date, format = '%Y-%m-%d')] # correct date format
  usage_train_test[,PDT_ := lapply(.SD, head, 1), by = .(month_name, train_indicator, year, season), .SDcols = 'date']

  usage_train_test <- usage_train_test[,lapply(.SD, sum), 
                                             by = .(train_indicator, year, customer, model_name, season, month, month_name, PDT_)
             ,.SDcols = c('.pred','usage_actual')]
  
  usage_train_test_sm <- usage_train_test[customer == '0008117489_9935171478' & train_indicator != 'BASELINE',] # subset

  usage_train_test_mlt <- melt.data.table(usage_train_test_sm
                                          , measure.vars = c('.pred','usage_actual')
                                          , variable.name = paste0('usage_type')
                                          , value.name = 'usage')
  
  # set keys to merge 
setkey(usage_train_test_mlt, customer, PDT_)
setkey(bill_dtrm, customer, PDT_)
# baseline <- baseline[customer == '0008117489_9935171478',]

billing_merge <- merge(x = usage_train_test_mlt, 
                         y = bill_dtrm[,(BILL_MERGE), with = FALSE], allow.cartesian = TRUE)
  
  # sprintf('%02d', match(c('Nov','Dec','Jan','Feb','Mar'), month.abb))
  template_cols[,(CAST_COLS), with = FALSE]
  summary(template_cols$PDT__1)
  template_cols[,.N, by = .()]
  
  # Determine billing tiers
  billing_merge[,`:=` ('TR1_' = tier(usage, BASAMT, 0, 1),
                   'TR2_' = tier(usage, BASAMT, 1, 100))]
  
  billing_merge <- billing_merge[usage_type == '.pred' & train_indicator == 'TEST',]
  
  billing_merge[,PDT__:=as.character(PDT_)]
  billing_merge[,CDT__:=as.character(CDT_)]
  setkey(billing_merge, customer)
  FORM <- customer + train_indicator + model_name + usage_type + SA_ID + PREM_ID + ACCT_ID  + CATFLAG + CSCHED ~ month
colnames(billing_merge2)
  billing_merge2 <- dcast.data.table(data = billing_merge
                                     , formula = FORM
                          , value.var = c(paste0('TR',1:2,'_'),'PDT_','CDT_','CSCH_','DAY','EMP')
                          , drop = TRUE)
  
  ?dcast.data.table()
  
  str(billing_merge2$CDT___11)
  value_vars <- paste0('tier_',1:5,'_kwh')
  
  weather_wide <- dcast.data.table(data = wea[climate_zone_cd %in% CLIMATE_ZONES] # cast wide for modeling
                                   , formula = opr_area_cd 
                                   + climate_zone_cd + date
                                   ~ type
                                   , value.var = c('avg_val'))
  
  make_total <- c(colnames(select(billing_merge2, contains('TIER'))))
  MAKE_TOTAL <- billing_merge2 %>%
    select(matches('TIER')) %>% 
    colnames() %>% unique(billing_merge2) %>% 
    toupper() %>% 
    paste0('_THERMS')
  
  setnames(billing_merge2, old = billing_merge2 %>%
             select(matches('TIER')) %>% 
             colnames() %>% unique(billing_merge2), new = MAKE_TOTAL)
  
  billing_merge2[ , USAGE := rowSums(.SD, na.rm = TRUE), .SDcols = MAKE_TOTAL]
  
  new_cols <- paste(unname(sapply(make_total, function(x) substr(x, start = 12, stop = 1000000)))
                    , unname(sapply(make_total, function(x) substr(x, start = 1, stop = 10))), sep = '_')

  setnames(billing_input, old = make_total, new = new_cols)
  
  # Merge back in to template (why???)
  template <- template[,.SD, .SDcols =!c('summer_tier_1_kwh', 'winter_tier_1_kwh', 
                                         'summer_tier_2_kwh', 'winter_tier_2_kwh', 
                                         'summer_tier_3_kwh', 'winter_tier_3_kwh', 
                                         'summer_tier_4_kwh', 'winter_tier_4_kwh', 
                                         'summer_tier_5_kwh','winter_tier_5_kwh',
                                         'tot_usg_kwh', 'baseline_usg_kwh')]
  
  make_blank <- unique(c(colnames(select(template, contains('summer'))), 
                         colnames(select(template, contains('winter'))), 
                         colnames(select(template, contains('tier')))))
  
  template[,c(make_blank) := as.integer(make_blank), with = FALSE]
  
  template[is.na(template)] <- '' # NA as blank
  
  billing_input <- merge(x = billing_input, 
                         y = template, 
                         by.x = c('acct_id','prem_id','nrml_yyyymm'), 
                         by.y = c('acct_id', 'char_prem_id', 'nrml_yyyymm'))
  
  # One last column name change
  setnames(billing_input, old = 'prem_id', new = 'char_prem_id')
  
  # Check that the column names match up
  missing_cols <- correct_col_names[!(correct_col_names %in% colnames(billing_input))]
  if(length(missing_cols) > 0){
    stop('There are missing columns in output file: ', missing_cols)    
  } 
  return(billing_input)
}

billing_input <- format_bills()

write.table(billing_input,paste0(OUT_LOC, OUT_NAME),row.names = FALSE, quote = TRUE)

print('Script Complete')
print(proc.time() - start_time)