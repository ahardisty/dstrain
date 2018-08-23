

# assign constants --------------------------------------------------------

# data sources and locations
# out location for train_test interval usage files (for prediction)
TRAIN_TEST_OUT_LOC <- 'E:/CEA_DATA/interval_data/electric/period_shift/' 
SAMPLE_DIR <- 'E:/CEA_DATA/interval_data/electric/hourly_by_sp_id/interval_14_15/'
SAMPLE <- list.files('E:/CEA_DATA/interval_data/electric/hourly_by_sp_id/interval_14_15/')

# out location for baseline prediction files (predicted by constant usage method)
BASELINE_OUT_LOC <- 'E:/CEA_DATA/model_export/shifted_tou_predictions/' 

# get sample sp_id from individual samples --------------------------------------------------------
# find sample
## need to do a replace == FALSE by checking prior predictions
SAMPLE_SIZE <- 4000
SAMPLE_SP_ID <- tibble::as_tibble(SAMPLE) %>%
  mutate(sp_id = gsub(pattern = '.csv',replacement = '',value)) %>% 
  dplyr::sample_n(SAMPLE_SIZE) %>% 
  select(value) %>%
  distinct(value) %>% 
  flatten_chr() 

x <- paste0(SAMPLE_DIR, SAMPLE_SP_ID)

# shift usage data ---------------------------------------------------------
# load usage for 8760 per sp_id 2014 & 2015
usg <- map(.x = x, .f = fread, header = TRUE, sep = 'auto'  
           , check.names = TRUE
           , stringsAsFactors = TRUE
           , integer64 = 'character'
           , na.strings = '?'
           , colClasses = list(factor = c('sp_id','month'),
                               numeric = c('hour','usage'),
                               character = c('date')),
           , verbose = FALSE) %>% rbindlist()

usg[,date := as.IDate(date),] # as date format
## need to change hours to ordered factor
usg[,hour:= as.numeric(hour)-1] # as date format
usg[,usage:=as.numeric(usage)]

setkey(usg,date,sp_id,hour) # set key for unique values
add_time(usg) # add date/time elements
setnames(usg, old = 'year_day', new = 'TOU_standard') # rename year_day to verify shift
usg[,TOU_standard:=as.numeric(TOU_standard)] # as numeric
usg[,day_name:=lubridate::wday(date, label = TRUE)] # get day name to verify shift
usg[,day:=lubridate::wday(date)] # week day number 1-7 (numeric duplicate of day_name)

# usg[,uniqueN(sp_id)]  

# create adjusted shift dates to correct year over year day shift
usg[year == TRAIN_YEAR,shift_date:= as.IDate(date -1)] # should make a year shift not day shift
usg[year == TEST_YEAR,shift_date:= as.IDate(date)]
usg[year == TRAIN_YEAR,TOU_shift:= as.numeric(yday(shift_date))]
usg[year == TEST_YEAR,TOU_shift:= as.numeric(yday(shift_date))]

usg <- melt.data.table(data = usg, 
                       , measure.vars = c('TOU_shift') # only melting TOU_shift
                       # , measure.vars = c('TOU_shift','TOU_standard')
                       , variable.name = 'model_name'
                       , value.name = 'year_day'
                       , na.rm = TRUE)
usg[,TOU_standard:= NULL]
# usg <- subset(usg, model_name == 'TOU_shift') # only TOU_shift

# # the only mismatch is day 365, will reconcile
# setkey(usg, date, model_name)
# check_shift <- usg[,unique(usg)]
# check_shift <- dcast.data.table(check_shift, formula = year_day + model_name ~ year, value.var = 'day_name')
# check_shift[`2014`!=`2015`,.N, by = .(model_name)] # only 365 for TOU_shift

# add period formula variables
usg[, weekend := day_name%in%c('Sat','Sun')] # weekend true false (per model name)

usg[, holiday := date%in%c(HOLIDAYS)] # holiday true false (per model name)

gc()
# Get the weather station data
DT_Dem <- fread('E:/CEA_DATA/demographics_data/electric/eua_cross_ref.txt',
                header = 'auto',
                sep='auto',
                integer64='character',
                na.strings = '?',
                stringsAsFactors = FALSE,
                showProgress = FALSE )

weather_station_xref <- fread('E:/CEA_DATA/weather_data/weather_station_xref.txt',
                              header = 'auto',
                              sep='auto',
                              integer64='character',
                              na.strings = '?',
                              stringsAsFactors = FALSE,
                              showProgress = FALSE )

DT_Dem[,customer := as.factor(paste0(prem_id,'_',acct_id))] # create custome
setkey(DT_Dem, sp_id)# set key for filter
DT_Dem <- DT_Dem[,unique(DT_Dem)]# only unique sp_id - drops data, need to reconcile

setkey(usg, sp_id) # set key for merge
setkey(DT_Dem, sp_id) # set key for merge
usg <- merge(usg, DT_Dem[,.SD, .SDcols = DEM_COLS]) # merge for weather station data
setkey(usg, opr_area_cd) # set key for merge
setkey(weather_station_xref, opr_area_cd) # set key for merge
usg <- merge(usg, weather_station_xref[wea_stn_owner == 'PGE',.(weather_station, opr_area_cd)]) # merge for weather station data
# usg[,uniqueN(customer)]
# usg[,uniqueN(sp_id)]
rm(DT_Dem)
# Change to factor for space
usg[, `:=`(sp_id = factor(sp_id)
           , prem_id = factor(prem_id)
           , acct_id = factor(acct_id)
           , pty_id = factor(pty_id)
           , opr_area_cd = factor(opr_area_cd)
           , climate_zone_cd = factor(climate_zone_cd)
           , weather_station = factor(weather_station))]
gc()
setkey(usg, date, model_name, hour)
usg[, period := get_period(TOU_CURRENT, season, weekend, hour, holiday)] # assign TOU period to all hours
usg[,`:=` (measure_type = 'CONS_USG', # placeholder to create baseline usg (no temp scenario)
           nrml_yyyymm = as.character(sprintf('%s%02d', year, month)),
           temperature = 0L)] # bill period

# usg[,uniqueN(customer)]
# sample_customers <- usg[,lapply(.SD, sample, 2), .SDcols = 'customer', by = .(climate_zone_cd)][,as.character(customer)]
# usg <- subset(usg, customer %in% sample_customers) 
# Create constant usage model ---------------------------------------------
gc()
# copy hourly usage to get correct year over year usage using TOU shift
# lag shift to get usage at same hour prior year

baseline_tou_hourly <- subset(usg, year == TEST_YEAR & model_name == 'TOU_shift') # subset on test data only
baseline_tou_hourly_train <- subset(usg, year == TRAIN_YEAR & model_name == 'TOU_shift') # subset on test data only

setkey(baseline_tou_hourly, customer, year_day, hour)
setkey(baseline_tou_hourly_train, customer, year_day, hour)

baseline_tou_hourly <- merge(baseline_tou_hourly
                             , baseline_tou_hourly_train[,.(customer, year_day, usage, hour)]
                             , suffix = c(x = '_usage',y = '_predicted'))
setnames(baseline_tou_hourly, old = c('usage_usage','usage_predicted'), new = c('usage','predicted')) 
rm(baseline_tou_hourly_train)
# baseline_tou_hourly[, predicted:=shift(.SD, 1, 0, 'lag')
#                     , .SDcols = 'usage', by = key(baseline_tou_hourly)] 

# spot check for year over year lag shift
# baseline_tou_hourly[customer %in% as.character(baseline_tou_hourly[,sample(x = customer, size = 3)])
#                     & hour == 15
#                     & year_day == 220,]

setcolorder(baseline_tou_hourly, c("opr_area_cd", "sp_id", "date", "hour", "month", "year", 
                                   "month_day", "month_name", "season", "day_name", "day", "shift_date", 
                                   "model_name", "year_day", "weekend", "holiday", "customer", "prem_id", 
                                   "acct_id", "pty_id", "climate_zone_cd", "weather_station", "period", 
                                   "nrml_yyyymm", "measure_type","usage","predicted","temperature"))


# create output lists for shifted interval usage output -------------------------------------------------

# create baseline data list at TOU_shift daily and monthly level actual and predicted
# create path for writing files
# write baseline usage daily at TOU_shift 
# write baseline usage monthly at TOU_shift

BASELINE_DATA_LIST <- list(
  # baseline_tou_hourly = baseline_tou_hourly, # hourly forecasts are not required  
  
  baseline_tou_daily = baseline_tou_hourly[,.(usage = sum(usage), predicted = sum(predicted),.N)
                                           # , .SDcols = c('usage','predicted')
                                           , by = c(DAILY_BILLING_COLS),],
  
  baseline_tou_monthly = baseline_tou_hourly[,.(usage = sum(usage), predicted = sum(predicted),.N)
                                             # , .SDcols = c('usage','predicted')
                                             , by = c(MONTHLY_BILLING_COLS),]
  # this is for creating baseline model RMSE comparision
)

# create data list path
BASELINE_OUTPUT_PATH <- paste0(BASELINE_OUT_LOC,tolower(names(BASELINE_DATA_LIST)),'_',SAMPLE_SIZE,'.txt')
# write out data list to path
walk2(BASELINE_DATA_LIST, BASELINE_OUTPUT_PATH, write.table, row.names = FALSE, append = F)
rm(BASELINE_DATA_LIST)
rm(baseline_tou_hourly)

DROP_COLS <- c(which(DAILY_BILLING_COLS %in% c('measure_type', 'temperature'))) # remove placeholder variable
# create train_test interval usage at TOU_shift
# sum to tou periods on daily level
# remove baseline for train_test interval usage data
# this is the input for model test and train

TRAIN_TEST_DATA_LIST <- list(
  train_daily = usg[year == TRAIN_YEAR,.(usage = sum(usage, na.rm = TRUE), .N) # aggregate usage at TOU periods
                    , by = c(DAILY_BILLING_COLS[-(DROP_COLS)]),],
  
  test_daily = usg[year == TEST_YEAR,.(usage = sum(usage, na.rm = TRUE), .N) # aggregate usage at TOU periods
                   , by = c(DAILY_BILLING_COLS[-(DROP_COLS)]),] 
)
# create data list path
TRAIN_TEST_OUTPUT_PATH <- paste0(TRAIN_TEST_OUT_LOC,tolower(names(TRAIN_TEST_DATA_LIST)),'_',SAMPLE_SIZE,'.txt')
# write out data list to path
walk2(TRAIN_TEST_DATA_LIST, TRAIN_TEST_OUTPUT_PATH, write.table, row.names = FALSE, append = F)

rm(TRAIN_TEST_DATA_LIST)
rm(usg)


