# Data preparation script
gc()
# rm(list = ls())
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

# Read in sample to determine column classes ------------------------------
gc()

interval_files <- col_types(dir = '../CEA_DATA/interval_data/gas/'
                         # , pat = 'wt'
                         , pat = 'no_station'
                         , sep = '\t'
                         , n = 5
                         , fctr_cols = c('prem_id')
                         # , char_cols = CHAR_COLS
                         # , num_cols = c('daily_uccf_usage', 'daily_cccf_usage','daily_thrm_usage')
                         )
weather_files <- col_types(dir = '../CEA_DATA/weather_data/'
                         , pat = '2008'
                         , sep = '\t'
                         , n = 5
                         # , char_cols = CHAR_COLS
                         , fctr_cols = 'opr_area_cd'
                         # , num_cols = c('daily_uccf_usage', 'daily_cccf_usage','daily_thrm_usage')
                         )

dem_files <- col_types(dir = '../CEA_DATA/demograhic_data/'
                       , sep = '\t'
                         , pat = '.txt'
                         , fctr_cols = c('prem_id','acct_id'))

# Read training and test interval data -----------------------------
# read in pivot data - all prem_id with full interval data
gc()
# rm(interval_usage)
interval_usage <- fread('../CEA_DATA/interval_data/gas/interval_usage_wt_2014_2015.txt', header = TRUE
        , drop = c('start_date', 'stop_date', 'Meter Id', 'daily_end'
                   , 'daily_uccf_usage','daily_cccf_usage', 'Dir Source System Code')
        , strip.white = TRUE
        , check.names = TRUE
        , stringsAsFactors = TRUE
        , integer64 = 'character'
        , verbose = FALSE
        # , colClasses = int_classes[['classes']]
        , na.strings = '?'
        , sep = '\t')

interval_usage[is.na(daily_thrm_usage),daily_thrm_usage:=0] # remove na from usage
# interval_usage[is.na(daily_thrm_usage),] # remove na from usage
# interval_usage[is.na(sa_id),] # remove na from usage

setnames(interval_usage, gsub(pattern = ' ', replacement = '_'
                              , x = colnames(interval_usage)))

setnames(interval_usage, old = c('usage_date', 'daily_thrm_usage')
         , new = c('date', 'usage_actual'))

interval_usage[,date := as.IDate(date, format = '%m/%d/%Y')]

interval_usage <- add_time(interval_usage)

outlier_usg <- as.character(interval_usage[(usage_actual > 20)][,unique(prem_id)]) #identify heavy users
# full_obs <- as.character(interval_usage[,.N, by = .(prem_id, year)][N >= 365][,unique(prem_id)])

# interval_usage[,sa_count:=uniqueN(sa_id), by = .(prem_id, sp_id, year)]
# interval_usage[,day_count :=.N, by = .(prem_id, sp_id, year)]
# interval_usage[day_count > 365,]
# interval_usage[,monthly_count :=.N, by = .(prem_id, sp_id, year)]
# interval_usage_summary <- interval_usage[, .(usage_days = .N), by = .(prem_id, sp_id, sa_id, year)
#                                          ][order(prem_id, usage_days)]
# 
# interval_usage_summary[,max_usage_days:=lapply(.SD,  max), .SDcols = 'usage_days'
#                        , by = .(year)]
# 
# interval_usage_summary[usage_days == 433,]
# interval_usage_summary[prem_id == '4322533128',]
# interval_usage[prem_id == '4322533128' & month_name == 'Dec'
#                & year == 2013,.N, by = .(prem_id, sa_id),]

# 
# 

# interval_usage[month_name %in% c('Dec','Mar'),.N, by = .(prem_id, usage_quantile)][order(prem_id)][N!=124]
# interval_usage[prem_id == '0064814705' & month_name %in% c('Dec','Mar'),]
# interval_usage[monthly_count != month_days & month_name %in% c('Dec','Mar'),]
# 
# interval_usage[,min(day_count), by = prem_id%in% c(full_obs)]
# interval_usage[day_count == 2, ]
# interval_usage[sp_id == '6069922505', ]

interval_usage <- interval_usage[season == 'winter',] # winter observations
interval_usage[prem_id == '6967523191',.N, by = .(prem_id, sp_id)]
               
interval_usage[prem_id == '6967523191',.N, by = .(prem_id, sp_id, year)]

setkey(interval_usage, prem_id, sp_id, date)
interval_usage[prem_id == '0023158471',uniqueN(sa_id), by = .(prem_id, year)]
interval_usage[prem_id == '0023158471',]

# Read demographic data ---------------------------------------------------

# cross reference for model population
CROSS_REF <- grep(pattern = 'gua_cross_ref', x = dem_files[['files']])
cross_ref <- rbindlist(map(dem_files[['files']][CROSS_REF],
                  function(x) fread(x,
                                    header = 'auto',
                                    sep='auto',
                                    stringsAsFactors = TRUE,
                                    na.strings = '?',
                                    integer64 = 'character',
                                    colClasses = c(sa_change = 'numeric'),
                                    showProgress = FALSE)
))

cross_ref[, colnames(cross_ref)[1:8]:=lapply(.SD, as.factor),.SDcols = colnames(cross_ref)[1:8]]
cross_ref[, customer:=as.factor(paste0(prem_id,'_',acct_id)),]
setkey(cross_ref, climate_zone_cd, opr_area_cd)

cross_ref[prem_id == '6967523191',]
# cross_ref <- cross_ref[rent_cd == 'CUS_OCC',] #only

UNIQUE_OPR_CD <- cross_ref[climate_zone_cd %in% c(CLIMATE_ZONES),
                           ][,as.character(unique(opr_area_cd))]
UNIQUE_CZ_CD <- cross_ref[,as.character(unique(climate_zone_cd))]


# Read WEATHER DATA -------------------------

wea <- rbindlist(map(weather_files[['files']][1]
                            , function(x) fread(x
                                                , stringsAsFactors = TRUE
                                                , na.strings = '?'
                                                , integer64 = 'numeric'
                                                , drop = c('wea_year', 'wea_month','wea_day'))))

wea[,wea_dt := as.IDate(wea_dt, format = '%m/%d/%Y')]
setkey(wea, wea_dt)
wea <- wea[wea_dt %between% c(LOWER,UPPER)] # limit data
wea <- wea[!is.na(avg_val),] # remove missing values
wea <- wea[wea_type != 'Normal Hourly Temperature Stan',] # remove standard deviation
setkey(wea, opr_area_cd)
wea <- wea[(UNIQUE_OPR_CD)] # only work with wea for sample population

# set names
setnames(wea, old = colnames(wea)
         , new = gsub(pattern = 'wea_', replacement = '',x = colnames(wea)))
setnames(wea, old = c('dt'), new = 'date')
wea[, type := as.factor(tolower(gsub(pattern = ' ', replacement = '_', type, )))]
wea <- wea[type == 'temperature', type := 'temp_actual',]
wea <- wea[type == 'normal_hourly_temperature', type := 'temp_normal',]

# daily average of weather type within opr_area_cd (may have multiple weather stations)
setkey(wea, date, opr_area_cd, type)
wea <- wea[,lapply(.SD, mean, na.rm = TRUE)
                   , by = key(wea)
                   , .SDcols = 'avg_val']
wea <- add_time(wea)


# Merge Demographic Data and Interval Data --------------------------------------------------------------
gc()

setkey(cross_ref, prem_id) # set key for merge
setkey(interval_usage, prem_id)

interval_usage <- merge(interval_usage # add climate_zone_cd, opr_area_cd, and lat/lon to usage
                        , cross_ref[climate_zone_cd %in% CLIMATE_ZONES,.(customer, acct_id, prem_id, opr_area_cd, climate_zone_cd, rent_cd)]
                        , allow.cartesian = TRUE)

setkey(interval_usage, customer) # set key for merge

# customers with full obs
interval_usage[,outlier := prem_id %in% outlier_usg] # tag outliers for later analysis
interval_usage[,.N, by = .(customer)][order(-N)]


# FULL_CUSTOMERS <- FULL_OBS[,unique(customer)] # full customers from actuals
# interval_usage <- interval_usage[customer %in% c(FULL_CUSTOMERS),]

# merge Demographic Data and Weather data-----------------------------------------------------------
setkey(wea, opr_area_cd) # set key for merge
setkey(pop_tenure, opr_area_cd)
setkey(cross_ref, opr_area_cd)

wea <- merge(wea, cross_ref[,unique(pop_tenure)][,.(opr_area_cd, climate_zone_cd)]
                 , allow.cartesian = TRUE)

weather_wide <- dcast.data.table(data = wea[climate_zone_cd %in% CLIMATE_ZONES & season == 'winter',] # cast wide for modeling
                                 , formula = opr_area_cd 
                                 + climate_zone_cd + date
                                 ~ type
                                 , value.var = c('avg_val'))

weather_wide[,c('hdd_actual','hdd_norm'):=lapply(.SD, get_hdd, 65)
             , .SDcols = c('temp_actual','temp_normal')] # add hdd
gc()

weather_wide <- add_time(weather_wide) # add time variables for merging and lag terms

# TEMP_LAG <- dcast.data.table(data = weather_wide[year %between% c(year(LOWER),year(UPPER)-1)]
#                              , formula  = opr_area_cd + year_day ~ .,value.var = c('temp_actual')
#                              , fun.aggregate = c(max, mean, median, min), na.rm = TRUE)
# 
# setnames(x = TEMP_LAG, old = colnames(TEMP_LAG)[-c(1:2)]
#          , new = c('MAX_5','MEAN_5','MEDIAN_5','MIN_5'))
# 
# setkey(TEMP_LAG, opr_area_cd, year_day)

# create lag terms for usage and weather ----------------------------------

# change train indicator to match year
# weather_wide <- weather_wide[year %in% c(TRAIN_YEAR,TEST_YEAR)] # limit data

weather_wide[,train_indicator := as.factor(ifelse(year !=  TEST_YEAR, 'TRAIN'
                                        , ifelse(year == TEST_YEAR, 'TEST'
                                                 , 'HISTORIC'))),] 

weather_wide[,.N, by = .(train_indicator, year)]

interval_usage[,train_indicator := as.factor(ifelse(year !=  2015, 'TRAIN'
                                                    , ifelse(year == 2015, 'TEST'
                                                             , 'HISTORIC'))),] 
interval_usage[,.N, by = .(month_name, customer)][order(N, customer)]
interval_usage[customer == '4322533128_2155249480',]
interval_usage[customer == '4322533128_2155249480',.N, by = .(month_name, year)][order(year)]
# create baseline data tableval[]
baseline_usage <- copy(interval_usage)

# baseline_usage <- dcast.data.table(data = weather_wide[year < TEST_YEAR,], formula  = opr_area_cd + year_day ~ .,value.var = c('temp_actual')
#                              , fun.aggregate = c(median), na.rm = TRUE)

# set keys for lag terms 
setkey(weather_wide, opr_area_cd, year_day)
setkey(interval_usage, customer, year_day)
setkey(baseline_usage, customer, year_day)

# temperature lags --------------------------------------------------------
# out of sample (TEST) for MEDIAN_5 terms uses actual temperature with lags
# weather_wide <- merge(weather_wide, TEMP_LAG)

# in sample (TRAIN) for all models uses actual temperature from train year
weather_wide[train_indicator == 'TRAIN'
             , c('TEMP_NORMAL') := (.SD)
             , .SDcols = 'temp_actual', by = key(weather_wide)] 

# out of sample (TEST) for TEMP_NORMAL uses normal temperature from test year
weather_wide[train_indicator != 'TRAIN'
             ,c('TEMP_NORMAL') := (.SD)
             , .SDcols = 'temp_normal', by = key(weather_wide)] 

# USAGE LAGS --------------------------------------------------------------

# in sample (TRAIN) and out of sample (TEST) for all models 
# uses actual usage from that year
interval_usage[,c('TEMP_NORMAL') := (.SD)
               , .SDcols = 'usage_actual', by = key(interval_usage)]

interval_usage[,c('usage_prior') := shift(.SD, 1, 'lag')
               , .SDcols = 'usage_actual', by = key(interval_usage)]

# constant usage (BASELINE) uses TRAIN year actual usage as TEST year prediction 
baseline_usage[,c(BASE_COLS) := shift(.SD, 1,  'lag')
               , .SDcols = 'usage_actual', by = key(baseline_usage)]


# remove any historic / lag data
weather_wide <- weather_wide[train_indicator != 'HISTORIC'] 
baseline_usage[year == TEST_YEAR,train_indicator:='BASELINE']
baseline_usage <- baseline_usage[train_indicator == 'BASELINE',]
# baseline_usage[prem_id == '0008117489' & year_day == 60, ]
# interval_usage[customer %in% c(bad_cust[1]) & year == 2014,]
bad_cust <- as.character(baseline_usage[is.na(CONS_USG) & year == 2015,.N, by = .(customer)][,customer])
baseline_usage[is.na(CONS_USG),CONS_USG:=0, ] # rename missing values to 0

# melt tables to long format ----------------------------------------------

# melt temperature data
weather_mlt <- melt.data.table(weather_wide,
                               id.vars = c('opr_area_cd','date','climate_zone_cd','train_indicator',
                                           'month_name', 'year', 'month_day','year_day','hdd_actual','hdd_norm'
                                           ,'temp_actual', 'temp_normal')
                               , measure.vars = c('TEMP_NORMAL')
                               , variable.name = 'model_name'
                               , value.name = 'temp')

# melt interval usage
interval_usage_mlt <- melt.data.table(interval_usage
                                      , measure.vars = c('TEMP_NORMAL','usage_actual')
                                      , variable.name = 'model_name'
                                      , value.name = 'usg')

interval_usage_mlt[prem_id == '0007958141' & year_day == 1,]
baseline_usage <- melt.data.table(baseline_usage
                                      , measure.vars = c(BASE_COLS)
                                      , variable.name = 'model_name'
                                      , value.name = '.pred')

gc()

# create hdd data ---------------------------------------------------------

# melt weather for hdd actual and normal
hdd_dt <- melt.data.table(weather_mlt[model_name == 'TEMP_NORMAL',],
                          id.vars = c('year','month_name', 'year_day', 'date'
                                      , 'opr_area_cd', 'climate_zone_cd')
                          , measure.vars = c('hdd_actual','hdd_norm')
                          , variable.name = paste('hdd_type')
                          , value.name = 'hdd_65')

hdd_dt[, hdd_type := as.factor(paste0(hdd_type,'_',year))]

# set key to aggregate down to one value per day, per hdd type, per opr area cd
setkey(hdd_dt, year, month_name, year_day, date, climate_zone_cd, opr_area_cd, hdd_type)
hdd_dt <- hdd_dt[,unique(hdd_dt)]
hdd_dt <- hdd_dt[,lapply(.SD, mean), by = .(month_name, year_day, date, climate_zone_cd, hdd_type)
      ,.SDcols = 'hdd_65']
hdd_dt <- hdd_dt[,lapply(.SD, sum), by = .(month_name, climate_zone_cd, hdd_type)
      ,.SDcols = 'hdd_65']

# merge temp and usage by location and model type ---------------------------------------------------------------

setkey(interval_usage_mlt, date, opr_area_cd, model_name)
setkey(weather_mlt, date, opr_area_cd, model_name)
setkey(baseline_usage, customer, date, opr_area_cd, model_name)

# merge usage and weather data for modeling
train_test <- merge(interval_usage_mlt
                       , weather_mlt[,c(WEA_COLS), with = FALSE])
setkey(train_test, customer, date, opr_area_cd, model_name)

# make sure everything is a factor
train_test[,c('season','train_indicator', 'month_name'):=lapply(.SD, as.factor)
               , .SDcols = c('season','train_indicator','month_name')]
baseline_usage[,c('customer','season','train_indicator', 'month_name'):=lapply(.SD, as.factor)
               , .SDcols = c('customer','season','train_indicator','month_name')]
hdd_dt[,c('month_name','climate_zone_cd','hdd_type'):=lapply(.SD, as.factor)
               , .SDcols = c('month_name','climate_zone_cd','hdd_type')]

hdd_dt$month_name <- factor(hdd_dt$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

# rm(wea);rm(weather_wide);rm(interval_usage);rm(weather_mlt);rm(interval_usage_mlt)
# gc()
# 
summary(train_test)
write.table(x = train_test, file = '../CEA_DATA/weather_data/train_test.txt'
          , row.names = FALSE)
write.table(x = baseline_usage, file = '../CEA_DATA/weather_data/baseline_usage.txt'
          , row.names = FALSE)
write.table(x = hdd_dt, file = '../CEA_DATA/weather_data/hdd_dt.txt'
          , row.names = FALSE)
# 
rm(train_test);rm(baseline_usage);rm(hdd_dt)

