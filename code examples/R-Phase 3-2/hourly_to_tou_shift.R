library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)

CLIMATE_ZONE <- 'w'

DEM_COLS <- c('prem_id','PREMI_cec_climate_zone_cd', 'PREMI_acct_id','sp_id','weather_station')
OUT_DIR_TEMP <- 'E:/CEA_DATA/interval_data/electric/shifted_tou/'
OUT_NAME_14 <- paste0('2014_tou_', CLIMATE_ZONE, '.txt')
OUT_NAME_15 <- paste0('2015_tou_', CLIMATE_ZONE, '.txt')
OUT_NAME_14_DAILY <- paste0('2014_daily_', CLIMATE_ZONE, '.txt')
OUT_NAME_15_DAILY <- paste0('2015_daily_', CLIMATE_ZONE, '.txt')



# Get the weatherstation data
dem_5rows <- read.table('E:/CEA_DATA/demographics_data/cea_demographics.txt'
                        , na.strings = '?'
                        , header = TRUE
                        , nrows = 5
                        , stringsAsFactors = TRUE)

dem_5rows[,c('prem_id', 'sa_id', 'sp_id','HOME_hm_htg_typ_cd','pty_id','PREMI_acct_id')] <- sapply(dem_5rows[,c('prem_id', 'sa_id', 'sp_id','HOME_hm_htg_typ_cd','pty_id','PREMI_acct_id')], as.character)
dem_5rows[,c('FUEL_coal_heat_pct', 'FUEL_solar_heat_pct','FUEL_kerosene_oil_heat_pct')] <- sapply(dem_5rows[,c('FUEL_coal_heat_pct', 'FUEL_solar_heat_pct','FUEL_kerosene_oil_heat_pct')], as.numeric)
dem_5rows[,c('HOME_hm_ac_typ_cd')] <- sapply(dem_5rows[,'HOME_hm_ac_typ_cd'], as.factor)
dem_classes <- sapply(dem_5rows, class)

DT_Dem <- fread('E:/CEA_DATA/demographics_data/cea_demographics.txt',
                                  header = 'auto',
                                  sep='auto',
                                  na.strings = '?',
                                  stringsAsFactors = FALSE,
                                  colClasses = dem_classes,
                                  showProgress = FALSE )

sample_sp_id <- list.files('E:/CEA_DATA/interval_data/electric/hourly_by_sp_id/interval_14_15/')
sample_sp_id <- tibble::as_tibble(sample_sp_id) %>% 
  mutate(sp_id = gsub(pattern = '.csv',replacement = '',value)) %>% 
  select(file = value, sp_id)
  

DT_Dem <- as.data.frame(DT_Dem)
# merge with demographic for weather station information
sample_lookup <- inner_join(sample_sp_id
                            , DT_Dem %>% select(prem_id,climate_zone_cd = PREMI_cec_climate_zone_cd, acct_id = PREMI_acct_id, sp_id, weather_station)
                            , by = 'sp_id') %>% 
  select(climate_zone_cd, file, sp_id, prem_id, acct_id, weather_station) %>% 
  group_by(climate_zone_cd) %>% 
  nest(.key = sample_100) %>% 
  mutate(sample_10 = map(sample_100, sample_frac, .10, replace = TRUE),
         sample_20 = map(sample_100, sample_frac, .20, replace = TRUE),
         sample_30 = map(sample_100, sample_frac, .30, replace = TRUE),
         sample_40 = map(sample_100, sample_frac, .40, replace = TRUE),
         sample_50 = map(sample_100, sample_frac, .50, replace = TRUE),
         sample_65 = map(sample_100, sample_frac, .65, replace = TRUE),
         sample_75 = map(sample_100, sample_frac, .75, replace = TRUE),
         sample_85 = map(sample_100, sample_frac, .85, replace = TRUE)) %>% 
  select(climate_zone_cd, sample_10:sample_85, sample_100)


TOU_CURRENT <- list(
  winter_partial_weekday = 17:19
  , winter_off_weekday = c(0:16, 20:23)
  
  , winter_off_weekend = 0:23
  
  , summer_peak_weekday = 13:18
  , summer_partial_weekday = c(10:12, 19:20)
  , summer_off_weekday = c(0:9, 21:23)
  
  , summer_partial_weekend = 17:19
  , summer_off_weekend = c(0:16, 20:23)
)

gc()
sample_pop <- sample_lookup %>% filter(climate_zone_cd == 'W') %>% 
  mutate(file = sample_10 %>% map('file')) %>% 
  select(file) %>%
  unnest() %>%
  flatten_chr()
IN_NAME <- 'E:/CEA_DATA/interval_data/electric/hourly_by_sp_id/interval_14_15/'
sample_pop <- paste0(IN_NAME,sample_pop)

d <- map(.x = sample_pop, .f = fread, header = TRUE, sep = 'auto'
         , check.names = TRUE
         , stringsAsFactors = TRUE
         , integer64 = 'character'
         , verbose = FALSE) %>% rbindlist()

  # Input: one file with usage
  # Output: separate files with usage for each year
  
  START_TIME <- proc.time()
  
  # OUT_DIR_TEMP <- paste0(OUT_LOC, subdir, '/')
  
  d[, `:=`(season = factor(ifelse(as.numeric(match(month, month.abb)) %in% c(4:10),'summer', 'winter'))
           , year = year(date)
           , holiday = FALSE)]
  
  DT_Dem <- as.data.table(DT_Dem)
  setkey(d, sp_id)
  setkey(DT_Dem, sp_id)
  
  d <- merge(d, DT_Dem[,.SD, .SDcols = DEM_COLS])
  setnames(d, old = c('PREMI_cec_climate_zone_cd', 'PREMI_acct_id')
           , new = c('climate_zone_cd','acct_id'))
  
  # Change to factor for space
  d[, `:=`(sp_id = factor(sp_id)
           , prem_id = factor(prem_id)
           , acct_id = factor(acct_id)
           , weather_station = factor(weather_station)
           , climate_zone_cd = factor(climate_zone_cd)
           , customer = paste0(prem_id,'_',acct_id))]
  
  
  # Get the time of use data
  train <- subset(d, year == 2014)
  test <- subset(d, year == 2015)
  
  # Get the time of use data
  train[,day_2014:=lubridate::wday(date)] # differentiate days
  train[,day_2015:= lubridate::wday(lubridate::ymd(date) + lubridate::years(1))] # differentiate days
  train[,date_2015:= lubridate::ymd(date) + lubridate::years(1)] # differentiate days
  train[,c(paste0('day_name_',2014:2015)):= lapply(.SD, weekdays)
        , .SDcols = c('day_2014','day_2015')]
  test[,day_2015:=lubridate::wday(date)]
  
  train[,c('weekend_2014','weekend_2015'):=lapply(.SD, function(x) x %in% c(1, 7))
        , .SDcols = c('day_2014','day_2015')]
  test[,c('weekend_2015'):=lapply(.SD, function(x) x %in% c(1, 7)), .SDcols = c('day_2015')]
  
#   train[date == '2014-08-03',]
#   test[date == '2015-08-03',]
#   
  
  train[, period_2014 := get_period(TOU_CURRENT, season, weekend_2014, hour, holiday)]
  train[, period_2015 := get_period(TOU_CURRENT, season, weekend_2015, hour, holiday)]
  test[, period_2015 := get_period(TOU_CURRENT, season, weekend_2015, hour, holiday)]
  
  
  # Convert to time of use data
  
  train_tou <- train[, .(usage = sum(usage, na.rm = TRUE), .N)
         , by = .(weather_station, sp_id, prem_id, acct_id, climate_zone_cd, season, period_2015, year, date) ]
  

  test_tou <- test[, .(usage = sum(usage, na.rm = TRUE), .N)
         , by = .(weather_station, sp_id, prem_id, acct_id, climate_zone_cd, season, period_2015, year, date) ]
  
  write.table(train_tou, paste0(OUT_DIR_TEMP, OUT_NAME_14), row.names = FALSE, quote = FALSE, sep = '\t')
  write.table(test_tou, paste0(OUT_DIR_TEMP, OUT_NAME_15), row.names = FALSE, quote = FALSE, sep = '\t')
  
#   write.table(train, paste0(OUT_DIR_TEMP, OUT_NAME_14_DAILY), row.names = FALSE, quote = FALSE, sep = '\t')
#   write.table(test, paste0(OUT_DIR_TEMP, OUT_NAME_15_DAILY), row.names = FALSE, quote = FALSE, sep = '\t')
#   
  print(paste0(climate_zone, ' is complete (', uniqueN(d, by = c('acct_id', 'prem_id')), ' customers)'))
  print(paste0(climate_zone, ' is complete (', uniqueN(d, by = c('sp_id')), ' sp_ids)'))
  print(proc.time() - START_TIME)


read_aggregate('t', 'sample1')
read_aggregate('w', 'sample1')
read_aggregate('x', 'sample1')

read_aggregate('t', 'sample2')
read_aggregate('w', 'sample2')
read_aggregate('x', 'sample2')

read_aggregate('t', 'sample3', 3100, TOU_ALT)
read_aggregate('w', 'sample3', 3100, TOU_ALT)
read_aggregate('x', 'sample3', 3100, TOU_ALT)