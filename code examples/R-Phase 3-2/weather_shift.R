
# Assign constant variable ------------------------------------------------
TEMP_TYPES <- c('Temperature', 'Normal Hourly Temperature', 'Normal Hourly Temperature Stan')


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

# list of constant holidays
WEATHER_OUT_LOC <- 'E:/CEA_DATA/weather_data/period_shift/'
HOLIDAYS <- c('2014-12-25','2015-12-25','2014-01-01','2015-01-01','2014-07-04','2015-07-04')
TRAIN_YEAR <- 2014
TEST_YEAR <- 2015
TRAIN_WEATHER_TYPE <- 'temperature'

# Get PGE weather normals and actuals
# x <- dir('E:/CEA_DATA/weather_data/', pattern = 'weather_daily_2008_2015.txt', full.names = T)
x <- dir('E:/CEA_DATA/weather_data/', pattern = 'weather_hourly', full.names = T)

w <- map(.x = x, .f = fread, header = TRUE, sep = 'auto'  
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , na.strings = '?'
          , colClasses = list(factor = c('wea_year','weather_station','wea_month'
                              ,'wea_day','temp_type','opr_area_cd'),
                              numeric = c('wea_hour')),
          , verbose = FALSE) %>% rbindlist()
          
setnames(w, old = colnames(w), new = gsub(pattern = 'wea_', replacement = '', x = colnames(w)))
w[, date := as.IDate(date, format = '%m/%d/%Y')]
setkey(w, date)
str(w)
setkey(w, weather_station)

w[weather_station %in% c('KBFL','LBFLT'),
  ][month %in% c(8)
    & day %in% c(19:20)
    & hour == '1'][order(hour)]


# w[,uniqueN(weather_station), by = .(opr_area_cd)][order(-V1)]

bad_station <- w[is.na(avg_val),.N, by = .(temp_type, weather_station)][,unique(weather_station)] # locate any na values
w <- w[!weather_station %in% bad_station,] # remove any stations with missing values

w <- dcast.data.table(data = w # eliminate weather station to get average temp across opr area cd
                       , formula = date + hour + opr_area_cd + weather_station ~ temp_type
                      , value.var = 'avg_val'
                      , fun.aggregate = mean
                      , na.rm = TRUE
                      , fill = 0) # use fill = 0 for hours without data (daily or normal measures)
# rename columns
setnames(w, old = TEMP_TYPES, new = c('temperature', 'normal_temperature', 'normal_temperature_sd'))

add_time(w) # add time variables

# eliminate opr_area_cd with missing cross reference and temperature data
# full_temp_areas <- w[,lapply(.SD, function(x) is.na(x)), .SDcols = c('normal_temperature','normal_temperature_sd','temperature')
#   , by = .(weather_station, opr_area_cd)][,rowSums(.SD, na.rm = TRUE)
#                          , .SDcols = c('normal_temperature','normal_temperature_sd','temperature')
#                          , by = .(weather_station, opr_area_cd)
#                          ][V1==0][,as.character(unique(weather_station))]
# w[weather_station %in% full_temp_areas,]

setnames(w, old = 'year_day', new = 'TOU_standard')
w[,TOU_standard:=as.numeric(TOU_standard)] # as numeric
w[,day:=lubridate::wday(date)]
w[,day_name:=lubridate::wday(date, label = TRUE)]

# create adjusted shift dates to correct year over year day shift
w[year == TRAIN_YEAR,shift_date:= as.IDate(date -1)]
w[year == TEST_YEAR,shift_date:= as.IDate(date)]
w[year == TRAIN_YEAR,TOU_shift:= as.numeric(yday(shift_date))]
w[year == TEST_YEAR,TOU_shift:= as.numeric(yday(shift_date))]

w <- melt.data.table(data = w, # create hourly temperature for every type and two models
                          , measure.vars = c('TOU_shift','TOU_standard') # limit to TOU Shift
                          , variable.name = 'model_name'
                          , value.name = 'year_day'
                          , na.rm = TRUE)

# check for TOU_shift alignment
# setkey(w, model_name, date) 
# check_shift <- w[,unique(w)]
# check_shift <- dcast.data.table(check_shift, formula = year_day + model_name ~ year, value.var = 'day_name')
# check_shift[`2014`!=`2015` & model_name == 'TOU_shift',] # only 365

## check for variation in hourly temperatures on a single day
# w[year_day == 220
#   & weather_station == 'LANGT'
#   & model_name == 'TOU_shift'
#   & year == 2015,.(date, weather_station, hour, normal_temperature, temperature)][order(hour)]

w[,weekend := day_name%in%c('Sat','Sun')] # weekend true false per model name
w[, holidate:=as.character(date)] # create placeholder date per model name
w[,holiday := holidate%in%c(HOLIDAYS)] # holiday true false per model name
w[,holidate:=NULL] # remove placeholder

# # check for na values
# w[,lapply(.SD, function(x) sum(is.na(x))),]
# w <- w[!is.na(normal_temperature_sd),] # remove na
# w <- w[!is.na(temperature),] # #remove na

# w <- subset(w, model_name == 'TOU_shift') # keep only TOU_shift model for predictions
# assign temperature scenarios
w[, `:=`(temperature_high_90 = qnorm(.90, mean = normal_temperature, sd = normal_temperature_sd)
              , temperature_mid_high_70 = qnorm(.70, mean = normal_temperature, sd = normal_temperature_sd)
              , temperature_low_10 = qnorm(.10, mean = normal_temperature, sd = normal_temperature_sd))]

## check for variation in hourly temperatures on a single day
w[year_day == 220
  & weather_station == 'LANGT'
  & model_name == 'TOU_shift'
  & year == 2015,.(date, opr_area_cd, weather_station, hour, temperature_high_90, temperature_mid_high_70, normal_temperature)][order(hour)]
# 
w[, period := get_period(TOU_CURRENT, season, weekend, hour, holiday)]  # assign periods per model
# check for variation in hourly temperatures on a single day

# melt to create single temperature variable
w <- melt.data.table(data = w
                          , measure.vars = c('temperature', 'normal_temperature'
                                             ,'temperature_high_90'
                                             ,'temperature_mid_high_70'
                                             ,'temperature_low_10'),
                          , variable.name = 'measure_type'
                          , value.name = 'temperature')
w[year_day == 220
  & weather_station == 'LCUPT'
  # & opr_area_cd == 'BG'
  & model_name == 'TOU_shift'
  & year == 2015,.(date, opr_area_cd, weather_station, hour, measure_type, temperature)]

# sum at tou and daily levels
w_tou <- w[, lapply(.SD, mean, na.rm = TRUE),
       , by = c(DAILY_WEATHER_COLS)
           , .SDcols = c("temperature")]

# check for variation at TOU periods
w_tou[year_day == 220
  # & weather_station == 'LCUPT'
  & opr_area_cd == 'BG'
  & year == 2015,.(date, opr_area_cd, weather_station, period, measure_type, temperature)][order(period)]

## check for missing values
# w_tou[,lapply(.SD, function(x) sum(is.na(x))),]


# write files -------------------------------------------------------------
# check subsets for scenario and model type
w_tou[year == 2014 & year_day == 220& measure_type == TRAIN_WEATHER_TYPE,]
w_tou[year == 2015 & year_day == 220 & measure_type != TRAIN_WEATHER_TYPE,]

# write train weather - only temperature in 2014

WEATHER_DAILY_LIST <- list(
  # write train weather - ONLY temperature in 2014
  weather_period_shift_2014 = subset(w_tou
                                     , year == TRAIN_YEAR & measure_type == TRAIN_WEATHER_TYPE),
  # write test weather - exclude temperature in 2015
  weather_period_shift_2015 = subset(w_tou
                                     , year == TEST_YEAR & measure_type != TRAIN_WEATHER_TYPE)
)

# create data list path
WEATHER_FILE_PATH <- paste0(WEATHER_OUT_LOC,tolower(names(WEATHER_DAILY_LIST)),'.txt')
# write out data list to path
walk2(WEATHER_DAILY_LIST, WEATHER_FILE_PATH, write.table, row.names = FALSE, append = F)



