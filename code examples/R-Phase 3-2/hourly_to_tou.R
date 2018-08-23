library(data.table)

rm(list = ls()); gc()

START_TIME <- proc.time()

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

# Alternative TOU Constant
TOU_ALT <- list(
  winter_partial_weekday = 15:17
  , winter_off_weekday = c(0:14, 18:23)
  
  , winter_off_weekend = 0:23
  
  , summer_peak_weekday = 11:16
  , summer_partial_weekday = c(8:10, 17:18)
  , summer_off_weekday = c(0:7, 19:23)
  
  , summer_partial_weekend = 15:17
  , summer_off_weekend = c(0:14, 18:23)
)

get_period <- function(tou, season, weekend, hour, holiday){
  
  # Note: Assumes hours range from 0-23
  x <- NA
  
  # Winter TOU categories
  x <- ifelse(season == 'winter' & weekend == 'FALSE' & hour %in% tou$winter_partial_weekday, 'winter_partial_weekday', x)
  x <- ifelse(season == 'winter' & weekend == 'FALSE' & hour %in% tou$winter_off_weekday, 'winter_off_weekday', x)
  x <- ifelse(season == 'winter' & weekend == 'TRUE' & hour %in% tou$winter_off_weekend, 'winter_off_weekend', x)
  
  # Summer tou categories
  x <- ifelse(season == 'summer' & weekend == 'FALSE' & hour %in% tou$summer_peak_weekday, 'summer_peak_weekday', x)
  x <- ifelse(season == 'summer' & weekend == 'FALSE' & hour %in% tou$summer_partial_weekday, 'summer_partial_weekday', x)
  x <- ifelse(season == 'summer' & weekend == 'TRUE' & hour %in% tou$summer_partial_weekend, 'summer_partial_weekend', x)
  x <- ifelse(season == 'summer' & weekend == 'FALSE' & hour %in% tou$summer_off_weekday, 'summer_off_weekday', x)
  x <- ifelse(season == 'summer' & weekend == 'TRUE' & hour %in% tou$summer_off_weekend, 'summer_off_weekend', x)
  
  # Handle holidays
  x <- ifelse(holiday == 'TRUE', 'winter_off_peak', x)
  
  # Return as factor
  return(factor(x))
}


OUT_LOC <- 'E:/CEA_DATA/interval_data/electric/kamal/'

read_aggregate <- function(climate_zone = 'w', subdir = 'sample1', sample_no = 3100, tou1 =TOU_CURRENT){
  # Input: one file with usage
  # Output: separate files with usage for each year
  
  START_TIME <- proc.time()
  IN_NAME <- paste0('2013_2015_interval_pivot_', climate_zone, '_2.txt')
  OUT_NAME_14 <- paste0('2014_tou_kamal_', climate_zone, '.txt')
  OUT_NAME_15 <- paste0('2015_tou_kamal_', climate_zone, '.txt')
  OUT_DIR_TEMP <- paste0(OUT_LOC, subdir, '/')
  
  d <- fread(paste0('E:/CEA_DATA/interval_data/electric/kamal/', IN_NAME), stringsAsFactors = TRUE, na.strings = '?', integer64 = 'character')
  
  # Format hours 
  d[, date :=as.IDate(usg_dt, '%m/%d/%Y')]
  d[, year := year(date)]
  
  # Filter out recieved (Typically from solar customers)
  d <- d[ener_dir_cd == 'D']
  
  # Cut down to only sp_ids with complete observations for two years
  d[, `:=`(min_date = min(date), max_date = max(date)), by = sp_id]
  print(paste('sp_ids in', climate_zone, ':', d[, uniqueN(sp_id)]))
  d <- d[min_date <= '2014-01-01' & max_date >= '2015-12-31', ]
  print(paste('sp_ids with two years in', climate_zone, ':', d[, uniqueN(sp_id)]))
  
  # Get random from 2014 data sample and store it
  random_sample <- sample(unique(d[year == 2014,]$sp_id), sample_no)
  write.table(random_sample, paste0(OUT_DIR_TEMP, 'random_sp_ids_', climate_zone, '.csv'), row.names = FALSE)
  d <- d[sp_id %in% random_sample,]
  
  HOURS_OLD <- c('kwh_0100', 'kwh_0200', 'kwh_0300', 'kwh_0400', 'kwh_0500', 
                 'kwh_0600', 'kwh_0700', 'kwh_0800', 'kwh_0900', 'kwh_1000', 'kwh_1100', 
                 'kwh_1200', 'kwh_1300', 'kwh_1400', 'kwh_1500', 'kwh_1600', 'kwh_1700', 
                 'kwh_1800', 'kwh_1900', 'kwh_2000', 'kwh_2100', 'kwh_2200', 'kwh_2300', 
                 'kwh_2400')
  
  HOURS_NEW <- paste0(seq(0, 23))
  
  setnames(d, old = HOURS_OLD, new = HOURS_NEW)
  
  # Transform from wide to long
  d <- melt(d, id.vars = c('sp_id', 'date'), measure.vars = HOURS_NEW, variable.name = 'hour', value.name = 'usage')
  d[, usage := as.numeric(usage)]
  d[, hour := as.numeric(hour) - 1]

  d[, month := factor(months(date, abbreviate = TRUE), levels=month.abb)]
  d[, `:=`(season = factor(ifelse(as.numeric(match(month, month.abb)) %in% c(4:10),'summer', 'winter'))
           , weekend = wday(date) %in% c(1, 7)
           , year = year(date)
           , holiday = FALSE)]
  
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
  
  setkey(DT_Dem, sp_id) # set key for merge
  
  # merge with demographic for weather station information
  DEM_COLS <- c('prem_id','PREMI_cec_climate_zone_cd', 'PREMI_acct_id','sp_id','weather_station')
  d <- merge(d, DT_Dem[,.SD, .SDcols = DEM_COLS], by = c('sp_id'))
  
  setnames(d, old = 'PREMI_acct_id', new = 'acct_id')
  
  # Change to factor for space
  d[, `:=`(sp_id = factor(sp_id)
           , prem_id = factor(prem_id)
           , acct_id = factor(acct_id)
           , weather_station = factor(weather_station)
           , PREMI_cec_climate_zone_cd = factor(PREMI_cec_climate_zone_cd))]
  
  # Get the time of use data
  d[, period := get_period(tou1, season, weekend, hour, holiday)]
  
  # Convert to time of use data
  d <- d[, .(usage = sum(usage, na.rm = TRUE), .N)
         , by = .(weather_station, sp_id, prem_id, acct_id, season, period, year, date) ]
  
  write.table(d[year == 2014,], paste0(OUT_DIR_TEMP, OUT_NAME_14), row.names = FALSE, quote = FALSE, sep = '\t')
  write.table(d[year == 2015,], paste0(OUT_DIR_TEMP, OUT_NAME_15), row.names = FALSE, quote = FALSE, sep = '\t')
  
  print(paste0(climate_zone, ' is complete (', uniqueN(d, by = c('acct_id', 'prem_id')), ' customers)'))
  print(paste0(climate_zone, ' is complete (', uniqueN(d, by = c('sp_id')), ' sp_ids)'))
  print(proc.time() - START_TIME)
}

read_aggregate('t', 'sample1')
read_aggregate('w', 'sample1')
read_aggregate('x', 'sample1')

read_aggregate('t', 'sample2')
read_aggregate('w', 'sample2')
read_aggregate('x', 'sample2')

read_aggregate('t', 'sample3', 3100, TOU_ALT)
read_aggregate('w', 'sample3', 3100, TOU_ALT)
read_aggregate('x', 'sample3', 3100, TOU_ALT)