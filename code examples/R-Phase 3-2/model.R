# source('r/functions.R')
# rm(list = ls())
library(tidyr)
# assign constants --------------------------------------------------------
DATA_OUT_LOC <-  'E:/CEA_DATA/model_export/shifted_tou_predictions/'
OUT_NAME_DAILY <- 'model_tou_daily.txt'
OUT_NAME_MONTHLY <- 'model_tou_monthly.txt'
TRAIN_YEAR <- 2014
TEST_YEAR <- 2015
TRAIN_WEATHER <- 'temperature'

start_time <- proc.time()

# Read and format  -----------------------------------------------

# load and format interval data
SAMPLE_SIZE <- 10

x <- dir('E:/CEA_DATA/interval_data/electric/period_shift/', 'daily', full.names = T)
x <- x[grep(pattern = SAMPLE_SIZE, x = x)]

train_test <- map(.x = x, .f = fread, header = TRUE, sep = 'auto'  
                  , check.names = TRUE
                  , stringsAsFactors = TRUE
                  , integer64 = 'character'
                  , colClasses = list(factor = c('year_day','customer','opr_area_cd'),
                                      numeric = c('usage'))
                  , na.strings = '?') %>% rbindlist()
#     train_test[,usage := as.character(usage)]
#     train_test[,usage := as.numeric(usage)]

# train_test[,year_day:=as.factor(year_day)] # convert to factor for merge
unique_cd <- as.character(train_test[,unique(opr_area_cd)]) # unique op_area_cd for weather subset
setnames(train_test, old = 'usage', new = 'y') # rename usage as y for mars model
# train_test[,lapply(.SD, uniqueN)] # check for unique values
setkey(train_test, model_name, period, year, year_day, opr_area_cd) # set key for merging temp
train_test[,train_indicator:= ifelse(year == TRAIN_YEAR, 'train','test')]

# impute missing values
# train_test[,`:=` (q_01 = quantile(y, .001, na.rm = TRUE)
#                   ,q_25 = quantile(y, .25, na.rm = TRUE) 
#                   , q_75 = quantile(y, .75, na.rm = TRUE))
#            , by = .(customer, month, day)] # range of use by customers

# train_test[,`:=` (q_99 = quantile(y, .99, na.rm = TRUE)) # extreme use in model
#            ,by = .(train_indicator)]

# train_test[y == 0,.(unique(customer))]
# train_test[y > q_99,.(unique(customer))]
# train_test[y == 0, y:= runif(1, q_25,q_75),]

# idenfity and remove outliers
# outlier_users <- train_test[ y <= 0, .N, by = .(model_name, period, prem_id, train_indicator
#                                                 , month_name)]
# [,unique(customer)]
# outlier_users <- train_test[ y == 0 |y > q_99 ,unique(customer)] # users at 100th percentile
# train_test <- train_test[!customer %in% outlier_users,] # eliminate customers with extreme usage (temp fix)

# train_test[,c('q_01', 'q_25', 'q_75','q_99'):=NULL] # remove percentile placeholders
# train_test[,uniqueN(customer)]

# create file load list for weather
weather_files <- dir('E:/CEA_DATA/weather_data/period_shift/', pattern = 'period_shift_', full.names = T)

# read and bind weather data (train data has only temperature, test has normal temperature and percintile scenarios)
weather <- map(weather_files, .f = fread, header = TRUE, sep = 'auto'
               , check.names = TRUE
               , stringsAsFactors = TRUE
               , integer64 = 'character'
               # , drop = c('month_name.1') # need to drop that
               , verbose = FALSE) %>% rbindlist()
weather[,.N, by = .(measure_type, model_name)]
weather[,month_name.1 := NULL]
# format weather data#
weather[,year_day:=as.factor(year_day)] # convert to factor for merge
setnames(weather, old = 'temperature', new = 'x') # rename temperature ax x for mars model
weather <- weather[opr_area_cd %in% c(unique_cd),.(model_name, period, year, year_day, opr_area_cd
                                                   , measure_type, x)] # subset weather on op_area_cd
# weather[,lapply(.SD, uniqueN)] # check for unique values
setkey(weather, model_name, period, year, year_day, opr_area_cd) # set key for merge
setkey(train_test, model_name, period, year, year_day, opr_area_cd) # set key for merging temp
# merge interval data and weather data
train_test_weather <- merge(train_test, weather, suffixes = c(x = '_usage', y = '_weather'), allow.cartesian = TRUE)
rm(train_test);gc()
rm(weather)
# train_test_weather[,lapply(.SD, uniqueN)]

# train_test_weather[,uniqueN(customer)]

# Set variable names for consistency
train_test_weather[, `:=`(customer = factor(customer), year_day = as.numeric(year_day))]
train_test_weather[,.N, by = model_name]
# check for complete shift merge 
bad_shift <- train_test_weather[,.N, by = .(year, customer, period, model_name)
                                ][N ==1][,as.character(unique(customer))] # find incomplete observations in periods
train_test_weather <- train_test_weather[!customer %in% c(bad_shift)] # remove bad shift merge with weather



# train_test_weather[,uniqueN(customer)]
#     check to verify day shift worked
#     setkey(train_test_weather, date, model_name, year, year_day)
#     check_shift <- train_test_weather[,unique(train_test_weather)]
#     check_shift <- dcast.data.table(check_shift
#                                         , formula = year_day + model_name ~ year
#                                         , value.var = 'day_name')
#     check_shift[`2014` != `2015` & model_name == 'TOU_shift',] # should only be day num 365
#     
# Fit and predict  -----------------------------------------------
start_time <- proc.time()
setkey(train_test_weather, customer, model_name, date)

# subset on train data
train <- train_test_weather %>%
  filter(train_indicator == 'train') %>%
  filter(measure_type == TRAIN_WEATHER) %>% 
  group_by(train_indicator, model_name, customer, period) %>% 
  nest(.key = TRAIN) %>% 
  mutate(models = map(TRAIN, ~ earth(y ~ x, data = ., nprune = 3)), # train model
         fitted = map2(models, TRAIN, predict)) # make in sample predictions

# subset test data
test <- train_test_weather %>%
  filter(train_indicator == 'test') %>% 
  filter(measure_type != TRAIN_WEATHER) %>% 
  group_by(train_indicator, model_name, customer, period) %>% 
  nest(.key = TEST) %>%
  mutate(
    predicted = map2(train$models, TEST, predict) # make out of sample predictions
  )


print('Predictions Complete')
print(proc.time() - start_time)
rm(train_test_weather)
# Format output predictions  -----------------------------------------------
# sample 2 customers from each climate zone code for data validation
# sample_customers <- train_test[,lapply(.SD, sample, 2), .SDcols = 'customer', by = .(climate_zone_cd)][,as.character(customer)]
#
start_time <- proc.time()
# unnest training data for inspection
train_dt <- train %>% 
  # filter(customer %in% sample_customers) %>% 
  select(customer, period, model_name, TRAIN, fitted) %>%
  # need better way to select only what is needed; similar to gas
  unnest() %>%
  mutate(day = wday(date)) %>%
  dplyr::rename(temperature = x, usage = y) %>% 
  dplyr::rename(predicted = fitted) %>%  # rename to predicted for merge
  as.data.table() # easier manipulation with data.table

# unnest test data for inspection
test_dt <- test %>% 
  # filter(customer %in% sample_customers) %>%
  select(customer, period, model_name, TEST, predicted) %>%
  # filter(model_name == 'TOU_shift') %>%  # only use TOU_shift model
  # need better way to select only what is needed; similar to gas
  unnest() %>%
  mutate(nrml_yyyymm = as.character(sprintf('%s%02d', year, month))) %>%
  mutate(day = wday(date)) %>%
  dplyr::rename(temperature = x, usage = y) %>% 
  as.data.table() # easier manipulation with data.table

sample_train_customers <- train_dt[,lapply(.SD, sample, 2), .SDcols = 'customer'
                                   , by = .(climate_zone_cd, model_name)][,as.character(customer)]
sample_test_customers <- test_dt[,lapply(.SD, sample, 2), .SDcols = 'customer'
                                 , by = .(climate_zone_cd, model_name)][,as.character(customer)]


print('Unnest Complete')
print(proc.time() - start_time)
# Aggregate on the tou and write out
# train data for baseline
DROP_COLS_DAILY <- c(which(DAILY_BILLING_COLS %in% c('temperature'))) # remove placeholder variable
DROP_COLS_MONTHLY <- c(which(MONTHLY_BILLING_COLS %in% c('temperature'))) # remove placeholder variable

MODEL_DATA_LIST <- list(
  # train data for model quality
  
  train_shifted_tou_daily = train_dt[, .(usage = sum(usage), predicted = sum(predicted)
                                         ,temperature = mean(temperature))
                                     , by = c(DAILY_BILLING_COLS[-(DROP_COLS_DAILY)])
                                     ,][order(customer, year_day)]
  
  #     train_shifted_tou_monthly = train_dt[, .(usage = sum(usage), predicted = sum(predicted)
  #                                 , temperature = mean(temperature)) # include mean temp for measure_type
  #                                 , by = c(MONTHLY_BILLING_COLS[-(DROP_COLS_MONTHLY)]),][order(climate_zone_cd, month)],
  #     
  # test data for model quality
  , test_shifted_tou_daily = test_dt[, .(usage = sum(usage), predicted = sum(predicted)
                                         ,temperature = mean(temperature))
                                     , by = c(DAILY_BILLING_COLS[-(DROP_COLS_DAILY)])
                                     ,][order(customer, year_day)]
  
  #     , test_shifted_tou_monthly = test_dt[, .(usage = sum(usage), predicted = sum(predicted)
  #                                , temperature = mean(temperature)) # include mean temp for measure_type
  #                                , by = c(MONTHLY_BILLING_COLS[-(DROP_COLS_MONTHLY)]),][order(climate_zone_cd, month)]
)
# Evaluate output  ------------------------------------------------------

# data validation
# levels(MODEL_DATA_LIST$test_shifted_tou_daily) <- factor(MODEL_DATA_LIST$test_shifted_tou_daily, levels = month.abb)
validation <- MODEL_DATA_LIST$test_shifted_tou_daily[, resid := abs(usage - predicted)][
  ][,.(resid = sum(resid)
       , temp = round(mean(temperature),2))
    ,by = .(climate_zone_cd, month, month_name, model_name, measure_type)]
validation[,rank:=factor(min_rank(resid)),by = .(model_name, climate_zone_cd, month_name)]
validation[rank == '1' & climate_zone_cd == 'T',][order(month)]
validation[rank=='1',.N, by = .(measure_type, climate_zone_cd, rank)]

str(validation)


# generate plots ----------------------------------------------------------
PLOT_LIST <- list(
  
  resid_variation_by_scenario = ggplot(data = validation %>%
                                         filter(rank %in%c(1,2))
                                       , aes(x = rank, y = resid, fill = measure_type))+
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_grid(climate_zone_cd~month) +
    ggtitle(paste('Monthly variation of residuals for best 2 temperature scenarios for'
            ,train_dt[,uniqueN(customer)],'customers')),
  
  
  train_sample_plots = ggplot(train_dt %>%
                                filter(customer %in% sample_train_customers) 
                              , aes(x = temperature)) +
    geom_line(aes(y = predicted, color = model_name), size = 1) +
    facet_grid(customer ~ period) +
    geom_point(aes(y = usage), alpha = .05) +
    ggtitle(paste('Monthly variation of residuals for best 2 temperature scenarios for'
                  ,train_dt[customer %in% sample_train_customers,uniqueN(customer)],'customers')) +
    ylab('Electricity Usage (KwH)') +
    xlab('Temperature'), 
  
  train_outliers = ggplot(train_dt, aes(x = reorder(month_name, month), y = usage)) + 
    geom_boxplot(aes(fill = climate_zone_cd)) +
    facet_wrap(~measure_type, ncol = 1) +
    labs(x = NULL, y = 'Electricity Usage (KwH)') +
    ggtitle(paste('Investigating usage distribution for'
                            ,train_dt[,uniqueN(customer)]
                  ,'customers\nless than 150 kWh usage')),
  
  test_outliers = ggplot(test_dt %>% 
                           mutate(residual = usage - predicted)
                         , aes(x = reorder(month_name, month), y = residual)) + 
    geom_boxplot(aes(fill = measure_type )) +
    facet_wrap(~climate_zone_cd, ncol = 1) +
    scale_fill_discrete('Temp Scenario Type (percentile)') +
    labs(x = NULL, y = 'Electricity Usage (KwH)') +
    ggtitle(paste('Investigating usage distribution for'
                  ,test_dt[,uniqueN(customer)]
                  ,'customers\nless than 150 kWh usage')),
  
  model_rmse_variation = ggplot(MODEL_DATA_LIST$test_shifted_tou_daily %>% 
                                  as.data.frame() %>% 
                                  # filter(climate_zone_cd == 'W') %>% 
                                  # dplyr::filter(customer == '2620301253_8735003097') %>%
                                  # filter(month_name == 'Aug') %>% 
                                  select(month, climate_zone_cd, measure_type, nrml_yyyymm
                                         , usage,predicted) %>% 
                                  dplyr::group_by(month, climate_zone_cd
                                                  ,measure_type, nrml_yyyymm) %>% 
                                  summarise_all(funs(sum,n())) %>% 
                                  mutate(rank = sqrt((usage_sum - predicted_sum)^2)
                                         / usage_n) %>% 
                                  ungroup() %>% 
                                  select(climate_zone_cd, measure_type, usage = usage_sum
                                         , predicted = predicted_sum, rank) %>% 
                                  group_by(climate_zone_cd) %>% 
                                  mutate(rmse = round(rank,2)) %>% 
                                  mutate_at(vars(rank),funs(min_rank)) %>% 
                                  filter(rank %in% c(1,2)) %>% 
                                  # filter(rank == max(rank))
                                  arrange(climate_zone_cd, rank)
                                , aes(x = factor(rank), y = rmse)) + 
    geom_bar(aes(fill = measure_type), stat = 'identity', position = 'dodge') +
    facet_grid(climate_zone_cd~.) +
    coord_flip() +
    geom_text(aes(label = rmse)) +
    scale_fill_discrete('Temp Scenario Type (percentile)') +
    labs(x = 'Model RMSE for top 2 scenarios'
         , y = 'Temp Scenario Error (RMSE - lower is better)') +
    ggtitle(paste('Investigating RMSE distribution for top two scenarios'
                                  , MODEL_DATA_LIST$test_shifted_tou_daily[,uniqueN(customer)]
                                  ,'customers\nless than 150 kWh'))
)


# write out plots results -------------------------------------------------

PLOT_OUTPUT_PATH <- paste0('images/tou_shift_',tolower(names(PLOT_LIST)),'_',SAMPLE_SIZE,'.png')
walk2(PLOT_OUTPUT_PATH, PLOT_LIST, ggsave, width = 11, height = 8.5)

# 
# # write out model results -------------------------------------------------
MODEL_OUTPUT_PATH <- paste0(DATA_OUT_LOC,tolower(names(MODEL_DATA_LIST)),'_',SAMPLE_SIZE,'.txt')
walk2(MODEL_DATA_LIST, MODEL_OUTPUT_PATH, write.table, row.names = FALSE, append = F)


# clean environment -------------------------------------------------------

rm(test_dt)
rm(train_dt)
