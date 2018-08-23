gc()
rm(list = ls())
library(data.table)
library(purrr)
library(dplyr)
library(lubridate)

# Comparing model at shifted tou level ------------------------------------

baseline <- fread()



# shifted TOU periods -----------------------------------------------------
non_tou <- c(sample1 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'w.txt', full.names = TRUE))

shift_tou <- map(non_tou, .f = fread, header = TRUE, sep = 'auto'
                      , check.names = TRUE
                      , stringsAsFactors = TRUE
                      , integer64 = 'character'
                      , verbose = FALSE)

shift_tou <- shift_tou %>%
  map(. %>% mutate(customer = paste0(prem_id,'_',acct_id)))%>% map(. %>% as.data.table()) %>%  
  map(. %>% setkey(date))

train <- copy(shift_tou[[1]])[,date := as.IDate(date)]
test <- copy(shift_tou[[2]])[,date := as.IDate(date)]

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


train[, period_2014 := get_period(TOU_CURRENT, season, weekend_2014, hour, holiday)]
train[, period_2015 := get_period(TOU_CURRENT, season, weekend_2015, hour, holiday)]
test[, period_2015 := get_period(TOU_CURRENT, season, weekend_2015, hour, holiday)]

train[,.N, by = .(period_2015)]
test[,.N, by = .(period_2015)]

# Convert to time of use data

train <- train[, .(usage = sum(usage, na.rm = TRUE), .N)
         , by = .(weather_station, customer, sp_id, prem_id, acct_id, season, period_2015, year, date) ]

test <- test[, .(usage = sum(usage, na.rm = TRUE), .N)
         , by = .(weather_station, customer, sp_id, prem_id, acct_id, season, period_2015, year, date) ]

# train[prem_id == '9959429009',]
# test[prem_id == '9959429009',]
subdir = 'sample1'
CLIMATE_ZONE <- 'w'
OUT_LOC <- 'E:/CEA_DATA/interval_data/electric/kamal/'
OUT_DIR_TEMP <- paste0(OUT_LOC, subdir, '/')
OUT_NAME_14 <- paste0('2014_tou_kamal_', CLIMATE_ZONE, '.txt')
OUT_NAME_15 <- paste0('2015_tou_kamal_', CLIMATE_ZONE, '.txt')

write.table(train, paste0(OUT_DIR_TEMP, OUT_NAME_14), row.names = FALSE, quote = FALSE, sep = '\t')
write.table(test, paste0(OUT_DIR_TEMP, OUT_NAME_15), row.names = FALSE, quote = FALSE, sep = '\t')



# finding outliers --------------------------------------------------------

predictions <- list(sample1 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'daily', full.names = TRUE),
sample2 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample2/', pattern = 'monthly_w', full.names = TRUE))

str(predictions)
# 
# w_files <- list(sample1 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'w.txt', full.names = TRUE),
# sample2 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample2/', pattern = 'w.txt', full.names = TRUE),
# sample3 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample3/', pattern = 'w.txt', full.names = TRUE)
# )
# 
customer_daily <- map(predictions[[1]], .f = fread, header = TRUE, sep = 'auto'
     , check.names = TRUE
     , stringsAsFactors = TRUE
     , integer64 = 'character'
     , verbose = FALSE) %>% rbindlist()

customer_daily[,date:= as.IDate(date)]

customer_monthly <- map(predictions[[2]], .f = fread, header = TRUE, sep = 'auto'
     , check.names = TRUE
     , stringsAsFactors = TRUE
     , integer64 = 'character'
     , verbose = FALSE) %>% rbindlist()

train_test[,date := as.IDate(date)]
train_test <- add_time(train_test)
train_test[,day_number:=data.table::wday(date)]
train_test[,day_name:=weekdays(date, abbreviate = TRUE)]
train_test <- train_test[month_name == 'Aug' & day_name == 'Sun',]
train_test[,day_type:= ifelse(day_number  %in% c(1, 7),'Weekend','Weekday')]


# Analyzing model quality on monthly and daily level -----------------------------------------

# taking tou data to determine shift days between 2014 and 2015 
train_test <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample2/', full.names = TRUE, pattern = '_w.txt')

train_test <- map(train_test, fread
                  , integer64 = 'character'
                  , sep = 'auto')

train_test <- rbindlist(train_test) # bind the lists to a single data.table

sp_id <- train_test[,.N, by = .(sp_id)][sample(.N, 1)][,(sp_id)] # one random sp_id

train_test_small <- subset(train_test, sp_id == sp_id[1]) # subset on one random sp_id
setkey(train_test_small, date) # set key to eliminate TOU periods and find unique days
train_test_small <- train_test_small[,unique(train_test_small)] # only unique days in each year (730)

train_test_small[,date:= as.IDate(date)] # set as date str
train_test_small[,year_day:= data.table::yday(date)] # year day 1-365
train_test_small[,month_name:= month.abb[data.table::month(date)]] # month name
train_test_small[,day_name:=lubridate::wday(date, label = TRUE)] # day name
train_test_small[,day_type:=ifelse(lubridate::wday(date)%in% c(1,7)
                                   , 'weekend','weekday')] # weekday weekend

# cast to determine day name and day type shift year over year
train_test_wide <- dcast.data.table(train_test_small[month_name == 'Aug',], year_day + month_name ~ year
                                    , value.var = c('day_type','day_name','period'))

train_test_wide[,shift_type:= paste0(day_type_2014,' to ', day_type_2015)]

# the answer is date plus + day


# load daily and monthly predictions for comparison
monthly_tou_shift <- fread('E:/CEA_DATA/model_export/shifted_tou_predictions/predictions_monthly_w.csv')
monthly_tou <- fread('E:/CEA_DATA/model_export/tou_predictions/predictions_monthly_w.csv')
daily_predictions <- fread('E:/CEA_DATA/model_export/daily_predictions/predictions_daily_w.csv')

setkey(monthly_tou_shift, month)
setkey(monthly_tou, month)
setkey(daily_predictions, date)
daily_predictions[,uniqueN(date), by = .(month)] # check to see if full observations


monthly_tou_shift[,model_type:= 'TOU_shift']
monthly_tou[,model_type:= 'TOU']
rbind(monthly_tou, monthly_tou_shift)
# analyze model on monthly level
monthly_tou_ <- monthly_predictions[month == 'Aug'
                        , .(rmse_new = rmse(actual_test, pred_norm)
                            , rmse_old = rmse(actual_test, pred_usage_lag)
                            , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                        , by = .(month, period)]
train[month == 'Aug',]
# analyze model on daily level
daily_metrics <- daily_predictions[month == 'Aug',] 
                                 , .(rmse_new = rmse(actual_test, pred_norm)
                                     , rmse_old = rmse(actual_test, pred_usage_lag)
                                     , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                                 , by = .(customer, date, month, day)][order(date)
                                                                       ][,resid := round((rmse_new - rmse_old),2),]

# customer_metrics[,five_num:=.(list(fivenum(improvement)))]
# fivenum(customer_metrics$improvement)
# model calibration
summary(customer_metrics$resid)
customer_metrics[resid > -.009]
customer_metrics[resid < .009,]

customer_metrics[,over_under:= ifelse(resid > 0, 'worse'
                                      , ifelse(resid < 0, 'better'
                                               ,'even'))]

customer_metrics[,breaks:=cut(improvement, include.lowest = TRUE, right = TRUE
                              , breaks = unique(quantile(improvement
                                                         , probs = seq(0, 1,.05))))]


customer_metrics[, .N, by = .( breaks, over_under)]


# Comparing predictions at daily customer level ---------------------------
str(customer_metrics)
daily_plot <- ggplot(data = customer_metrics[,.N, by = .(date, over_under, month)][order(over_under)]
       , aes(x = date, y = N, fill = over_under)) +
  geom_bar(stat = 'identity') +
  scale_x_date(date_breaks = 'day', date_labels = '%a %d') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~month ) +
  scale_fill_manual(values = c(better = CP$blue, even = CP$green, worse = CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               , 'Temp even with constant'
                               , 'Temp worse than constant')) +
  geom_hline(data = melt(customer_metrics[,.(sample75 = uniqueN(customer)*.75
                                             , sample50 = uniqueN(customer)*.50
                                             , sample25 = uniqueN(customer)*.25), by = .(month)
                                          ],id.vars = 'month',variable.name = 'line',value.name = 'N')
             ,aes(yintercept = N, linetype = line)
             , color = 'black', size = 1.5) +
  scale_linetype_discrete(name = 'Expected Breaks'
                          , labels = c('.75 of sample'
                                       , '.50 of sample'
                                       , '.25 of sample')) +
  labs(x = NULL, y = '# of Customers per day') +
  ggtitle('Count and Quality of daily predictions in W compared to baseline model')

ggsave('images/daily_model_performance_new.png',daily_plot, width = 11, units = 'in')

# comparing the weight of predictions at daily level ----------------------
daily_average <- ggplot(data = customer_metrics[over_under!='even',lapply(.SD, function(x) round(mean(x),2)), by = .(date, over_under, month)
                               , .SDcols = 'improvement'][order(over_under)]
       , aes(x = date, y = improvement, fill = over_under)) +
  geom_bar(stat = 'identity', position = 'identity') +
  scale_x_date(date_breaks = 'day', date_labels = '%a %d') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(CP$blue, CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               
                               , 'Temp worse  than constant')) +
  labs(x = NULL, y = '% improvement over baseline on daily level') +
  geom_text(aes(label = improvement)) +
  ggtitle('Average improvement for daily predictions in W compared to baseline model')

ggsave('images/daily_model_improvement_new.png',daily_average, width = 11, units = 'in')

# Distribution of actual values -------------------------------------------

breaks <- levels(unique(customer_metrics$breaks))
break_plot <- c(1, 2, 10, 15, 20)
bad_days <- c(2,9,16,10)
select_breaks <- breaks[break_plot]

customer_metrics[breaks %in% c(select_breaks) ,.N, by = .(breaks)]

imp_disribution_plot <- ggplot(data = customer_metrics[day %in% c(bad_days) 
                                                       & breaks %in% c(select_breaks)
                                                       & improvement > -1,][order(-over_under)]
       ,aes(x = factor(day), y = improvement)) +
  # facet_wrap(~over_under, nrow = 1, scales = 'free') +
         geom_boxplot(outlier.colour = 'red')
  labs(x = "Selected days in August", y = 'range of % improvement over baseline on daily level') +
  ggtitle('Actual improvement for daily predictions in W compared to baseline model')



# customer_metrics[,`:=`(impr_min = lapply(five_num, `[`, 1), 
#                        impr_q1 = lapply(five_num, `[`, 2),
#                        impr_med = lapply(five_num, `[`, 3),
#                        impr_q3 = lapply(five_num, `[`, 4),
#                        impr_max = lapply(five_num, `[`, 5))]

# customer_metrics[,`:=` (min_floor = as.numeric(impr_min)-1
#                           , max_ceil = as.numeric(impr_max)+.0001)]


# customer_metrics[,bucket:= cut(improvement, include.lowest = TRUE
#                                , right = TRUE
#                                , breaks = unique(quantile(improvement, c(0, .2,.5,.75,.95,1))))]

ggplot(data = customer_metrics, aes(y = improvement, x = period)) +
  geom_boxplot()


eua_cross_ref <- fread('../CEA_DATA/demograhic_data/eua_cross_ref.txt',
                header = 'auto',
                sep='auto',
                na.strings = '?',
                integer64 = 'character',
                stringsAsFactors = TRUE,
                # colClasses = dem_classes,
                showProgress = FALSE )
eua_cross_ref[,customer:= paste0(prem_id,'_',acct_id)]
setkey(eua_cross_ref, prem_id)
setkey(eua_cross_ref, customer)

DT_Dem <- fread('../CEA_DATA/demograhic_data/cea_demographics.txt',
                header = 'auto',
                sep='auto',
                na.strings = '?',
                integer64 = 'character',
                stringsAsFactors = TRUE,
                # colClasses = dem_classes,
                showProgress = FALSE )
DT_Dem <- DT_Dem[TENUR_acct_yrs !=0,]
DT_Dem[,customer:= as.factor(paste0(prem_id,'_',PREMI_acct_id))]


DEMO <- '(customer|AGE|ANNUA|DWELL_prem|ENGAG|FUEL|HOME|NUMBE|PROGR|RATES|RESID|pty)'
MELT <- '(AGE|ANNUA|DWELL_prem|ENGAG|FUEL|HOME|NUMBE|PROGR|RATES|RESID)'

DEMO_VARS <- grep(pattern = DEMO,x = names(DT_Dem))

num_cols <- which(sapply(DT_Dem,is.numeric))
int_cols <- which(sapply(DT_Dem,is.integer))
char_cols <- which(sapply(DT_Dem,is.character))

customer_metrics[,climate_zone_cd:='W']
# change to factors
customer_metrics[,c('customer','prem_id', 'month','period','over_under','climate_zone_cd','bucket'):=lapply(.SD, as.factor)
           , .SDcols = c('customer','prem_id', 'month','period','over_under','climate_zone_cd','bucket')]

setkey(DT_Dem, customer)
setkey(customer_metrics, customer)

customer_metrics_m <- merge(customer_metrics, DT_Dem[,DEMO_VARS, with = FALSE]
                            , suffixes = c(x = '_metrics', y ='_cross'))

str(customer_metrics_m)
# MELT_VARS <- grep(pattern = MELT,x = names(customer_metrics_m))
ID_VARS <- c(colnames(customer_metrics))
MEAS_COLS <- c('rmse_new','rmse_old','improvement')

id_cols <- which(sapply(customer_metrics,is.factor))
pty_id <- which(colnames(customer_metrics_m)=='pty_id')
id_cols <- c(id_cols, pty_id)
meas_cols <- which(sapply(customer_metrics,is.numeric))

factor_cols <- which(sapply(customer_metrics_m,is.factor))
num_cols <- which(sapply(customer_metrics_m,is.numeric))
int_cols <- which(sapply(customer_metrics_m,is.integer))

num_cols <- num_cols[!num_cols %in% meas_cols]
num_cols <- num_cols[!num_cols %in% int_cols]
int_cols <- int_cols[!int_cols %in% num_cols]
factor_cols <- factor_cols[!factor_cols %in% id_cols]

customer_metrics_factor <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(factor_cols), variable.name = 'measure', value.name = 'value')


customer_metrics_integer <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(int_cols), variable.name = 'measure', value.name = 'value'
                                      , na.rm = TRUE)


customer_metrics_num <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(num_cols), variable.name = 'measure', value.name = 'value'
                                      )
customer_metrics_num[,.N, by = .(measure)]
customer_metrics_integer[,.N, by = .(measure)]
customer_metrics_factor[,.N, by = .(measure)]
str(customer_metrics_numeric$value)



# plots -------------------------------------------------------------------

factor_sub <- subset(customer_metrics_factor, measure == 'DWELL_prem_typ_cd' & over_under == 'worse')
customer_metrics_factor[,lapply(.SD, mean), .SDcols = 'improvement', by = .(measure, over_under, value)]

ggplot(data = factor_sub, aes(x = bucket, y = improvement)) +
  facet_wrap(~period) +
  geom_boxplot(aes(fill = value))

