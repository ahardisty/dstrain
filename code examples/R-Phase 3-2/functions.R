# rm(list = ls())

PACKAGES <- c(
  'ggplot2', 
  'tidyr',
  # 'plyr',
  'dplyr', 
  # 'randomForest', 
  'earth', 
  'lubridate',
  #               'rattle',
  #               'RCurl',
  #               'rpart',
  #               'rpart.plot',
  'haven',
  'stringr',
  # 'broom',
  'purrr',
  'data.table',
  'readxl',
  'xlsx'
  
)

TRAIN_YEAR <- 2014
TEST_YEAR <- 2015
HOLIDAYS <- c('2014-12-25','2015-12-25','2014-01-01','2015-01-01','2014-07-04','2015-07-04')
LAG_YEARS <- 5
LOWER <- paste0(TEST_YEAR-5,'-01-01')
UPPER <- paste0(TEST_YEAR,'-12-31')
# CLIMATE_ZONES <- c('T','W')
# LAG_TERMS <- c(paste0('LAG_',1:5))
TRAIN_WEATHER_TYPE <- 'temperature'


# weather_wide[,c(lag_terms) := shift(.SD, 1:length(lag_terms),0, 'lag')
#              , .SDcols = 'temperature', by = key(weather_wide)]


DEM_COLS <- c('customer','prem_id', 'acct_id','sp_id','pty_id','opr_area_cd','climate_zone_cd')

MOD_COLS <- c('TEMP_NORMAL')
BASE_COLS <- c('CONS_USG')
WEA_COLS <- c('date', 'opr_area_cd', 'model_name', 'temp_actual', 'temp_normal', 'temp')
CHAR_COLS <- c('start_date','stop_date', 'usage date', 'daily_end')
FCTR_COLS <- c('sp_id', 'sa_id', 'prem_id')

# will need to add opr_area_cd back in for non-PGE weather stations
DAILY_BILLING_COLS <- c('sp_id', 'date', 'month', 'year', 'month_day', 'month_name', 'season', 'day_name'
                        , 'day', 'model_name', 'year_day', 'weekend', 'holiday', 'customer', 'prem_id'
                        , 'acct_id', 'pty_id', 'weather_station', 'opr_area_cd','climate_zone_cd', 'period'
                        , 'measure_type', 'nrml_yyyymm','temperature')

SELECT_COLS <- c("sp_id", "date", "month", "year", "month_day", "month_name", 
                 "season", "day_name", "day", "model_name", "year_day", "weekend", 
                 "holiday", "customer", "prem_id", "acct_id", "pty_id", "weather_station", 
                 "opr_area_cd", "climate_zone_cd", "period", "measure_type", "nrml_yyyymm", 
                 "usage", "predicted", "temperature")

MONTHLY_BILLING_COLS <- c('month', 'year', 'month_name', 'model_name', 'climate_zone_cd'
                          ,'measure_type','period', 'nrml_yyyymm','temperature')


# MONTH_LEVELS <- c('Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','O'))
DAILY_WEATHER_COLS <- c('model_name', 'weather_station','opr_area_cd','year_day', 'year', 'day_name', 'month_name', 'month_day'
                        , 'month_name','season', 'date', 'shift_date', 'period', 'measure_type')

MONTHLY_WEATHER_COLS <- c('month', 'year', 'month_name', 'model_name','weather_station','opr_area_cd', 'climate_zone_cd'
                          , 'measure_type','period')

# Colors
CP <- list('blue'='#1f77b4', 'orange'='#ff7f0e',
           'green' = '#2ca02c', 'red' = '#d62728',
           'purple'='#9467bd', 'brown' = '#8c564b',
           'pink' = '#e377c2', 'grey' = '#7f7f7f',
           'yellow' = '#bcbd22', 'teal' = '#17becf')

bucket <- function(x, type){
  # Break up billing differences into discrete buckets
  #
  # Args:
  #   x: the column/variable which is a continuous variable
  #   type: whether the column is in absolute ($) or relative terms (%)
  #
  # Returns:
  #   A column with discrete values instead of continous values
  
  if(type == 'percent'){
    return(cut(x, 
               breaks = c(min(x), .10, .20, .30, max(x)),
               labels = c('<10%','10% - 20%','20% - 30%','>30%')))    
  }else if(type == 'absolute'){
    return(cut(x, 
               breaks = c(min(x), 10 ,20, 30, max(x)), 
               labels = c('<$10', '$10 - $20','$20 - $30', '>$30')))
  }
}

get_impact <- function(diff_pct, diff, care_ind){
  # Determine the impact associated with difference in bills
  #
  # Args:
  #   diff_pct: Difference in bills expressed as column of percents
  #   diff: Difference in bills expressed as absoolute dollar value
  #
  # Returns:
  #   A column with high, medium, low, or saver depending on the bill impact
  
  x <- 'Not Sure'
  
  x <- ifelse((care_ind == 'Y' & (diff_pct < .10 & diff < 10)), 'low', x)
  
  x <- ifelse((care_ind == 'Y' & (diff_pct > .10 | diff > 10)), 'medium', x)
  
  x <- ifelse((care_ind == 'Y' & (diff_pct > .10 & diff > 10)), 'high', x)
  
  x <- ifelse((care_ind == 'N' & (diff_pct < .10 & diff < 20)), 'low', x)
  
  x <- ifelse((care_ind == 'N' & (diff_pct > .10 | diff > 20)), 'medium', x)
  
  x <- ifelse((care_ind == 'N' & (diff_pct > .10 & diff > 20)), 'high', x)
  
  x <- ifelse(diff < 0, 'savers', x)
  
  x <- factor(x, levels = c('savers', 'low', 'medium', 'high'))
  
  return(x)
}

plot_heatmap <- function(df, cols, title, file_name){
  # Create a heatmap in ggplot2
  #
  # Args:
  #   df: the data.table with the billing calculations in it
  #   cols: a vector with the variable names meant to appear as the rows, columns
  #   title: The title of the chart
  #   file_name:  the name of the file to save, if no file name is given 
  #               no file will be created
  #
  # Returns:
  #   A ggplot object with mostly default settings (can be customized further)
  
  df <- bills[sample == 'sample2']
  title <- paste0('Delta Confusion Matrix (Sample ', 2, ')')
  
  hm_old <- df[!is.na(old_diff_pct_break) & 
                 !is.na(old_diff_break) & 
                 !is.na(new_diff_break)
               , .N, by = c('old_impact', 'actual_impact', 'month', 'deriv_baseline_terr_cd')]
  
  hm_new <- df[!is.na(old_diff_pct_break) & 
                 !is.na(old_diff_break) & 
                 !is.na(new_diff_break)
               , .N, by = c('new_impact', 'actual_impact', 'month', 'deriv_baseline_terr_cd')]
  
  hm_master <- merge(hm_new, hm_old
                     , by.x = c("new_impact", "actual_impact", "month", "deriv_baseline_terr_cd")
                     , by.y = c("old_impact", "actual_impact", "month", "deriv_baseline_terr_cd")
                     , suffixes = c('_new', '_old')
                     , all.x = TRUE
                     , all.y = TRUE)
  hm_master[is.na(hm_master)] <- 0
  hm_master[, N_diff := round(ifelse((N_new - N_old) / (N_old + 0.0001) > 1, 1
                                     , (N_new - N_old) / (N_old + 0.0001)), 3)]
  # setnames(hm_master, old = 'new_impact', new = 'projected_impact')
  setnames(hm_master, old = c('new_impact', 'actual_impact'), new = c('x', 'y'))
  # summary(hm_master)
  plt <- ggplot(hm_master, aes(x= x, y = y, fill = N_diff)) + 
    geom_tile() +
    geom_text(aes(label = sprintf("%1.1f%%", 100*N_diff, label = N_diff)), color = 'white') +
    facet_grid(month ~ deriv_baseline_terr_cd) + 
    xlab('Predicted Impact') + 
    ylab('Actual Impact') + 
    scale_fill_gradient(low='#56B1F7', high='#132B43') + 
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none",
          text = element_text(size = 16), 
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  if(!missing(file_name)){
    ggsave(paste0('C:/Users/J8DG/Documents/images/', file_name, '.pdf'), plot = plt, 
           width = 11, height = 8.5)    
  }
  # print(plt)
  return(plt)
}

true_positive <- function(actual, prediction, positive = 'high'){
  # When it's actually high impact, how often does it predict high impact?
  return(sum(prediction == positive & actual == positive) / sum(actual == positive))
}

false_positive <- function(actual, prediction, positive = 'high'){
  # When it's actually not high impact, how often does it predict high impact?
  return(sum(prediction == positive & actual != positive) / sum(actual != positive))
}

specificity <- function(actual, prediction, positive = 'high'){
  # When it's actually not high impact, how often does it predict not high impact?
  return(sum(prediction != positive & actual != positive) / sum(actual != positive))
}

precision <- function(actual, prediction, positive = 'high'){
  # When it predicts high impact, how often is it correct?
  return(sum(prediction == positive & actual == positive) / sum(prediction == positive))
}

accuracy <- function(actual, prediction){
  # How often did it predict the correct class?
  return(sum(prediction == actual) / NROW(actual))
}

calculate_metrics <- function(in_data = bills, group_vars){
  x <- in_data[, .(
    rmse_old = rmse(actual_test, forecast_old)
    , rmse_new = rmse(actual_test, forecast_new)
    , accuracy_old = accuracy(actual_impact, old_impact)
    , accuracy_new = accuracy(actual_impact, new_impact)
    , true_positive_old = true_positive(actual_impact, old_impact)
    , true_positive_new = true_positive(actual_impact, new_impact)
    , false_positive_old = false_positive(actual_impact, old_impact)
    , false_positive_new = false_positive(actual_impact, new_impact)
    , precision_old = precision(actual_impact, old_impact)
    , precision_new = precision(actual_impact, new_impact)
  )
  , by = group_vars]
  
  # Do some data wrangling to make the output prettier
  x <- melt(x, id.vars = group_vars )
  x[, metric := str_extract(variable, '[a-z]+_*[a-z]*(?=_new|_old)')]
  x[, version := str_extract(variable, '(new)|(old)')]
  x <- dcast(x, paste(paste(group_vars, collapse = ' + '), '+ metric ~ version'))
  # x <- dcast(x, 'month + deriv_baseline_terr_cd + care_ind + sample + metric ~ version', value.var = 'value')
  x[, change := (new - old) / old]
  
  return(x)
}

# Read and format data ----------------------------------------------------
read_bills <- function(file_name){
  x <- as.data.table(read_sas(IN_FILE_BILLS))
  x <- IN_FILE_BILLS
  
  # Check that the column names match up
  if(any(!(BILL_COLS %in% colnames(x)))){
    stop('Columns are missing in the source data: '
         , paste(BILL_COLS[!(BILL_COLS %in% colnames(x))], collapse = ', '))
  }
  x[,.N, by = .(sample)]
  x[,bill_type:= substr(sample, 4,6)]
  x[,model_type:= substr(sample, 1,3)]
  actual_temp_2014 <- subset(x, sample %in% c('traact','tesact') & month == TEST_MONTH & prem_id == TEST_PREM)
  actual_cons_2015 <- subset(x, sample == 'conact')
  pred_cons_2015 <- subset(x, sample == 'conpre')
  actual_temp_2015 <- subset(x, sample == 'tesact')
  pred_temp_2015 <- subset(x, sample == 'tespre')
  
  formula <- care_ind + deriv_baseline_terr_cd + prem_id + acct_id +month + percentile + sample + model_type ~ bill_type
  # formula <- care_ind + sample + deriv_baseline_terr_cd + prem_id + acct_id + month ~  percentile
  
  bill_list <- list(
    actual_cons_2015 = dcast.data.table(data = actual_cons_2015
                                        , formula = formula
                                        , value.var = c('bill_amt')
                                        , fun.aggregate = sum), # Due to changing SA ids
    
    pred_cons_2015 = dcast.data.table(data = pred_cons_2015
                                      , formula = formula
                                      , value.var = c('bill_amt')
                                      , fun.aggregate = sum), # Due to changing SA ids
    
    actual_2015 = dcast.data.table(data = actual_2015
                                   , formula = formula
                                   , value.var = c('bill_amt')
                                   , fun.aggregate = sum), # Due to changing SA ids
    
    pred_2015 = dcast.data.table(data = pred_2015
                                 , formula = formula
                                 , value.var = c('bill_amt')
                                 , fun.aggregate = sum), # Due to changing SA ids
    
    actual_2014 = dcast.data.table(data = actual_2014
                                   , formula = formula
                                   , value.var = c('bill_amt')
                                   , fun.aggregate = sum) # Due to changing SA ids
  )
  
  
  actual_bills <- 
    train_actual <- subset(x, model_type == 'tra')
  x[,.N, by = sample]
  test
  
  x[sa_id=='0836410029',]  
  x[, care_ind := ifelse(grepl('L', cmprs_rt_sched_cd), 'Y', 'N')]
  x[, month := factor(month.abb[month(bill_dt)])] # get month
  # x <- x[month %in% c('May', 'Aug')]
  # formula <- care_ind + deriv_baseline_terr_cd + prem_id + acct_id + month ~  sample
  formula <- care_ind + deriv_baseline_terr_cd + prem_id + acct_id +month +percentile + sample +model_type   ~ bill_type
  # formula <- care_ind + sample + deriv_baseline_terr_cd + prem_id + acct_id + month ~  percentile
  x <- dcast.data.table(data = x
                        , formula = formula
                        , value.var = c('bill_amt')
                        , fun.aggregate = sum) # Due to changing SA ids
  setnames(x, old = c('conact','conpre','tesact','tespre','traact')
           , new = c('cons_usg_actual','cons_usg_predicted','temp_train_actual'
                     ,'temp_train_predicted','temp_train_actual'))
  
  
  x[, deriv_baseline_terr_cd := factor(deriv_baseline_terr_cd
                                       , levels = c('w','x', 't')
                                       , labels = c('W (Desert/Mountains)', 'X (Hills)', 'T (Coast)'))]
  
  return(x)
}

change_col_name <- function(column_name){
  # Format column names for billing
  
  split_col_name <- str_split(column_name, '_')
  new_col_name <- paste(split_col_name[[1]][c(4:5, 1:3)], collapse = '_')
  return(new_col_name)
}

# Test case
# change_col_names("tier_1_kwh_summer_partial")


read_process <- function(file_location, usage_file = 'tou_kamal_w_2014.txt', yr = 2014){
  
  # Read in usage data
  d <- fread(paste0(file_location, usage_file)
             , stringsAsFactors = TRUE
             , na.strings = 'NA'
             , integer64 = 'character')
  
  d[, date := as.IDate(date, format = '%Y-%m-%d')]
  d[, `:=`(year = year(date)
           , month = factor(months(date, abbreviate = TRUE))
           , day = mday(date)
           , customer = factor(paste(acct_id, prem_id, sep = '_')))]
  
  # Filter out inactive customers
  inactive_customers <- d[usage == 0, .N, by = customer][N > (.75 * 365)]$customer
  d <- d[!(customer %in% inactive_customers)]
  
  # Remove small number of missing usage values
  d <- d[!is.na(usage) & year == yr,]
  
  # Get PGE weather normals and actuals
  w <- fread('CEA_DATA/weather_data/weather_daily_2008_2015.txt'
             , integer64 = 'character'
             , na.strings = '?'
             , col.names = c('year', 'weather_station', 'date', 'month', 'day'
                             , 'wea_type', 'avg_val', 'opr_area_cd'))
  
  TEMP_TYPES <- c('Temperature', 'Normal Hourly Temperature', 'Normal Hourly Temperature Stan')
  
  w <- w[substr(weather_station, 1, 1) == 'L' & (wea_type %in% TEMP_TYPES),]
  
  w <- dcast(w
             , formula = 'date + weather_station ~ wea_type'
             , value.var = 'avg_val'
             , fun.aggregate = mean)
  
  setnames(w, old = TEMP_TYPES, new = c('temperature', 'normal_temperature', 'normal_temperature_sd'))
  
  w[, date := as.IDate(date, format = '%m/%d/%Y')]
  
  d <- merge(d, w, by = c('date', 'weather_station'))
  
  d[, `:=`(temperature_high = qnorm(.90, mean = normal_temperature, sd = normal_temperature_sd)
           , temperature_mid_high = qnorm(.70, mean = normal_temperature, sd = normal_temperature_sd)
           , temperature_low = qnorm(.10, mean = normal_temperature, sd = normal_temperature_sd))]
  
  return(d)
}

get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
  #   Returns loaded packages.
  
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  library(p, character.only = TRUE, verbose = TRUE)
}

tier <- function(pred, base, lower, upper){
  # Calculate the electricity usage in a given tier
  #
  # Args:
  #   pred: The column of values being tiered
  #   base: the baseline usage used for splitting out the tiers
  #   lower: the lower limit for the tier of interest as a multiplier
  #   upper: the upper limit for the tier of interest as a multiplier
  #
  # Returns:
  #   The electricity usage in a given tier as a column of numeric values
  
  return(ifelse((pred < base * upper) & (pred > base * lower), 
                pred - base * lower,
                ifelse(pred >= upper * base,
                       (upper - lower) * base, 
                       0)))
}

# Calculate the distance between two points on a sphere
# Ref: http://www.movable-type.co.uk/scripts/latlong.html
haversine <- function(lat1, long1, lat2, long2){
  lat1 <- lat1 * pi / 180 # Convert degrees to radians
  lat2 <- lat2 * pi / 180
  
  dlong <- (long2 - long1) * pi / 180
  dlat <- lat2 - lat1
  
  earth_radius <- 3959 # miles,  6371 # km
  a = sin(dlat / 2) * sin(dlat / 2) # + cos(lat1) * cos(lat2) * sin(dlong / 2) * sin(dlong / 2)
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d = earth_radius * c
  return(d)
}

# Simple test
round(haversine(37.7749, -122.4194, 37.98608,-122.3352), 3) ==  14.592
# Test to see that they get the same result
#library(geosphere)
#distHaversine(c(-122.4194, 37.7749), c(-122.3352, 37.98608), 3959)

# Create an message, value entry in a log
entry <- function(df, message, value1){
  return(rbind(df, data.frame(message = message, value1 = value1)))  
}

# Calculate the R squared value
r_squared <- function(y, y_hat){
  y_bar <- mean(y)
  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  rsq <- 1 - (SS_res / SS_tot)
  return(rsq)
}

# Calculate coefficint of variation
cv <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)) / mean(y))
}

rmse <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)))
}

# Create a CDD variable
get_cdd <- function(temp, base){
  return(ifelse(temp - base > 0, round(temp - base, 1), 0))
}

get_hdd <- function(temp, base){
  return(ifelse(base - temp > 0, round(base - temp, 1), 0))
}



# Evaluate the relationship
plot_yoy <- function(x, alpha = 0.5, title = 'Year over Year Usage'){
  cv <- round(x[,cv(tot_usg_kwh_b, tot_usg_kwh_a)], 2)
  n <- x[,.N]
  return(ggplot(x, aes(x = tot_usg_kwh_a, y = tot_usg_kwh_b)) + 
           geom_point(alpha = alpha) + 
           geom_smooth(method = 'lm') + 
           geom_text(data = NULL, x = 250, y = 4000, label = paste('cv =', cv)) + 
           # geom_text(data = NULL, x = 250, y = 3800, label = paste('n =', n)) + 
           theme(text = element_text(size = 16)) + 
           geom_abline() + 
           scale_x_continuous(name = 'July 2013 Usage (kwh)', limits = c(0, 4000)) + 
           scale_y_continuous(name = 'July 2014 Usage (kwh)', limits = c(0, 4000)) + 
           ggtitle(title)
  )
}


# Prep environment and load functions
get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
  #   Returns loaded packages.
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, lib = 'C:/Program Files/R/R-3.3.0/library' )
  library(p, character.only = TRUE, verbose = TRUE)
}

get_time <- function(print_message, start_time) {
  # Print out the time associated with a print message
  #
  # Args:
  #   print_message: Description of the task being timed as a string.
  #   start_time: Reference point for the timer.
  #
  # Returns:
  #   Print out of the ellapsed time along with a printed message.
  
  tm <- unname(((proc.time() - start_time) / 60)['elapsed'])
  print(paste(print_message, round(tm, 2), sep = ': '))
}

get_hdd <- function(temp, base){
  return(ifelse(base - temp > 0, round(base - temp, 1), 0))
}


set_zero = function(DT) {
  # Identify NA values and set to zero
  #
  # Args:
  #   DT: data table
  #
  # Returns:
  #   data table with NA values replaced with zeros.
  
  
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}


get_segment_subset <- function(df_train) {
  # Return three customer segment ids, corresponding to highest, lowest, 
  # and middle-of-the-road customer usage averages. Customer segments
  # correspond to the terminal node on the decision tree.
  #
  # Args:
  #   df_train: The in-sample hourly data after the segmentation has occured
  #
  # Returns:
  #   Three customer segment ids associated with high, medium, low customer
  #   usage customer segments.
  
  segments <- df_train[, .(usage = mean(usage), 
                           n = uniqueN(sp_id)), 
                       by = segment][order(usage)][,segment]
  
  return(c(segments[1],
           segments[floor(0.5 * length(segments))],
           segments[length(segments)]))
}

msd <- function(x, y) sqrt(mean((x - y) ^ 2))

r_squared <- function(y, y_hat){
  # Calculate the R squared value to calculate the accuracy of a set of 
  # predictitions
  #
  # Args:
  #   y: The observed values as a single column in a dataframe.
  #   y_hat: The predicted values as a single column in a dataframe.
  #
  # Returns:
  #   The r squared value as a single number.
  
  
  y_bar <- mean(y)
  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  rsq <- 1 - (SS_res / SS_tot)
  return(rsq)
}



trim_model <- function(x){
  # Remove unnecessary elements from a model object 
  #
  # Args:
  #   x: the model object.
  #
  # Returns:
  #   The model object itself, minus the extraneous information.
  
  x$rss <- c()
  x$rsq <- c()
  x$gcv <- c()
  x$grsq <- c()
  x$bx <- c()
  x$fitted.values <- c()
  x$residuals <- c()
  x$rss.per.response <- c()
  x$rsq.per.response <- c()
  x$gcv.per.response <- c()
  x$grsq.per.response <- c()
  x$leverages <- c()
  x$nprune <- c()
  x$penalty <- c()
  x$nk <- c()
  x$thresh <- c()
  x$termcond <- c()
  x$weights <- c()
  x$call <- c()
  x$namesx.org <- c()
  return(x)
}

segment <- function(usage_value){
  # Segment customer using a decision tree model, plot the results, and 
  # save the model.
  #
  # Args:
  #   usage_value: the target variable to segment on.
  #
  # Returns:
  #   The customer segments as a column in a dataframe.
  
  # Fit tree
  tree_params = rpart.control(maxdepth = 10, 
                              minbucket = 30,
                              cp = 0.001)
  
  tree <- rpart(paste(usage_value, '~', paste(features, collapse=' + ')), 
                d, 
                control = tree_params)
  
  # Assign group membership
  tree$frame$target <- tree$frame$yval
  tree$frame$yval <- as.numeric(row.names(tree$frame))
  segment <- predict(tree, d)
  tree$frame$yval <- tree$frame$target
  
  # Plot Decision Tree
  dev.new(width=11.5, height=17)
  fancyRpartPlot(tree, main = "Customer Segments", palettes=c('BuGn'), sub = "")
  return(segment)
}

fit_predict <- function(usage_value, 
                        splits = 'segment',
                        index, 
                        df_train = train,
                        df_test = test) {
  
  # Fit MARS model, predict on out of sample, and unadjust usage 
  #
  # Args:
  #   usage_value: string representing the target varaible in the MARS model
  #   splits: column name(s) (as a string) with the variable(s) create ...
  #           different models across
  #   index: column name (as a string) with the indexed usage
  #   df_train: data frame with the in sample observations
  #   df_test: data frame with out of sammple observations
  #   
  # Returns:
  #   Returns df_test with the predictions adjusted by the index 
  
  fit <- dlply(df_train,
               splits,
               function(x) earth(as.formula(paste(usage_value, '~ temp')),
                                 data = x,
                                 Scale.y = TRUE))
  
  # Trim model objects
  fit <- lapply(fit, trim_model)
  
  # Save the list of models
  saveRDS(fit, file = 'E:/CEA_DATA/model_export/regression_models.rds')
  
  # Generate predictions on in-sample data
  df_train <- ddply(df_train, 
                    splits, 
                    function(x)
                      transform(x,
                                yHat = predict(fit[[paste(
                                  as.matrix(x[1,splits]),
                                  collapse=".")]],
                                  newdata = x)[,1] ))  
  # Calculate the residuals
  df_train$residuals <- df_train[,usage_value] - df_train$yHat
  
  # Calculate the average residual
  avg_residual <- df_train %>% 
    group_by(sp_id, hour) %>% 
    summarize(
      average_residual = median(residuals)
    )
  
  # Generate the estimated usage based on temperature actual
  df_test <- ddply(df_test, 
                   splits, 
                   function(x)
                     transform(x,
                               yHat = predict(fit[[paste(
                                 as.matrix(x[1,splits]),
                                 collapse=".")]],
                                 newdata = x)[,1] ))
  
  # Join the residuals from the training set to the test set 
  df_test <- left_join(df_test, 
                       avg_residual, 
                       by=c('sp_id'= 'sp_id',
                            'hour' = 'hour'))
  
  # Forecast the usage based off of temperature and the ...
  # ... average residuals
  df_test$pred_adj <- df_test$average_residual + df_test$yHat
  
  # Multiply by the hourly index
  df_test$pred <- df_test$pred_adj * df_test[,index]
  
  
  df_test$average_residual <- NULL
  df_test$pred_adj <- NULL
  
  return(df_test)
}

find_var_hourly <- function(DT){
  # Calculate the sum of the covariance between hour-to-hour errors 
  #
  # Args:
  #   DT: data table with aggregated on the hourly-basis
  #   
  # Returns:
  #   a single value for each customer, month combination
  
  DT.W <- copy(DT)
  DT.W <- dcast.data.table(data = DT.W, 
                           formula = sp_id + date + month ~ hour, 
                           value.var = 'residual')
  DT.W[,.(var_hourly = sum(cov(.SD, use = "complete.obs"))), 
       keyby = .(sp_id, month), .SDcols = HOUR_COLS_MELT ]
}
# 4:27

find_corr_coef <- function(DT){
  # Calculate the sum of the correlation matrix between day-to-day errors
  #
  # Args:
  #   DT: data table with aggregated on the hourly-basis
  #   
  # Returns:
  #   a single value for each customer, month combination
  
  
  DT <- DT[,lapply(.SD, sum), by = .(sp_id, month, date), 
           .SDcols = c('residual', 'usage', 'pred')][order(sp_id, date)]
  DT.M <- copy(DT)
  DT.M[, residual_lag := shift(residual, n = 1, type = "lag"), 
       by = .(sp_id)] # residual_lag
  DT.M[, .(corr_coef = cor(residual, residual_lag, use = 'complete.obs'), 
           n=.N, 
           usage = sum(usage),
           pred = sum(pred)), 
       by = .(sp_id, month)
       ][,.(corr_daily = sum(n, 2*(n - seq(1, n - 1))*corr_coef^seq(1, n - 1)),
            usage = sum(usage),
            n = sum(n),
            pred = sum(pred)), 
         keyby = c('sp_id', 'month')
         ]
}

tier <- function(pred, base, lower, upper){
  # Calculate the electricity usage in a given tier
  #
  # Args:
  #   pred: The column of values being tiered
  #   base: the baseline usage used for splitting out the tiers
  #   lower: the lower limit for the tier of interest as a multiplier
  #   upper: the upper limit for the tier of interest as a multiplier
  #
  # Returns:
  #   The electricity usage in a given tier as a column of numeric values
  
  return(ifelse((pred < base * upper) & (pred > base * lower), 
                pred - base * lower,
                ifelse(pred >= upper * base,
                       (upper - lower) * base, 
                       0)))
}

add_time <- function(dt){
  # add date time variables to a data table
  #
  # Args:
  #   dt: name of data table
  #
  # Returns:
  #   year, month, year_day, month_day, season, day name and weekend/weekday indicator
  dt[,`:=` (month = data.table::month(date))]
  # dt <- dt[!month %in% c(4:10)]
  dt[,`:=` (year = data.table::year(date),
            year_day = data.table::yday(date),
            month_day = data.table::mday(date),
            month_name=month.abb[data.table::month(date)]),
     ][,season := ifelse(month %in% c(4:10),'summer', 'winter')]
  dt
}

col_types <- function(dir, pat, char_cols, num_cols, fctr_cols, n = 5, ...){
  # read in first five columns of a file to define class of variables
  #
  # Args:
  #   dir: directory of the file
  #   pat: pattern to match
  #   char_cols: columns to assign character class
  #   num_cols: columns to assign number class
  #   n: number of rows to read
  # Returns:
  #   column name and classes for increased fread speed
  
  read_data <- dir(path = dir, pattern = pat, full.names = TRUE, ...)
  init_5 <- read.table(read_data[[1]]
                       , na.strings = '?'
                       , header = TRUE
                       , stringsAsFactors = TRUE
                       , nrows = n
                       , check.names = FALSE
                       , sep = '\t')
  # init_5[,fctr_cols] <- sapply(init_5[,fctr_cols], as.factor)
  init_5[,char_cols] <- sapply(init_5[,char_cols], as.character)
  init_5[,num_cols] <- sapply(init_5[,num_cols], as.numeric)
  classes <- sapply(init_5, class)
  list('classes' = classes, files = read_data)
}

rmse <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)))
}


# Colors
CP <- list('blue'='#1f77b4', 'orange'='#ff7f0e',
           'green' = '#2ca02c', 'red' = '#d62728',
           'purple'='#9467bd', 'brown' = '#8c564b',
           'pink' = '#e377c2', 'grey' = '#7f7f7f',
           'yellow' = '#bcbd22', 'teal' = '#17becf')

# Calculate the distance between two points on a sphere
# Ref: http://www.movable-type.co.uk/scripts/latlong.html
haversine <- function(lat1, long1, lat2, long2){
  lat1 <- lat1 * pi / 180 # Convert degrees to radians
  lat2 <- lat2 * pi / 180
  
  dlong <- (long2 - long1) * pi / 180
  dlat <- lat2 - lat1
  
  earth_radius <- 3959 # miles,  6371 # km
  a = sin(dlat / 2) * sin(dlat / 2) # + cos(lat1) * cos(lat2) * sin(dlong / 2) * sin(dlong / 2)
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d = earth_radius * c
  return(d)
}

# Simple test
round(haversine(37.7749, -122.4194, 37.98608,-122.3352), 3) ==  14.592
# Test to see that they get the same result
#library(geosphere)
#distHaversine(c(-122.4194, 37.7749), c(-122.3352, 37.98608), 3959)

# Create an message, value entry in a log
entry <- function(df, message, value1){
  return(rbind(df, data.frame(message = message, value1 = value1)))  
}

# Calculate the R squared value
r_squared <- function(y, y_hat){
  y_bar <- mean(y)
  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  rsq <- 1 - (SS_res / SS_tot)
  return(rsq)
}

# Calculate coefficint of variation
cv <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)) / mean(y))
}

rmse <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)))
}

# Create a CDD variable
get_cdd <- function(temp, base){
  return(ifelse(temp - base > 0, round(temp - base, 1), 0))
}

read_usage_long <- function(){
  
  cols <- c(
    'char_prem_id'
    , 'sa_id'
    , 'acct_id'
    , 'nrml_yyyymm'
    #   , 'baseline_usg_kwh'
    # , 'care_ind'
    #   , 'cmprs_rt_sched_cd'
    , 'deriv_baseline_terr_cd'
    , 'opr_area_cd'
    # , 'opr_area_cd'
    # , 'rt_sched_cd'
    # , 'solar_ind'
    , 'tot_usg_kwh'
    # , 'tou_ind'
    #   , 'split_ind'
    # , 'med_allot_qty'
    #   , 'net_mtr_ind'
    #   , 'revn_amt'
    #   , 'max_tier'
    # , 'fera_ind'
  ) 
  
  file_location <- 'C:/Users/J8DG/Documents/data/'
  
  file_names <- list.files(file_location, pattern = "kamal_monthly_" )
  
  x <- rbindlist(lapply(file_names, function(y) fread(paste0(file_location,y)
                                                      , stringsAsFactors = FALSE
                                                      , na.strings = '?'
                                                      , integer64 = 'character'
                                                      , select = cols)))
  
  
  # Remove duplicate columns
  x <- x[,.SD, .SDcols = c(1:6, 9)]
  
  setkey(x, acct_id, char_prem_id)
  
  if(is.null(key(x))){
    stop('Must set key before pre_processing')
  }
  
  x <- x[deriv_baseline_terr_cd == 'W']
  
  lg <- entry(data.frame(), 'Initial Customers', x[,.N, by = key(x)][,.N])
  
  x[, date := as.IDate(as.character(paste0(nrml_yyyymm, '01')), format = '%Y%m%d')]
  
  x[,`:=` (year = data.table::year(date),
           month = data.table::month(date))]
  
  # Subset for 2012 + for now because we don't have weather for earlier
  x <- x[year > 2011,]
  
  x[,`:=` (min_date = min(date),
           max_date = max(date),
           num_sa = uniqueN(sa_id),
           num_zero = sum(tot_usg_kwh == 0)), 
    by = key(x)]
  
  x <- x[(min_date <= '2012-01-01') & (max_date >= '2015-12-01'),]
  
  lg <- entry(lg, 'Stationary Customers', x[,.N, by = key(x)][,.N])
  
  x <- x[(num_sa == 1),]
  
  lg <- entry(lg, 'Constant SA Customers', x[,.N, by = key(x)][,.N])  
  
  x <- x[(num_zero == 0),]
  
  lg <- entry(lg, 'Customers No Zero Usage', x[,.N, by = key(x)][,.N])
  
  x[, `:=`(nrml_yyyymm = NULL,
           min_date = NULL,
           max_date = NULL,
           num_zero = NULL)]
  
  setkey(x, acct_id, char_prem_id, date) # This sorts by key
  
  x[,`:=`(prior = data.table::shift(tot_usg_kwh, n = 12L, type = 'lag'))
    , by = .(char_prem_id, acct_id)]
  
  print(lg)
  
  return(x)
  
}

read_weather <- function(){
  
  # Read data into memory
  w <- fread(paste0('C:/Users/J8DG/Documents/data/2012_2015_daily_weather.txt')
             , na.strings = '?'
             , integer64 = 'character')
  
  # Define date fields
  w[, wea_dt := as.IDate(wea_dt, format = '%m/%d/%Y')]
  w[, month := month(wea_dt)]
  
  # Define Weather Station Type: L is PG&E, K is National Weather Service
  w$weather_station_type = ifelse(substr(w$weather_station, 1, 1) == 'L', 'PGE', 'NWS')
  
  # Do a groupby to elimnate duplicate values 
  w <- w[, .(avg_val = mean(avg_val))
         , by = .(wea_year, month, wea_dt, baseline_terr_cd, opr_area_cd, weather_station,weather_station_type, wea_type)]
  
  # Conver to wide format
  w <- dcast(w
             , formula = 'wea_year + wea_dt + baseline_terr_cd + opr_area_cd + 
             weather_station + month + weather_station_type ~ wea_type'
             , value.var = 'avg_val')
  
  # Convert Names to make them easier to access
  setnames(w, old = colnames(w), new = gsub(x = colnames(w), pattern = ' ', replacement = '_'))
  
  # Calculate CDD from the hourly/30m temperature actuals
  w[,`:=`(cdd_55 = get_cdd(Temperature, 55),
          cdd_60 = get_cdd(Temperature, 60),
          cdd_65 = get_cdd(Temperature, 65),
          cdd_70 = get_cdd(Temperature, 70),
          cdd_75 = get_cdd(Temperature, 75))]
  
  # Get monthly values from daily values
  w <- w[,lapply(.SD, sum)
         , by = .(wea_year, month, baseline_terr_cd, opr_area_cd, weather_station_type, weather_station)
         , .SDcols = c('cdd_55', 'cdd_60', 'cdd_65', 'cdd_70', 'cdd_75')]
  
  
  # Subset for PGE weather stations
  w <- w[weather_station_type == 'PGE',]
  
  return(w)
}

read_usage_wide <- function(f1, f2){
  
  start_time <- proc.time()
  
  cols <- c(
    'char_prem_id'
    , 'sa_id'
    , 'acct_id'
    , 'nrml_yyyymm'
    #   , 'baseline_usg_kwh'
    , 'care_ind'
    #   , 'cmprs_rt_sched_cd'
    , 'deriv_baseline_terr_cd'
    , 'opr_area_cd'
    , 'rt_sched_cd'
    , 'solar_ind'
    , 'tot_usg_kwh'
    , 'tou_ind'
    #   , 'split_ind'
    , 'med_allot_qty'
    #   , 'net_mtr_ind'
    #   , 'revn_amt'
    #   , 'max_tier'
    , 'fera_ind'
  ) 
  
  file_location <- 'C:/Users/J8DG/Documents/data/'
  
  x1 <- fread(paste0(file_location, f1), na.strings = '?', integer64 = 'character',
              select = cols)
  
  x2 <- fread(paste0(file_location, f2), na.strings = '?', integer64 = 'character', 
              select = cols)
  
  lg <- entry(data.frame(), 'Customers in 2014', x1[,.N])
  
  lg <- entry(lg, 'Customers in 2015', x2[,.N])
  
  x <- merge(x1, x2, by = c('acct_id', 'char_prem_id'), suffixes = c('_a', '_b'))
  
  lg <- entry(lg, 'In 2014 and 2015', x[,.N])
  
  x[, `:=`(nrml_yyyymm_a = as.IDate(paste0(nrml_yyyymm_a, '01'), format = '%Y%m%d')
           ,nrml_yyyymm_b = as.IDate(paste0(nrml_yyyymm_b, '01'), format = '%Y%m%d'))]
  
  x[, `:=`(year_a = year(nrml_yyyymm_a)
           , year_b = year(nrml_yyyymm_b)
           , month_a = month(nrml_yyyymm_a)
           , month_b = month(nrml_yyyymm_b))]
  
  x[, `:=`(
    sa_change = sa_id_a != sa_id_b,
    got_solar = solar_ind_a == 'N' & solar_ind_b == 'Y',
    lost_solar = solar_ind_a == 'Y' & solar_ind_b == 'N',
    got_tou = !grepl('E7', rt_sched_cd_a) & grepl('E7', rt_sched_cd_b),
    lost_tou = grepl('E7', rt_sched_cd_a) & !grepl('E7', rt_sched_cd_b),
    got_care = care_ind_a == 'N' & care_ind_b == 'Y',
    lost_care = care_ind_a == 'Y' & care_ind_b == 'N',
    got_fera = fera_ind_a == 'N' & fera_ind_b == 'Y',
    lost_fera = fera_ind_a == 'Y' & fera_ind_b == 'N',
    got_medical_condition = med_allot_qty_a < med_allot_qty_b,
    lost_medical_condition = med_allot_qty_a > med_allot_qty_b, 
    usg_change = tot_usg_kwh_b - tot_usg_kwh_a
  )][, `:=`(any_change = 
              (sa_change == 'TRUE') |
              (got_solar == 'TRUE') | (lost_solar == 'TRUE') |
              (got_tou == 'TRUE') | (lost_tou == 'TRUE') |
              (got_care == 'TRUE') | (lost_care == 'TRUE') | 
              (got_fera == 'TRUE') | (lost_fera == 'TRUE') |
              (got_medical_condition == 'TRUE') | (lost_medical_condition == 'TRUE')
  )]
  
  cols <- c("got_solar", "got_tou", "got_care", "got_fera", "got_medical_condition", 
            "lost_solar", "lost_tou", "lost_care", "lost_fera", "lost_medical_condition",
            "sa_change")
  
  lg <- rbind(lg, melt(data = x[, lapply(.SD, function(x) sum(x == 'TRUE')), 
                                .SDcols = cols], measure.vars = cols, 
                       variable.name = 'message', value.name = 'value1'))
  
  write.table(lg, paste0(file_location, 'log.txt'))
  
  print((proc.time() - start_time)['elapsed'])
  
  print(lg)
  
  return(x)
}

# Evaluate the relationship
plot_yoy <- function(x, alpha = 0.5, title = 'Year over Year Usage'){
  cv <- round(x[,cv(tot_usg_kwh_b, tot_usg_kwh_a)], 2)
  n <- x[,.N]
  return(ggplot(x, aes(x = tot_usg_kwh_a, y = tot_usg_kwh_b)) + 
           geom_point(alpha = alpha) + 
           geom_smooth(method = 'lm') + 
           geom_text(data = NULL, x = 250, y = 4000, label = paste('cv =', cv)) + 
           # geom_text(data = NULL, x = 250, y = 3800, label = paste('n =', n)) + 
           theme(text = element_text(size = 16)) + 
           geom_abline() + 
           scale_x_continuous(name = 'July 2013 Usage (kwh)', limits = c(0, 4000)) + 
           scale_y_continuous(name = 'July 2014 Usage (kwh)', limits = c(0, 4000)) + 
           ggtitle(title)
  )
}

usg_temp_lm <- function(x) {
  lm(usg ~ temp, data = x)
}


model_lift <- function(df, baseline, model_value){
  ## function to calculate the percent difference from two periods
  df_new <- df %>% 
    mutate(model_delta = baseline - model_value,
           model_pct = round(model_delta/baseline * 100,1),
           model_lift = ifelse(model_value == baseline, "Baseline",
                               ifelse(model_value < baseline, 'Improvement'
                                      , "No Improvement")))
  df_new
}

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

# Load packages
sapply(PACKAGES, get_packages)


# TOU periods -------------------------------------------------------------

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
