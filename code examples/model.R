library(earth)
library(data.table)
library(ggplot2)
library(plyr)
library(tidyr)
library(purrr)
library(dplyr)

# TO-DO
# - Read and process output from the super computer
# - Add complete observations check into the pre-processing function
# - Clean up the directory code

source('eua/r/initialize.R')

start_time <- proc.time()

for (cz in c('w', 't', 'x')){
  for (smpl in c('1', '2', '3')){
    
    CLIMATE_ZONE <- cz
    SAMPLE <- smpl
    DATA_LOC <- paste0('C:/Users/J8DG/Documents/data/sample',  SAMPLE, '/')
    OUT_NAME <- paste0('predictions_monthly_', CLIMATE_ZONE, '.csv')
    EXPECTED_OBS <- 840 # change to 365 for daily
    
    # Read and format  -----------------------------------------------
    
    
    train <- read_process(DATA_LOC, paste0('2014_tou_kamal_', CLIMATE_ZONE, '.txt'), 2014)
    test <- read_process(DATA_LOC, paste0('2015_tou_kamal_', CLIMATE_ZONE, '.txt'), 2015)
    
    print('Data read complete')
    print(proc.time() - start_time)
    
    # Filter for complete records
    complete_test <- factor(test[, .N, by = customer][N == EXPECTED_OBS]$customer)
    complete_train <- factor(train[, .N, by = customer][N == EXPECTED_OBS]$customer)
    test <- test[customer %in% complete_test & customer %in% complete_train,]
    train <- train[customer %in% complete_test & customer %in% complete_train,]
    
    # Set variable names for consistency
    train[, `:=`(customer = factor(customer), y = usage, X = temperature)]
    test[, `:=`(customer = factor(customer), y = usage, X = temperature_mid_high)]
    
    # Order by customer and date
    train <- train[order(customer, period, date)]
    test <- test[order(customer, period, date)]
    
    # Fit and predict  -----------------------------------------------
    start_time <- proc.time()
    d <- rbind(train, test) %>% 
      mutate(
        train_indicator = factor(ifelse(year == 2015, 'test', 'train'))
      ) %>% 
      group_by(train_indicator, customer, period) %>% 
      nest() %>% 
      spread(key = train_indicator, value = data) %>% 
      mutate(
        # Fit the models
        models = map(train, ~ earth(y ~ X, data = ., nprune = 3)),
        
        # Make insample predictions
        fitted = map2(models, train, predict),
        
        # Make out of sample predictionss
        pred_norm = map2(models, test, predict)
      )
    
    print('Predictions Complete')
    print(proc.time() - start_time)
    
    # Format output predictions  -----------------------------------------------
    train <- cbind(train, d %>% unnest(fitted) %>% select(fitted))
    test <- cbind(test, d %>% unnest(pred_norm) %>% select(pred_norm))
    
    # Add usage lag to the test set
    test <- merge(test, train[, .SD, .SDcols = c('month', 'day', 'usage','period', 'customer')],
                  by = c('month', 'day', 'period', 'customer'),
                  suffixes = c('', '_lag'))
    test <- as.data.table(test)
    
    # Add/format variables
    test[, actual_train := usage_lag]
    setnames(test, old = c('usage', 'usage_lag'), new = c('actual_test', 'pred_usage_lag'))
    test[, nrml_yyyymm := as.character(sprintf('%s%02i', year, as.numeric(match(month, month.abb))))]
    
    # Aggregate on the monthly and write out
    test_monthly <- test[year == 2015, lapply(.SD, sum)
                         , by = c('acct_id', 'prem_id', 'nrml_yyyymm', 'month', 'period')
                         , .SDcols = c('actual_train', 'actual_test', 'pred_norm', 'pred_usage_lag')]
    
    write.table(test_monthly, paste0(DATA_LOC, OUT_NAME), sep = ',', row.names = FALSE)
    
  }
}

# Evaluate output  ------------------------------------------------------
random_customers <- factor(sample(unique(train$customer), 5))
print(ggplot(train[customer %in% random_customers, ], aes(x = temperature)) +
        geom_line(aes(y = fitted), size = 1, color = CP$blue) +
        facet_grid(customer ~ period) +
        geom_point(aes(y = usage), alpha = .1) +
        ggtitle(expression(atop('Modeling at the Customer/Period Level'
                                , atop(italic('Randomly Selected Customers (in sample), Rows are Customers')
                                       , "")))) +
        ylab('Electricity Usage (KwH)') +
        xlab('Temperature'))

metrics <- test_monthly[month == 'May' | month == 'Aug'
                        , .(rmse_new = rmse(actual_test, pred_norm)
                            , rmse_old = rmse(actual_test, pred_usage_lag)
                            , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                        , by = .(month, period)]

print(metrics)

print('Model.R complete')
print(proc.time() - start_time)