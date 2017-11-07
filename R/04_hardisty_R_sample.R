# Script Details -----------------------------------------------------
# This code was written to test the in-memory processing capacity of local machines 
# prior to investing in cloud or in database processing power as well as test the quality 
# of predictions at time of use (TOU) period. Upstream data processing was completed in R 
# and saved locally to flat files of increasing sizes. The model script reads in files of 
# a pre-determined size: train interval usage data (usage in 2012) at TOU period aggregation, 
# test interval usage data (usage in 2013) at TOU period aggregation, and weather data for train 
# and test years. We train the model on actual temperature and actual usage and predict on forecasted 
# temperature for 2013. Actual 2013 usage and billing data were used to compare the binary 
# classification of high or non-high at a monthly level compared to the Constant Usage Model. 
# 
# The assumption of the Constant Usage Model, which we are trying to test with this script, 
# is that daily usage year over year is the same for each customer. We “predict” usage for 
# the Constant Usage model by assigning the daily usage for each customer from 2012 as 2013 
# predicted usage

# Prepare Environment -----------------------------------------------------

# load helper function

get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
    # Installs packages as needed  
    # Loads installed packages 
  
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  library(p, character.only = TRUE, verbose = FALSE)
}

# list of packages for analysis
PACKAGES <- c('lubridate','earth','data.table','ggplot2','stringr','tidyr','purrr','broom','dplyr')

# load or install packages with helper function
sapply(PACKAGES, get_packages)

# determine location of source .RDA files
DATA_IN_LOC <- getwd()

OUTPUT_DIR <- 'hardisty_r_example'
# determine location of model results and plots
DATA_OUT_LOC <- paste0(DATA_IN_LOC,'/',OUTPUT_DIR,'/', 'model_output',sep = '/')
#defined by user  

# load clean data for modeling
load('hardisty_r_example/TRAIN.rda')
load('hardisty_r_example/TEST.rda')
load('hardisty_r_example/CONS_USG.rda')
load('hardisty_r_example/TEST_ACTUAL.rda')

# Fit and predict  -----------------------------------------------
# begin timing process
start_model_time <- proc.time()

# fit model on train data
TRAIN_MODEL <- TRAIN %>%
  group_by(train_indicator, model_indicator, CUSTOMER_KEY, TOU_CD) %>% 
  nest(.key = TRAIN) %>% 
  mutate(models = map(TRAIN, ~ earth(Y ~ X, data = ., nprune = 3)), # train model
         fitted = map2(models, TRAIN, predict)) # make in sample predictions

# nest test data to match train data format
TEST_MODEL <- TEST %>%
  group_by(train_indicator, model_indicator, CUSTOMER_KEY, TOU_CD) %>% 
  nest(.key = TEST)

# merge train and test to make predictions on customers with observations in both train and test years
TRAIN_TEST <- left_join(TRAIN_MODEL, TEST_MODEL, by = c("CUSTOMER_KEY",'model_indicator','TOU_CD'))

# make out of sample predictions
TRAIN_TEST <- TRAIN_TEST %>% 
  mutate(predicted_usage = map2(models, TEST, predict) # need to edit code to use X +- SD for temp scenarios
)

print('Predictions Complete')
print(proc.time() - start_model_time)

# Format output predictions  -----------------------------------------------

# unnest training data for inspection
TEMP_MODEL_RESULTS <- TRAIN_TEST %>% 
  select(train_indicator = train_indicator.y, model_indicator, CUSTOMER_KEY, TOU_CD, TEST, predicted_usage) %>%
  unnest() %>% # unnests all elements in nested lists; needs to be edited to return only required data
  as.data.table() # easier manipulation with data.table

# Compare temperature and constant usage model  ------------------------------------------------------
# select only meaningful comparison columns
comp_cols <- c('CUSTOMER_KEY', 'Month', 'Day', 'Year','mdy','predicted_usage','GEO_KEY'
               , 'MODEL_ID','train_indicator','model_indicator','year_day', 'week_day','season'
               , 'TOU_CD')
base_cols <- c('CUSTOMER_KEY', 'MODEL_ID','Actual_Usage','median_temp','train_indicator'
               ,'model_indicator','year_day', 'TOU_CD')

# set column order for rbind in later step
col_order <- c("CUSTOMER_KEY", "Month", "Day", "Year", "mdy", "predicted_usage", 
               "GEO_KEY", "MODEL_ID", "train_indicator", "model_indicator", 
               "year_day", "week_day", "season", "TOU_CD")

TEMP_MODEL_COMP <- copy(TEMP_MODEL_RESULTS[,comp_cols, with = FALSE])
setkey(TEMP_MODEL_COMP, CUSTOMER_KEY, year_day, TOU_CD, MODEL_ID)
setcolorder(TEMP_MODEL_COMP, col_order)

CONS_USG_COMP <- copy(CONS_USG[,comp_cols, with = FALSE])
setkey(CONS_USG_COMP, CUSTOMER_KEY, year_day, TOU_CD, MODEL_ID)
setcolorder(CONS_USG_COMP, col_order)

TEST_ACTUAL_COMP <- copy(TEST_ACTUAL[,base_cols, with = FALSE])
setkey(TEST_ACTUAL_COMP, CUSTOMER_KEY, year_day, TOU_CD, MODEL_ID)

# merge actual with temperature model
TEMP_MODEL_QUALITY <- merge(TEMP_MODEL_COMP, TEST_ACTUAL_COMP, suffixes = c(x = '_prediction', y='_actual'))
CONS_USG_QUALITY <- merge(CONS_USG_COMP, TEST_ACTUAL_COMP, suffixes = c(x = '_prediction', y='_actual'))

FULL_MODEL_QUALITY <- rbind(TEMP_MODEL_QUALITY, CONS_USG_QUALITY)
FULL_MODEL_QUALITY[,residual := Actual_Usage - predicted_usage]
FULL_MODEL_QUALITY[,on_off_peak := ifelse(week_day %in% c('Sat','Sun'), 'Off-Peak','On-Peak'),]

# generate plots ----------------------------------------------------------
# split on meaningful factors
FULL_MODEL_QUALITY_LIST <-  FULL_MODEL_QUALITY %>% 
  split(list(.$GEO_KEY, .$MODEL_ID), drop = TRUE)

# CREATE PLOTS USING DATA SPLIT ON BASELINE TERRITORY CODE  ---------------
ALL_PLOTS <- FULL_MODEL_QUALITY_LIST %>%
  map(~ ggplot(., aes(x = median_temp, y = residual)) + 
        geom_jitter(aes(color = model_indicator_prediction , shape = on_off_peak), alpha = .5) +
        # facet data
        facet_wrap(~Month) +
        # plot title variables generated by split factors
        labs(
          title = paste(.$Year, "Daily residuals comparing constant usage and temperature model for"
                        ,.$GEO,'geographic area'),
          subtitle = paste("Residuals calculated against",.$train_indicator_actual,'for sample group',.$MODEL_ID),
          caption = "Temperature Model predicts usage based on average monthly temperature;
                          Constant Usage Model predicts usage based on actual usage for prior year",
          x = "Median Temperature",
          y = "Daily Residuals"
          ))
        

# View plot results -------------------------------------------------------
ALL_PLOTS

# write out plots results -------------------------------------------------
PLOT_OUTPUT_PATH <- paste0(DATA_OUT_LOC,'model_comparison_plots_geo_code_'
                           ,tolower(names(FULL_MODEL_QUALITY_LIST)),'.png')

walk2(PLOT_OUTPUT_PATH, ALL_PLOTS, ggsave, width = 11, height = 8.5)

# write out model results -------------------------------------------------
MODEL_OUTPUT_PATH <- paste0(DATA_OUT_LOC,'model_comparison_data_geo_code_',tolower(names(FULL_MODEL_QUALITY_LIST)),'.txt')

walk2(FULL_MODEL_QUALITY_LIST, MODEL_OUTPUT_PATH, write.table, row.names = FALSE, append = F)


# clean environment -------------------------------------------------------

# rm(list = ls())
