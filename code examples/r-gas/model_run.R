# # Prepare Environment -----------
# clean environment
rm(list = ls()); gc()
st <- proc.time()
# set data source constants 


# columns for manipulating weather data
march_dts <- c('2013-03-10', '2014-03-09','2015-03-08','2016-03-13')  # <= UPDATE AS NEEDED
nov_dts <- c('2013-11-03', '2014-11-02','2015-11-01','2015-11-06')  # <= UPDATE AS NEEDED
out_cols <- c('sp_id', 'date', 'hour', 'month', 'usage', 'pred', 'temp') # columns for subsetting

# columns for prediction data
melt_cols <- c('sp_id','sa_id', 'month', 'segment', 'corr_daily', 'n', 'var_hourly', 
              'standard_error', 'z_score') # columns for prediction transformation prior to billing

val_cols <- c('actual_train','actual_test','old', 'p05','p50', 'p95')

sub_cols <- c('sp_id', 'sa_id','month', 'standard_error', 'percentile','predicted','segment')

# variables for prediction data
scaling_constant <- 2.0



