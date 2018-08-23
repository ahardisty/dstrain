# OUT OF SAMPLE PREDICTIONS ---------------------------------------------------

source('./Phase_3-3/01_functions.R') # load functions if needed (verify path)

rt_sched_cd_model <- 'HETOUA' # rt_sched_cd for modeling
rt_sched_cd <- paste0("'",rt_sched_cd_model,"'") # rt_sched_cd for SQL query
LOWER_MODEL_ID <- 1 #first model group
UPPER_MODEL_ID <- 5 #last model group
model_seq <- (LOWER_MODEL_ID:UPPER_MODEL_ID) # vector of model groups for sequential modeling
DATA_OUT_LOC <- getwd() # default to working directory; change as appropriate

# Set standard deviation levels for temperature scenarios
temp_high <- .85
temp_mid_high <- .50 
# temp_normal: placeholder, no change
temp_mid_low <- .50 
temp_low <-  .85

# Create connection string ----------------------------

# ODBC
DSN <- 'TDDSNP'
UID <- USER
PW <- PWD
ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

# MySQL
library(RMySQL)
ch <- dbConnect(MySQL(),
                user = 'root',
                host = 'localhost',
                dbname = 'RDA_P_REVR')

# Verify Data Connection --------------------------------------------------
# ODBC


# MySQL
dbGetInfo(ch)

# Create SQL query to bring Test data into memory ----------------------------------------------------

baseTbl <- "EUA_TEST"
modelTbl <- "EUA_COMPLETE"

predictQuery <- str_replace_all(paste("SELECT ", 
                                      baseTbl,".usg_dt,", 
                                      baseTbl,".day_of_year,",
                                      baseTbl,".customer,", 
                                      baseTbl,".temp_scenario,", 
                                      modelTbl,".model_id,",
                                      modelTbl,".model_id_sm,",
                                      baseTbl,".tou_cd,",
                                      baseTbl,".X,",
                                      baseTbl,".X_sd,",
                                      rt_sched_cd, "AS rt_sched_cd",
                                      " FROM ", baseTbl, " ",
                                      " JOIN ", modelTbl," ON (",
                                      baseTbl,".customer = ", modelTbl,".customer", 
                                      ") WHERE ",modelTbl,".model_id =",sep="")
                                , "[\r\n]" , "")

predictQuery_list <- map2(.x = predictQuery, .y = model_seq, .f = paste)

# in sample predictions with temperature scenarios

predictQuery <- predictQuery_list[[1]]


predictModel_memory <- function(query = predictQuery, channel = ch, models = MODELS)
{
  start_model_time <- proc.time() 
  # sql query for Test data
  TEST <- dbGetQuery(channel, query)
  
  # create temp scenario values and gather into long format
  TEST <- TEST %>%
    mutate(
      temp_high = X+(X_sd * temp_high),
      temp_mid_high = X+(X_sd * temp_mid_high),
      temp_normal = X,
      temp_mid_low = X-(X_sd * temp_mid_low),
      temp_low= X-(X_sd * temp_low)
      ) %>% 
    rename(model_type = temp_scenario) %>% #rename temp scenario as model type
    rename(temp_norm_actual = X) %>% 
    # filter(customer == '0010305996' & day_of_year %in% c(94:97)) %>% 
    tidyr::gather(key = temp_scenario,value = X, c(temp_high, temp_mid_high, temp_normal, temp_mid_low
                                                  , temp_low))
  
  # nest for joining models
  TEST_MODEL <- TEST %>%
    group_by(model_type, customer, tou_cd, model_id, model_id_sm, temp_scenario, rt_sched_cd) %>% 
    nest(.key = TEST)
  
  # join test data and model data
  TEST <- dplyr::left_join(TEST_MODEL, models, by = c('customer','tou_cd','rt_sched_cd'
                                                      ,'model_id','model_id_sm','temp_scenario'))
  
  # make predictions across multiple temperature scenarios
  TEST <- TEST %>%
    mutate(PRED_VAL = map2(MODEL, TEST %>% map('X'), predict)) %>% 
    mutate(pred_date = format(Sys.Date(), '%Y-%m-%d'),
           usg_dt = TEST %>% map('usg_dt')) %>% 
    select(usg_dt, customer, rt_sched_cd, model_id, model_id_sm, tou_cd, model_date, pred_date
           , temp_scenario, PRED_VAL) %>% 
    unnest() %>% 
    mutate(PRED_VAL = ifelse(PRED_VAL <0, 0, PRED_VAL)) %>% 
    data.table() %>% 
    data.table::dcast.data.table(formula = usg_dt + customer + rt_sched_cd  + tou_cd 
                                         + model_date + model_id + model_id_sm + pred_date ~ temp_scenario
                                 , value.var = 'PRED_VAL')
  
  # rename to match predictions table
  data.table::setnames(TEST, old = c('temp_normal', 'temp_high', 'temp_low', 'temp_mid_high', 'temp_mid_low')
              , new = paste0('PRED_VAL_',c('temp_normal', 'temp_high', 'temp_low', 'temp_mid_high', 'temp_mid_low')))
  
  # insert predictions into predictiona table
  dbWriteTable(ch, "OUT_SAMPLE_PRED",TEST[,.(usg_dt
                                             , customer
                                             , rt_sched_cd
                                             , tou_cd
                                             , model_date
                                             , pred_date
                                             , PRED_VAL_temp_normal
                                             , PRED_VAL_temp_high
                                             , PRED_VAL_temp_low
                                             , PRED_VAL_temp_mid_high
                                             , PRED_VAL_temp_mid_low)]
               , append = TRUE, row.names = FALSE)
  
  # create cycle time of predictions
  cycle_time <- (proc.time() - start_model_time)
  cycle_time <- round(cycle_time[[3]]/60)
  
  # write out summary of fit step
  write.table(x = TEST %>%
                group_by(model_id, model_date, pred_date) %>%
                summarise(number_of_customers = n_distinct(customer)) %>%
                mutate(cycle_time = cycle_time), file = 'out_sample_time.csv',sep = ',', append = TRUE, row.names = FALSE, col.names = FALSE)
  
  print(paste('Out of Sample Usage Predictions for model group', TEST[,unique(model_id)], 'completed in', cycle_time,'minutes;'
        ,'check OUT_SAMPLE_PRED for results'))
  
  process_statement <- paste('Out of Sample Usage Predictions for model group', TEST[,unique(model_id)], 'completed in', cycle_time,'minutes;'
                     ,'check OUT_SAMPLE_PRED for results')
  
  return(process_statement)
  
  }


# run out-of-sample predictions -------------------------------------------

predict_time <- map(.x = predictQuery_list, .f = predictModel_memory) %>% data.table::rbindlist()

# check output file -------------------------------------------------------

predict_output_summary <- fread(paste0(DATA_OUT_LOC,'/','out_sample_time.csv'), header = FALSE,
                              col.names = c('model_group','model_date','predict_date','model_group_size','group_cycle_time_minutes'))

# Clean up model environment ----------------------------------------------

# disconnect from ODBC 


# diconnect from MySQL
dbDisconnect(ch)

# remove unused objects from environment
rm(fitQuery_list)

