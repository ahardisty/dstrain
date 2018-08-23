# OUT OF SAMPLE PREDICTIONS ---------------------------------------------------
# A IS FOR FLAT FILES

setwd('~')
rm(list = ls())
setwd('energy')
source('Phase_3-3/01_functions.R') # load functions if needed (verify path)
rt_sched_cd_model <- 'HETOUA' # rt_sched_cd for modeling
LOWER_MODEL_ID <- 1 #first model group
UPPER_MODEL_ID <- 1 #last model group
fileType = ".csv"

DATA_IN_LOC <- getwd() # default to working directory; change as appropriate
DATA_OUT_LOC <- getwd() # default to working directory; change as appropriate

# Set standard deviation levels for temperature scenarios
temp_high <- .85
temp_mid_high <- .5 
# temp_normal: placeholder, no change
temp_mid_low <- .5 
temp_low <- .85

# no user intervention beyond defining variables above
rt_sched_cd <- paste0("'",rt_sched_cd_model,"'") # rt_sched_cd for SQL query
model_seq <- (LOWER_MODEL_ID:UPPER_MODEL_ID) # vector of model groups for sequential modeling

# Create connection string ----------------------------

# ODBC
# DSN <- 'TDDSNP'
# UID <- USER
# PW <- PWD
# ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

# MySQL
# library(RMySQL)
ch <- dbConnect(MySQL(),
                user = 'root',
                host = 'localhost',
                dbname = 'RDA_P_REVR')

# Verify Data Connection --------------------------------------------------
# ODBC


# MySQL
dbGetInfo(ch)

# Create list of SQL queries to bring Test data into memory ----------------------------------------------------

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

# Create list of model data from local computer ---------------------------

modelObject_list <- pmap(.l = list(
  location = DATA_IN_LOC
  , sep1 = '/'
  , model = 'model_id_'
  , model_seq = model_seq
  , sep2 = '_'
  , rt_sched = rt_sched_cd_model
  , file_type = '_model.rds'), .f = paste0)

# out of sample predictions with temperature scenarios

predictQuery <- predictQuery_list[[1]]
modelObject <- modelObject_list[[1]]

predictModel_memory <- function(pred = predictQuery, model = modelObject,
                                rate_code = rt_sched_cd_model, channel = ch, outType = fileType)
{
  start_model_time <- proc.time() 
  print("predictions initiated")
  
  # sql query for Test data
  start_read_time <- proc.time()
  print("reading test data from database")
  TEST <- dbGetQuery(channel, pred)
  read_time <- (proc.time() - start_read_time)[[3]]/60
  
  # create temp scenario values and gather into long format
  print("creating temperature scenarios and nesting test data")
  start_scenario_time <- proc.time()
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
    tidyr::gather(key = temp_scenario,value = X
                  , c(temp_high, temp_mid_high, temp_normal, temp_mid_low, temp_low))
  
  # nest for joining models
  TEST_MODEL <- TEST %>%
    group_by(model_type, customer, tou_cd, model_id, model_id_sm, temp_scenario, rt_sched_cd) %>% 
    nest(.key = TEST)
  
  rm(TEST)
  scenario_time <- (proc.time() - start_scenario_time)[[3]]/60
  
  # load saved model object information
  # TRAIN_MODEL <- map(models, readRDS)
  # TRAIN_MODEL <-models
  print("reading model data from flat .RDS files")
  start_read_time <- proc.time()
  TRAIN_MODEL <- readRDS(model)
  read_model_time <- (proc.time() - start_read_time)[[3]]/60
  
  
  # join test data and model data
  # TEST <- dplyr::left_join(TEST_MODEL, TRAIN_MODEL, by = c('customer','tou_cd','rt_sched_cd','model_id','model_id_sm','temp_scenario'))
  print('performing out of sample predictions')
  start_pred_time <- proc.time()
  TEST <- dplyr::left_join(TEST_MODEL, TRAIN_MODEL, by = c('customer','tou_cd','rt_sched_cd'))
  
  # make predictions across multiple temperature scenarios
  TEST <- TEST %>%
    mutate(PRED_VAL = map2(MODEL, TEST %>% map('X'), predict)) %>% 
    mutate(pred_date = format(Sys.Date(), '%Y-%m-%d'),
           pred_time = Sys.time(),
           usg_dt = TEST %>% map('usg_dt')) %>% 
    select(usg_dt, customer, rt_sched_cd, model_id, model_id_sm, tou_cd, model_date, pred_date
           , pred_time, temp_scenario, PRED_VAL) %>% 
    unnest() %>% 
    mutate(PRED_VAL = ifelse(PRED_VAL <0, 0, PRED_VAL)) %>% # get rid of negative predictions
    arrange(usg_dt) %>% 
    data.table() %>% 
    data.table::dcast.data.table(formula = usg_dt + customer + rt_sched_cd  + tou_cd 
                                 + model_date + model_id + model_id_sm + pred_date + pred_time ~ temp_scenario
                                 , value.var = 'PRED_VAL')
  
  # rename to match predictions table
  data.table::setnames(TEST, old = c('temp_normal', 'temp_high', 'temp_low', 'temp_mid_high', 'temp_mid_low')
                       , new = paste0('PRED_VAL_',c('temp_normal', 'temp_high', 'temp_low', 'temp_mid_high', 'temp_mid_low')))
  
  pred_time <- (proc.time() - start_pred_time)[[3]]/60

  print("writing predictions to OUT_SAMPLE_PRED flat file")
  # write predictions to flat file  
  # assign location of saved model elements
  start_write_time <- proc.time()
  OUT_PATH <- pmap(.l = list(
    location = DATA_IN_LOC
    , sep1 = '/'
    , model_type = 'model_id_'
    , model_id = unique(TEST$model_id)
    , sep2 = '_'
    , rt_sched = rt_sched_cd_model
    , sep3 = '_'
    , function_type = 'prediction'
    , file_type = outType), .f = paste0) %>% unlist()

  write.csv(x = TEST[,.(usg_dt
                      , customer
                      , rt_sched_cd
                      , tou_cd
                      , model_date
                      , pred_date
                      , PRED_VAL_temp_normal
                      , PRED_VAL_temp_high
                      , PRED_VAL_temp_low
                      , PRED_VAL_temp_mid_high
                      , PRED_VAL_temp_mid_low)], file = OUT_PATH, row.names = FALSE)
  pwalk(.l = list(x = out_pred, 
                  file = output_list), .f = write.csv)

  
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
  
  write_time <- (proc.time() - start_write_time)[[3]]/60
  # create cycle time of predictions
  cycle_time <- (proc.time() - start_model_time)[[3]]/60
  
  # write out summary of fit step
  write.table(cbind(TEST %>% group_by(model_id, pred_date, pred_time, rt_sched_cd) %>% 
                      summarise(number_of_customers = n_distinct(customer)) %>% 
                      ungroup(),tibble(model_io_type = 'rds', read_time, scenario_time, read_model_time,
                                       pred_time, write_time, cycle_time))
              , file = 'out_sample_pred_time.csv',sep = ',', append = TRUE, row.names = FALSE, col.names = FALSE)
  
  print(paste('Out of Sample Usage Predictions for model group', TEST[,unique(model_id)], 'completed in'
              , round(cycle_time),'minutes;'
              ,'check RDA_P_REVR.OUT_SAMPLE_PRED for results'))
  
  return(cbind(TEST %>% group_by(model_id, pred_date, pred_time, rt_sched_cd) %>% 
                 summarise(number_of_customers = n_distinct(customer)) %>% 
                 ungroup(),tibble(model_io_type = 'rds', read_time, scenario_time, read_model_time,
                                  pred_time, write_time, cycle_time)))
}

# Command to run predictModel_memory with summary table ----------------

predict_time <- pmap(.l = list(query = predictQuery_list,
                               models = modelObject_list),
                     .f = predictModel_memory) %>% rbindlist()

# Command to run predictModel_memory without summary table ----------------
pwalk(.l = list(pred = predictQuery_list,
                model = modelObject_list),
      .f = predictModel_memory)

# Visualize model summary file -------------------------------------------------------

predict_output_summary <- fread(paste0(DATA_OUT_LOC,'/','out_sample_pred_time.csv'), header = FALSE,
                                col.names = c('model_group','pred_date','pred_time'
                                              ,'rt_sched_cd','model_group_size','model_io_type'
                                              , 'read_time', 'scenario_time', 'read_model_time'
                                              , 'pred_cycle_time', 'write_time', 'cycle_time')) %>% 
  data.table::melt.data.table(id.vars = c('rt_sched_cd','model_io_type','pred_date','model_group_size'
                                          ,'pred_time','model_group'))

predict_output_plot <- ggplot(predict_output_summary %>% filter(variable != 'cycle_time')
                              ,aes(x = factor(variable), y = value)) +
  geom_boxplot(aes(color = model_io_type)) +
  facet_wrap(~model_io_type, ncol = 1) +
  labs(x = 'Step Name', y = "Step Time (Minutes)") +
  ggtitle(paste("Out of Sample Prediction Cycle Time Comparisons for", n_distinct(predict_output_summary$model_group)
                ,"group(s)"))

# save predict output summary
ggsave('predict_output_summary.png',predict_output_plot, width = 11.5, height = 8)

# Clean up model environment ----------------------------------------------

# disconnect from ODBC 


# diconnect from MySQL
dbDisconnect(ch)

rm(list = ls())
