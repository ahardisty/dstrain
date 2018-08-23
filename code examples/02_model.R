## SCRIPT TO BE USED ON FOR IN-MEMORY PROCESSING, REQUIRES
# 64 BIT ODBC DRIVER
# 
##

rm(list = ls()) # clear environment 
source('./Phase_3-3/01_functions.R') # load functions (verify path)

rt_sched_cd_model <- 'HETOUA' # rt_sched_cd for modeling
rt_sched_cd <- paste0("'",rt_sched_cd_model,"'") # rt_sched_cd for SQL query
LOWER_MODEL_ID <- 1 #first model group
UPPER_MODEL_ID <- 5 #last model group
model_seq <- (LOWER_MODEL_ID:UPPER_MODEL_ID) # vector of model groups for sequential modeling
DATA_OUT_LOC <- getwd() # default to working directory; change as appropriate

# create cross-ref table for model placeholder
temp_scenario <- c('temp_high','temp_mid_high','temp_normal','temp_mid_low','temp_low')
SCENARIO_XREF <- data.frame(rt_sched_cd = rt_sched_cd_model, temp_scenario)


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

# Create SQL query to bring Train data into memory ----------------------------------------------------

fitQuery <- str_replace_all(paste("SELECT 
a.customer
, a.tou_cd
, a.temp_scenario
, a.X
, a.Y
, b.model_id 
, b.model_id_sm ,", rt_sched_cd ,"AS rt_sched_cd FROM RDA_P_REVR.EUA_TRAIN AS a JOIN 
(SELECT 
customer AS custID
, model_id 
, model_id_sm 
FROM RDA_P_REVR.EUA_COMPLETE)AS b 
ON a.customer = b.custID 
WHERE a.Y >0 AND b.model_id =")
, "[\r\n]" , "")


# create list for looping function (applying function over a list)
fitQuery_list <- map2(.x = fitQuery, .y = model_seq, .f = paste)
fitQuery <- fitQuery_list[[1]]
# Train model on actual temperature and actual usage ----------------------

estimateModel_memory <- function(query = fitQuery, channel = ch, rate_code = rt_sched_cd_model
                                 , scenarios = SCENARIO_XREF)
{
  # start timing
  start_model_time <- proc.time() 
  
  # sql query to extract train data
  TRAIN <- dbGetQuery(channel, query)
  
  # nest and fit on train data
  TRAIN_MODEL <- TRAIN %>%
    group_by(temp_scenario, customer, tou_cd, model_id ,model_id_sm) %>% 
    nest(.key = TRAIN) %>% 
    mutate(MODEL = map(TRAIN, ~ earth(Y ~ X, data = ., nprune = 3)),
           rt_sched_cd = rate_code,
           model_date = format(Sys.Date(), '%Y-%m-%d')) %>% 
    select(customer, tou_cd, model_id, model_id_sm, rt_sched_cd, model_date, MODEL)

  # assign location of saved model elements
  OUT_NAME <- paste('model_id',unique(TRAIN_MODEL[[3]]), sep = '_')
  OUT_FILE <- paste(OUT_NAME,'model.rda',sep='_')
  OUT_PATH <- paste0(DATA_OUT_LOC, '/', OUT_FILE)
  
  # save model elements for later use
  save(TRAIN_MODEL, file = OUT_PATH)
  
  # create cycle time of model fit
  cycle_time <- (proc.time() - start_model_time)
  cycle_time <- round(cycle_time[[3]]/60)

  # write results of model fit step
  write.table(x = TRAIN_MODEL %>% 
    group_by(model_id, model_date) %>% 
    summarise(number_of_customers = n()) %>% 
    mutate(cycle_time = cycle_time), file = 'model_time.csv',sep = ',', append = TRUE, row.names = FALSE, col.names = FALSE)

  print(paste('Model Fit for model_id', unique(TRAIN_MODEL[[3]]),'Complete in', cycle_time,'minutes'))

  # merge with XREF to get full model values and all temp scenarios
  TRAIN_MODEL <- left_join(TRAIN_MODEL, scenarios, by = c('rt_sched_cd'))
  

  return(TRAIN_MODEL)
} 

# retain model objects for next step
MODELS <- map(.x = fitQuery_list, .f = estimateModel_memory) %>% data.table::rbindlist()
MODELS <- TRAIN_MODEL
# check output file -------------------------------------------------------

model_output_summary <- fread(paste0(DATA_OUT_LOC,'/','model_time.csv'), header = FALSE,
      col.names = c('model_group','model_date','model_group_size','group_cycle_time_minutes'))

# http://stackoverflow.com/questions/5118074/reusing-a-model-built-in-r


# Clean up model environment ----------------------------------------------

# disconnect from ODBC 


# diconnect from MySQL
dbDisconnect(ch)

# remove unused objects from environment
rm(fitQuery_list)
