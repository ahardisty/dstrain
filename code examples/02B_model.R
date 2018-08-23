## SCRIPT TO BE USED ON FOR IN-MEMORY PROCESSING, REQUIRES
# 64 BIT ODBC DRIVER
# saves model objects for looping in next step
##
# B IS FOR DB IO
rm(list = ls()) # clear environment 
gc()
source('./Phase_3-3/01_functions.R') # load functions (verify path)

DATA_OUT_LOC <- getwd() # default to working directory; change as appropriate
rt_sched_cd_model <- 'HETOUA' # rt_sched_cd for modeling
LOWER_MODEL_ID <- 1 #first model group
UPPER_MODEL_ID <- 2 #last model group

# # create cross-ref table for model placeholder
# temp_scenario <- factor(c('temp_high','temp_mid_high','temp_normal','temp_mid_low','temp_low'))
# SCENARIO_XREF <- data.frame(rt_sched_cd = factor(rt_sched_cd_model), temp_scenario)

# no user intervention beyond defining variables above
model_seq <- (LOWER_MODEL_ID:UPPER_MODEL_ID) # vector of model groups for sequential modeling
rt_sched_cd <- paste0("'",rt_sched_cd_model,"'") # rt_sched_cd for SQL query

# Create connection string ----------------------------

# ODBC
# DSN <- 'TDDSNP'
# UID <- USER
# PW <- PWD
# ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

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

fitQuery <- stringr::str_replace_all(paste("SELECT 
                                           a.customer
                                           , a.tou_cd
                                           , a.temp_scenario
                                           , a.X
                                           , a.Y
                                           , b.model_id 
                                           , b.model_id_sm ,", rt_sched_cd ,"AS rt_sched_cd FROM RDA_P_REVR.EUA_TRAIN AS a JOIN 
                                           ( SELECT 
                                           customer AS custID
                                           , model_id 
                                           , model_id_sm 
                                           FROM RDA_P_REVR.EUA_COMPLETE )AS b 
                                           ON a.customer = b.custID 
                                           WHERE a.Y >0 AND b.model_id =")
                                     , "[\r\n]" , "")


# create list for looping function (applying function over a list)
fitQuery_list <- map2(.x = fitQuery, .y = model_seq, .f = paste)

# Train model on actual temperature and actual usage ----------------------

estimateModel_memory <- function(query = fitQuery, channel = ch, rate_code = rt_sched_cd_model)
{
  # start timing
  print("model fit initiated")
  start_model_time <- proc.time() 
  
  # RODBC sql query to extract train data
  # TRAIN <- dbGetQuery(channel, query)
  # TRAIN <- dbGetQuery(ch, fitQuery_list[[1]])
  
  print("reading train data from sql query")
  start_read_time <- proc.time()
  # MySQL sql query to extract train data
  TRAIN <- dbGetQuery(channel, query)
  # TRAIN <- dbGetQuery(ch, fitQuery_list[[1]])
  read_time <- (proc.time() - start_read_time)[[3]]/60
  
  # nest and fit on train data
  print("nesting data and fitting earth model")
  start_scenario_time <- proc.time()
  TRAIN <- TRAIN %>%
    group_by(temp_scenario, customer, tou_cd, model_id ,model_id_sm) %>% 
    nest(.key = TRAIN) %>% 
    mutate(MODEL = map(TRAIN, ~ earth(Y ~ X, data = ., nprune = 3)),
           rt_sched_cd = rate_code,
           model_date = format(Sys.Date(), '%Y-%m-%d'),
           model_time = Sys.time()) %>% 
    select(customer, tou_cd, model_id, model_id_sm, rt_sched_cd, model_date, model_time, MODEL)
  
  # assign location of saved model elements
  OUT_NAME <- paste('model_id',unique(TRAIN$model_id), sep = '_')
  OUT_FILE <- paste(OUT_NAME,unique(TRAIN$rt_sched_cd),'model.rds',sep='_')
  OUT_PATH <- paste0(DATA_OUT_LOC, '/', OUT_FILE)
  
  # Convert the model to a character string
  print("converting model elements to be inserted into database")
  TRAIN <- TRAIN %>% 
    mutate(MODEL_TRIM = MODEL %>% map(trim_model),
           MODEL_TRIM = MODEL_TRIM %>% map(serialize, connect = NULL, ascii = TRUE),
           MODEL_TRIM = MODEL_TRIM %>% map(rawToChar),
           CONS = 5000,
           MODEL_LEN = MODEL_TRIM %>% map_dbl(nchar)) %>% 
    mutate(diff = CONS - MODEL_LEN) %>%
    mutate(xout = pmap(.l = list(x = 'X',
                                 times = diff),.f = rep)) %>% 
    mutate(MODEL = map2(MODEL_TRIM, xout, paste, collapse = ""))
  scenario_time <- (proc.time() - start_scenario_time)[[3]]/60
  
  # modelString <- paste( modelString,
  #                       paste(rep("X", 5000 - nchar(modelString)), collapse = ""),
  #                       sep="")
  # 
  print("writing model to MODELS table")
  start_write_time <- proc.time()
  dbWriteTable(ch, "MODELS", TRAIN %>% select(customer, tou_cd, model_id, model_id_sm, rt_sched_cd
                                              , model_date, MODEL) %>% 
                 unnest(), append = TRUE, row.names = FALSE)
  write_time <- (proc.time() - start_write_time)[[3]]/60
  
  # create cycle time of model fit
  cycle_time <- (proc.time() - start_model_time)[[3]]/60
  
  # write results of model fit step
  print("writing model time summary")
  
  write.table(cbind(TRAIN %>% group_by(model_id, model_date, model_time, rt_sched_cd) %>% 
    summarise(number_of_customers = n_distinct(customer)) %>% 
    ungroup(),tibble(model_io_type = 'sql',read_time, scenario_time, write_time, cycle_time))
    , file = 'model_time.csv',sep = ',', append = TRUE, row.names = FALSE, col.names = FALSE)
  
  
  print(paste('Model Fit for model_id', unique(TRAIN$model_id),'complete in', round(cycle_time),'minutes; in-sample and out-of-sample predictions can now be made'))
  
  return(cbind(TRAIN %>% group_by(model_id, model_date, model_time, rt_sched_cd) %>% 
                 summarise(number_of_customers = n_distinct(customer)) %>% 
                 ungroup(),tibble(model_io_type = 'sql',read_time, scenario_time, write_time, cycle_time)))
} 

# Command to run estimateModel_memory with summary table ----------------

# fit_time <- map(.x = fitQuery_list, .f = estimateModel_memory) %>% data.table::rbindlist()

# Command to run estimateModel_memory without summary table ----------------

walk(.x = fitQuery_list, .f = estimateModel_memory)

# Visualize model summary file -------------------------------------------------------

model_output_summary <- fread(paste0(DATA_OUT_LOC,'/','model_time.csv'), header = FALSE,
                              col.names = c('model_group','model_date','model_time'
                                            ,'rt_sched_cd','model_group_size','model_io_type'
                                            , 'read_time', 'scenario_time', 'write_time', 'cycle_time')) %>% 
  data.table::melt.data.table(id.vars = c('rt_sched_cd','model_io_type','model_date','model_group_size'
                                          ,'model_time','model_group'))

model_output_plot <- ggplot(model_output_summary,aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = model_io_type)) +
  facet_wrap(~model_group_size) +
  labs(x = 'Step Name', y = "Step Time (Minutes)") +
  ggtitle(paste("Cycle Time Comparisons for", n_distinct(model_output_summary$model_group)
                ,"group(s)"))

  # # plot model output summary
  # ggplot(aes(x = model_time, elapsed_time_minutes)) +
  # geom_point(aes(size = num_of_customers, color = elapsed_time_minutes )) +
  # theme(axis.text.x=element_blank())+
  # scale_color_gradient(high = "#e34a33", low = "#2c7fb8", name = "Model Fit Time (minutes)") +
  # scale_size_continuous(name = 'Number of Customers') +
  # labs(x = NULL, y = NULL) 
# save model output summary
ggsave('model_output_summary.png', model_output_plot, width = 11.5, height = 8)


# http://stackoverflow.com/questions/5118074/reusing-a-model-built-in-r


# Clean up model environment ----------------------------------------------

# disconnect from ODBC 


# diconnect from MySQL
dbDisconnect(ch)

# remove unused objects from environment
rm(fitQuery_list)
rm(fitQuery)

