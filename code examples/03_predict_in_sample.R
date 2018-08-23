# IN SAMPLE PREDICTIONS ---------------------------------------------------

# Create connection string and compute context ----------------------------

# tdConnString <- "DSN=TDDSNP;pwd=Eat3onions!;" # TDDBPRDR PROD CONNECTION STRING
# tdConnString <- "DSN=TDDSNP; DATABASE = RDA_P_REVR; pwd=Eat3onions!;" # TDDBPRDR PROD CONNECTION STRING
# 
# tdCC <- RxInTeradata(
#   connectionString = tdConnString,
#   # shareDir = getwd(),
#   # shareDir = 'C:/Users/A3HE/Documents/eua/model_temp',
#   shareDir = 'C:temp',
#   remoteShareDir = "/tmp/revoJobsNoCleanup",
#   # remoteShareDir = "/tmp",
#   revoPath = "/usr/lib64/microsoft-r/8.0/lib64/R",
#   consoleOutput = TRUE,
#   wait=TRUE,
#   autoCleanup=FALSE,
#   # autoCleanup=TRUE,
#   traceEnabled=TRUE
#   ,traceLevel=7
# )


# Verify TD Data Connection --------------------------------------------------                              
# verify data structure EUA_TEST_IN_SAMPLE data

# tdQuery <- "SELECT * FROM RDA_P_REVR.EUA_TEST_IN_SAMPLE"
# 
# # create an RxTeradata data source object:
# teradataDS <- RxTeradata(connectionString = tdConnString, sqlQuery = tdQuery, rowsPerRead = 50000)
# 
# # Basic Information about Data
# rxGetVarInfo(data = teradataDS)
#                                
# 
# # Make In-sample (TRAIN) Predictions -------------------------------------------

baseTbl <- "EUA_TEST_IN_SAMPLE"
modelTbl <- "MODELS"

predictQuery <- str_replace_all(paste("SELECT ", 
                      baseTbl,".usg_dt,", 
                      baseTbl,".day_of_year_shift,",
                      baseTbl,".customer,", 
                      # baseTbl,".model_id,", 
                      # baseTbl,".model_id_sm,", 
                      baseTbl,".tou_cd,",
                      baseTbl,".X,",
                      baseTbl,".X_sd,",
                      modelTbl, ".rt_sched_cd, ",
                      modelTbl, ".model_date, ",
                      modelTbl, ".MODEL ",
                      "FROM ", baseTbl, " ",
                      "JOIN ", modelTbl," ON (",
                      baseTbl,".customer = ", modelTbl,".customer AND ", 
                      baseTbl,".tou_cd = ", modelTbl,".tou_cd", 
                      ") WHERE ",modelTbl,".rt_sched_cd = ",rt_sched_cd
                      ," AND ", baseTbl,".model_id_sm IN (", model_id,")",
                      " AND ", baseTbl,".temp_scenario <> 'CONS_USG')",sep="")
                      , "[\r\n]" , "")

# create partition clause
partitionKeyword <- "PARTITION BY"
partitionCols <- "customer, tou_cd"
predPartitionClause <- paste(partitionKeyword,
                             paste(paste(baseTbl,".",sep="")
                                   , strsplit(x = partitionCols, split=" ")[[1]]
                                   , sep="", collapse=" "))

# create in sample prediction data source 
teradataDS <- RxTeradata(connectionString = tdConnString, sqlQuery = predictQuery, rowsPerRead = 50000)

# verify in sample prediction data source
rxGetVarInfo(data = teradataDS)

# complete IN-SAMPLE prediction data source
inDataSource <- RxTeradata(sqlQuery = predictQuery,
                           connectionString = tdConnString,
                           tableOpClause = predPartitionClause, rowsPerRead = 10000)

rxGetVarInfo(data = inDataSource)

# Create in-sample predictions location 
scoresDataSource <-  RxTeradata(table = "IN_SAMPLE_PRED_TMP",
                                connectionString = tdConnString)

# set compute context
rxSetComputeContext(tdCC, wait=FALSE) # set compute environment

# in sample predictions with temperature scenarios
pred_time <- system.time(rxDataStep(inData = inDataSource, 
                                    outFile = scoresDataSource, 
                                    transformFunc = ScoreChunkTEST,
                                    transformPackages = c("earth"), # call earth package
                                    reportProgress = 0, overwrite = TRUE ))

# SAVE IN SAMPLE PREDICTIONS TO PERMANENT TABLE -----------------------------------
DSN <- 'TDDSNP'
UID <- USER
PW <- PWD
ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

# query to transfer model elements from temporary table to permanent table
insertQuery <- str_replace_all("INSERT INTO RDA_P_REVR.IN_SAMPLE_PRED SELECT
                               * FROM RDA_P_REVR.IN_SAMPLE_PRED_TMP", "[\r\n]",'')


# transfer model elements from temporary table to permanent table
odbcQuery(ch, insertQuery)


# query to clear model elements from temporary table
deletetQuery <- str_replace_all("DELETE RDA_P_REVR.IN_SAMPLE_PRED_TMP", "[\r\n]",'')

odbcQuery(ch, deletetQuery)

close(ch)

