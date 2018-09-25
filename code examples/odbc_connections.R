
# rm(list = ls())
# CONNECTION VARIABLES ----------------------------------------------------

DSN <- 'TDDSND'
UID <- 'a3he'
PW <- 'Eat3onions!'
ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

DSN <- 'TDDSNP'
UID <- 'a3he'
PW <- 'Eat3onions!'
ch <- odbcConnect(dsn = DSN, uid = UID, pwd = PW)

# VIEW DATA ODBC--------------------------------------------------------------

ACCT <- sqlTables(ch, tableName = "TRAIN", tableType = 'Table')
SCHEMA <- sqlTables(ch, tableType = 'Table')
EUA <- sqlTables(ch, schema = 'RDA_P_REVR')
summary(as.factor(EUA$TABLE_SCHEM))


# VIEW DATA MYSQL ---------------------------------------------------------

dbListTables(ch)
dbListFields(ch, 'CUST_REVR')

# ACCESS DATA ODBC-------------------------------------------------------------

# TOP_WEA <- sqlQuery(ch, paste("select TOP 10 * FROM RDA_P_REVR.TEMP_TOU_TRAIN"))

TOP_10000_TRAIN <- sqlQuery(ch, paste("select TOP 10000 * FROM RDA_P_REVR.EUA_TRAIN"))
TOP_10000_TEST <- sqlQuery(ch, paste("select TOP 10000 * FROM RDA_P_REVR.EUA_TEST"))

# TOP_10000_wea <- sqlQuery(ch, paste("select TOP 10000 * from RDA_P_REVR.TEMP_TOU_TRAIN"))

summary(TOP_10000_TRAIN)
summary(TOP_10000_TEST)



# CREATE ONE VOLATILE TABLE ---------------------------------------------------
# one volatile table with direct sql command
# sqlQuery(ch, paste("CREATE VOLATILE TABLE wea_stn_cd 
#                    AS (SELECT wea_stn_cd FROM cust_d_wea_v.WEATHER_OP_AREA_XREF)
#                    WITH DATA NO PRIMARY INDEX ON COMMIT PRESERVE ROWS;")
# )
# # dataframe
# wea_stn_cd <- sqlQuery(ch, paste("SELECT TOP 10 * FROM wea_stn_cd")) # SELECT FROM A VOLATILE TABLE
# str(wea_stn_cd)

# LOOP TO CREATE TABLES ODBC---------------------------------------------------

model_files <- dir('sql/data_prep/PROD/', pattern = 'VOL|STATS', full.names = TRUE)
model_lines <- map(model_files[1], readLines)
model_lines <- map(model_lines, cleanLines)  
model_lines <- map(model_lines, paste, collapse = " ")  

# sqlQuery(ch, model_lines[1]) # CREATE ONE VOLATILE TABLE
# sqlQuery(ch, model_lines[2]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[1]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[4]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[5]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[6]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[7]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[8]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[9]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[10]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[11]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[12]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[13]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[14]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[15]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[16]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[17]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[18]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[19]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[20]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[21]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[22]) # CREATE ONE VOLATILE TABLE
sqlQuery(ch, model_lines[23]) # INSERT TRAIN FROM VOL
# sqlQuery(ch, model_lines[24]) # STATS TRAIN
sqlQuery(ch, model_lines[25]) # INSERT TEST FROM VOL
# sqlQuery(ch, model_lines[26]) # STATS TEST

# CREATE MULTIPLE VOLATILE TABLES -----------------------------------------

# find all .sql files that create volatile tables and populate persistent tables
model_files <- dir('sql/data_prep/', pattern = '.sql', full.names = TRUE)
model_lines <- map(model_files, readLines)
model_lines <- map(model_lines, cleanLines)  
model_lines <- map(model_lines, paste, collapse = " ")  

# find all .sql files that perform summary functions
summary_files <- dir('sql/XX/', pattern = '.sql', full.names = TRUE)
summary_lines <- map(summary_files, readLines)
summary_lines <- map(summary_lines, lineclean)  
summary_lines <- map(summary_lines, paste, collapse = " ")  
summary_lines[6]
summary_lines[10]

# summary population size
CUST_REVR_SUMMARY <- sqlQuery(ch, summary_lines[grep(pattern = 'SUMMARY', x = summary_files)]) %>% 
  arrange(climate_zone_cd, desc(population))

CUST_REVR_SUMMARY %>% filter(opr_area_cd == 'TC')

# CREATE IN A LOOP
CREATE <- grep(pattern = '00', model_files)
model_lines[-CREATE]
walk(.x = model_lines[-c(CREATE,24,26)], .f = sqlQuery, channel = ch) # Create in a loop

INSERT_TEST <- grep(pattern = 'INSERT_TEST', model_files)
walk(.x = model_lines[INSERT_TEST], .f = sqlQuery, channel = ch) # Create in a loop

CUST_REVR <- sqlQuery(ch, paste("SELECT TOP 10 * FROM CUST_REVR ")) # SELECT FROM A VOLATILE TABLE

TOU_TRAIN <- sqlQuery(ch, paste("SELECT TOP 10 * FROM ELECTRIC_INTERVAL_TOU_TRAIN")) # SELECT FROM A VOLATILE TABLE
TOU_TEST <- sqlQuery(ch, paste("SELECT TOP 10 * FROM ELECTRIC_INTERVAL_TOU_TEST")) # SELECT FROM A VOLATILE TABLE
HOURLY_TEMP <- sqlQuery(ch, paste("SELECT TOP 10 * FROM HOURLY_TEMP_TRAIN")) # SELECT FROM A VOLATILE TABLE
HOURLY_TEMP <- sqlQuery(ch, paste("SELECT COUNT (*) FROM HOURLY_TEMP_TEST")) # SELECT FROM A VOLATILE TABLE
TEMP_TOU_TRAIN <- sqlQuery(ch, paste("SELECT TOP 10 * FROM TEMP_TOU_TRAIN")) # SELECT FROM A VOLATILE TABLE
TEMP_TOU_TEST <- sqlQuery(ch, paste("SELECT TOP 10 * FROM TEMP_TOU_TEST")) # SELECT FROM A VOLATILE TABLE
TEMP_USG_TOU_TRAIN_VOL <- sqlQuery(ch, paste("SELECT TOP 10 * FROM TEMP_USG_TOU_TRAIN")) # SELECT FROM A VOLATILE TABLE
TEMP_USG_TOU_TEST_VOL <- sqlQuery(ch, paste("SELECT TOP 10 * FROM TEMP_USG_TOU_TEST")) # SELECT FROM A VOLATILE TABLE

TEMP_USG_TOU_TRAIN_COUNT <- sqlQuery(ch, paste("SELECT COUNT (DISTINCT customer) FROM TEMP_USG_TOU_TRAIN")) # SELECT FROM A VOLATILE TABLE
TEMP_USG_TOU_TEST_COUNT <- sqlQuery(ch, paste("SELECT COUNT (DISTINCT customer) FROM TEMP_USG_TOU_TEST")) # SELECT FROM A VOLATILE TABLE

TRAIN_COUNT <- sqlQuery(ch, paste("SELECT COUNT (DISTINCT customer) FROM EUA_TRAIN")) # SELECT FROM A VOLATILE TABLE
TEST_DATA_COUNT <- sqlQuery(ch, paste("SELECT COUNT (DISTINCT customer) FROM EUA_TEST")) # SELECT FROM A VOLATILE TABLE

TEMP_USG_TOU_TRAIN <- sqlQuery(ch, paste("SELECT * FROM EUA_TRAIN")) %>% tibble::as_tibble()
TEMP_USG_TOU_TEST <- sqlQuery(ch, paste("SELECT * FROM EUA_TEST")) %>% tibble::as_tibble()

head(TEMP_USG_TOU_TRAIN)
head(TEMP_USG_TOU_TEST)

# model <- lm(Y ~ X, data = TEMP_USG_TOU_TEST)


# READ OBJECTS INTO MEMORY ODBC ------------------------------------------------

PAT <- '(?<=VOL_).\\w+(?=.sql)' # Pattern matching for naming
NUM <- 10 # sample size
# NUM <- '*' # sample size
DF_NAMES <- stringr::str_extract(model_files, pattern = PAT) # create new names from files
# CALL_LIST <- paste('SELECT TOP', NUM ,'* FROM', DF_NAMES) # loop of sql VOL table commands

CALL_LIST <- paste('SELECT count(*) FROM', DF_NAMES) # loop only on train | test
CALL_LIST <- paste('SELECT TOP', NUM ,'* FROM'
                   , DF_NAMES[grep(pattern = '(TEST|TRAIN)', x = DATA_LIST)]) # loop only on train | test

DATA_LIST <- map(.x = CALL_LIST, .f = sqlQuery, channel = ch) # list of DF in memory from sql VOL table commands

names(DF_LIST) <- DF_NAMES[grep(pattern = '(TEST|TRAIN)', x = DF_NAMES)] # loop only on train | test

sizes <- rbindlist(DIM_LIST)[,table_name:= DF_NAMES]

DF_LIST$TEMP_TOU_TRAIN
DF_LIST$ELECTRIC_INTERVAL_TOU_TRAIN





# INSERTING DATA TO TERADATA -------------------------------------------------------------

## Not run: 
channel <- odbcConnect("test")
sqlSave(channel, USArrests, rownames = "state", addPK=TRUE)
sqlFetch(channel, "USArrests", rownames = "state") # get the lot
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 222
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlDrop(channel, "USArrests") 
close(channel)

columnTypes <- list(customer="varchar(12)", acct_yrs="float", sa_start="date")
top_df <- ch %>% sqlQuery(paste("select TOP 1 * from REVR_D.SNAPSHOT_CUST_REVR"))

as.character(gsub(pattern = ' ', replacement = '0', sprintf('%10s', 111111111)))
as.character(gsub(pattern = ' ', replacement = '0', sprintf('%10s', 111111111)))

res_small <- DF_LIST$TEMP_TOU_TRAIN %>% filter(climate_zone_cd == 'S') %>% 
  # mutate_at(vars(matches('_id')), funs(nchar))
  mutate_at(vars(matches('_id')), funs(sprintf('%10s',.))) %>% 
  mutate_at(vars(matches('_id')), funs(gsub(pattern = ' ', replacement = '0',.))) %>% 
  mutate(customer = paste0(prem_id,'_',acct_id)) %>% 
  select(customer, acct_yrs, sa_start)

res_small <- DF_LIST$ELECTRIC_INTERVAL_TOU_TRAIN %>% 
  # mutate_at(vars(matches('_id')), funs(nchar))
  mutate_at(vars(matches('_id')), funs(sprintf('%10s',.))) %>% 
  mutate_at(vars(matches('_id')), funs(gsub(pattern = ' ', replacement = '0',.))) %>% 
  mutate(customer = paste0(prem_id,'_',acct_id)) %>% 
  select(customer, acct_yrs, sa_start)

DF_LIST[grep(pattern = '(INTERVAL)', x = names(DF_LIST))] <- DF_LIST[grep(pattern = '(INTERVAL)', x = names(DF_LIST))] %>% 
  # map(. %>% filter((shift_year_day != 229))) %>%
  map(. %>% mutate_at(vars(matches('_id')), funs(sprintf('%10s',.))) %>%
        mutate_at(vars(matches('_id')), funs(gsub(pattern = ' ', replacement = '0',.))) %>% 
        mutate(customer = paste0(prem_id,'_',acct_id)))

sqlSave(ch, dat = res_small, tablename = 'REVR_D.T2', rownames = FALSE, varTypes = columnTypes)
?sqlSave()

# DROPPING TABLES ---------------------------------------------------------

sqlDrop(ch, "HOURLY_TEMP", errors = FALSE)
sqlDrop(ch, "wea_stn_cd", errors = FALSE)


# PLOT CUSTOMER POPULATION ------------------------------------------------

# extract customer details (tenure and sa changes) from subset population
CUST_REVR_DETAILS <- sqlQuery(ch, paste("SELECT * FROM REVR_D.SNAPSHOT_CUST_REVR"))
summary(as.factor(CUST_REVR_DETAILS$prem_lat))

CUST_REVR_DETAILS <- CUST_REVR_DETAILS %>% mutate_at(vars(matches('_id')), funs(sprintf('%10s',.))) %>% 
  mutate_at(vars(matches('_id')), funs(gsub(pattern = ' ', replacement = '0',.))) %>% 
  mutate(customer = paste0(prem_id,'_',acct_id))
CUST_REVR_DETAILS <- tibble::as_tibble(CUST_REVR_DETAILS)

CUST_REVR_DETAILS <- CUST_REVR_DETAILS %>% mutate_at(vars(matches('_id')), funs(sprintf('%10s',.))) %>% 
  mutate_at(vars(matches('prem')), funs(as.factor(.))) %>% 
  mutate_at(vars(matches('_id')), funs(as.factor(.))) %>% 
  mutate(customer = as.factor(customer))

str(CUST_REVR_DETAILS)

# CLOSE CONNECTION --------------------------------------------------------

close(ch)

