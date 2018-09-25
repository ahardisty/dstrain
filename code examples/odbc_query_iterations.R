# Setting up ODBC Drivers  ------------------------------------------------
# https://db.rstudio.com/best-practices/drivers/
odbc::odbcListDrivers()


# Connect to a Database ---------------------------------------------------

# https://db.rstudio.com/getting-started/connect-to-database/
install.packages("DBI")
install.packages("odbc")
install.packages("RSQLite")
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
# VERIFY ODBC DRIVERS
sort(unique(odbcListDrivers()[[1]]))

con <- odbc::dbConnect(MySQL(),
                       username = 'root',
                       host = 'localhost',
                       dbname = 'RDA_P_REVR')

install.packages("nycflights13")

dplyr::copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE
)

dbListTables(con)
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con) %>% tibble::as.tibble()
?Id
dbListObjects(con)
dbListObjects(con) %>% as_tibble()
dbq <- dbGetQuery(con,"select * from flights")


flights_db <- dplyr::tbl(con, "flights")
flights_db %>% select(year:day, dep_delay, arr_delay)

# RMYSQL

# http://www.slideshare.net/RsquaredIn/rmysql-tutorial-for-beginners

# MySQL -------------------------------------------------------------------
library(RMySQL)
library(data.table)
ch <- dbConnect(MySQL(),
                user = 'root',
                host = 'localhost',
                dbname = 'RDA_P_REVR')

library(devtools)
library(RPostgres)s
con <- dbConnect(Postgres())
devtools::install_github("rstats-db/DBI")
devtools::install_github("rstats-db/RMySQL")
nycflights13::airlines

(id <- Id(schema = "nycflights13", table = "flights"))
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
dbListTables(con)
library(DBI)

id <- Id(schema = "nycflights13")
dbListObjects(con, id) %>% as_tibble()

dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)
dbGetInfo(ch)

# ACCESS DATA MYSQL-------------------------------------------------------------

# TOP_WEA <- sqlQuery(ch, paste("select TOP 10 * FROM RDA_P_REVR.TEMP_TOU_TRAIN"))
TOP_TRAIN <- dbGetQuery(ch, paste("select * FROM RDA_P_REVR.EUA_TRAIN"))
TOP_TRAIN <- dbGetQuery(ch, paste("select * FROM RDA_P_REVR.EUA_TRAIN where model_id IN (2)"))
TOP_TEST <- dbGetQuery(ch, paste("select  * FROM RDA_P_REVR.EUA_TEST"))
TOP_TEST <- dbGetQuery(ch, paste("select  * FROM RDA_P_REVR.EUA_TEST where model_id IN (2)"))
TOP_TRAIN <- dbReadTable(ch, 'EUA_TRAIN')
dbReadTable(ch, 'EUA_TRAIN')

summary(TOP_TRAIN)
summary(TOP_TEST)

# LOOPING WITH MYSQL ------------------------------------------------------
library(purrr)
SQL_STATEMENT <- 'SELECT * FROM RDA_P_REVR.EUA_TRAIN WHERE day_of_year_shift ='
model_id <- c(1:2)
QUERY_LIST <- map2(.x = SQL_STATEMENT,.y = model_id, .f = paste)
# http://stackoverflow.com/questions/14726114/rmysql-fetch-cant-find-inherited-method

QUERY <- paste0(QUERY_LIST[[1]],";")
dbGetQuery(conn = ch, statement = QUERY)
library(RMySQL)

install.packages('DBI')
DATA_LIST <- map(.x = QUERY_LIST[[1]], .f = dbGetQuery, ch) # list of DF in memory from sql VOL table commands
DATA_LIST <- map(.f = dbSendQuery, ch, .x = QUERY_LIST, ) # list of DF in memory from sql VOL table commands
map()
data_query <- dbSendQuery(ch, QUERY_LIST[[1]])
data <- dbFetch(data_query)
df <- c(1,2)


DATA_LIST <- map(.f = dbGetQuery, .x = QUERY, ch) # list of DF in memory from sql VOL table commands

purrr::flatten(newNames)

library(data.table)
TEST <- as.data.table(TEST)

library(tidyverse)

load('data/TEST_ACTUAL.rda') %>% as.data.table()
load('data/TEST.rda') %>% as.data.table()

train <- TEST_ACTUAL[,.(CUSTOMER_KEY, year_day, TOU_CD, mdy, model_indicator, X, Actual_Usage)]
setnames(train, old = colnames(train), new = c('customer','day_of_year_shift','tou_cd','usg_dt'
                                               , 'temp_scenario','X','Y'))
train[, customer:=factor(sprintf("%010i", customer))]

test <- TEST[,.(CUSTOMER_KEY, year_day, TOU_CD, mdy, model_indicator, X, X_sd)]
setnames(test, old = colnames(test), new = c('customer','day_of_year','tou_cd','usg_dt'
                                             , 'temp_scenario','X','X_sd'))

test[, customer:=factor(sprintf("%010i", customer))]
setkey(test, customer)

cust <- copy(test[,.(customer = unique(customer))])
cust[, model_id_sm := seq(from = 1, to = 10),]
cust[, model_id := seq(from = 1, to = 5),]
cust[model_id_sm == 2,]
cust[model_id == 2,]


dbWriteTable(ch, "EUA_TRAIN",train, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(ch, "EUA_TEST",test, append = FALSE, overwrite = TRUE, row.names = FALSE)
dbWriteTable(ch, "EUA_COMPLETE",cust, append = FALSE, overwrite = TRUE, row.names = FALSE)


# create sample customer data customers -----------------------------------
library(data.table)
library(lubridate)
library(RMySQL)
ch <- dbConnect(MySQL(),
                user = 'root',
                host = 'localhost',
                dbname = 'RDA_P_REVR')

# CREATE COMPLETE CUSTOMER TABLE DATA
cust <- data.table(customer=1:100)
setkey(cust, customer)
cust[, customer:=factor(sprintf("%010i", customer))]
cust[, model_id_sm := seq(from = 1, to = .N/1),]
cust[, model_id := seq(from = 1, to = .N/10),]
cust[model_id_sm == 1,]
cust[model_id == 1,]
cust[,.N, by = .(model_id_sm)]
cust[,.N, by = .(model_id)]

# PUT SAMPLE CUSTOMERS IN COMPLETE OBSERVATIONS
dbWriteTable(ch, "EUA_COMPLETE",cust, append = TRUE, overwrite = FALSE, row.names = FALSE)
dbDisconnect(ch)

# train
customer <- rep(cust[1001:10000,customer], each = 366*2)
train_start_date <-  ymd('2016-01-01')
train_end_date <- ymd('2016-12-31')
usg_dt <- seq.Date(train_start_date,train_end_date, by = 'days')

temp_scenario <- 'Temperature_Normal'
dt <- data.table(customer, usg_dt, temp_scenario)

dt[,month := data.table::month(usg_dt),]
dt[,hour := rep(c(0:1)), by = .(usg_dt)]
dt[,day_of_week := wday(usg_dt), ]
dt[,season := ifelse(month %in% c(10, 11, 12, 1, 2, 3),'W','S'), ]
dt[,tou_cd := paste0(season, ifelse(day_of_week %in% c(1,7), 'OP',ifelse(hour %in% c(1),'OP', 'PK'))), ]
setkey(dt, customer, usg_dt)

dt[usg_dt == '2016-01-04' & customer == '0000000001', ]

dt[,usg_dt_shift := lubridate::ymd(usg_dt) - lubridate::days(2),]
dt[,day_of_year_shift :=  yday(usg_dt_shift),]
dt[usg_dt_shift<train_start_date,day_of_year_shift :=day_of_year_shift+1, ]

dt[,`:=` (X = ifelse(tou_cd %in% c('WPK','WOP')
                     ,rnorm(n=50, 45, 5)
                     ,rnorm(n=50, 90, 5)),
          Y = abs(ifelse(tou_cd %in% c('WPK','WOP')
                         ,rnorm(n=100, 3, 5)
                         ,abs(rnorm(n=100, 10, 5))))
)]

dt[day_of_year_shift %in% c(229),]
dt[day_of_year_shift %in% c(2),]
dt[day_of_year_shift %in% c(1),]

dt[,mean(X), by = .(tou_cd)]
dt[,max(X), by = .(tou_cd)]
dt[,max(Y), by = .(tou_cd)]
dt[,mean(Y), by = .(tou_cd)]

dt <- dt[,.(X = mean(X), Y = sum(Y)), by = .(customer, day_of_year_shift, tou_cd, usg_dt, temp_scenario)]

fwrite(dt[,.(customer, day_of_year_shift, tou_cd, usg_dt, temp_scenario, X, Y)], 'train10k.csv',append = FALSE)
rm(dt)

# test
customer <- rep(cust[1001:10000,customer], each = 365*2)
test_start_date <-  ymd('2017-01-01')
test_end_date <- ymd('2017-12-31')
usg_dt <- seq.Date(test_start_date,test_end_date, by = 'days')
week_day <- lubridate::wday(usg_dt, label = TRUE, abbr = TRUE)
month <- lubridate::month(usg_dt)
season <- ifelse(month %in% c(10, 11, 12, 1, 2, 3),'W','S')
tou_cd <-  paste0(season, ifelse(week_day %in% c('Sat','Sun'), 'OP','PK'))
temp_scenario <- 'Temperature_Normal'
dt <- data.table(customer, usg_dt, temp_scenario)

dt[,month := data.table::month(usg_dt),]
dt[,hour := rep(c(0:1)), by = .(usg_dt)]
dt[,day_of_week := wday(usg_dt), ]
dt[,day_of_year :=  yday(usg_dt),]
dt[,season := ifelse(month %in% c(10, 11, 12, 1, 2, 3),'W','S'), ]
dt[,tou_cd := paste0(season, ifelse(day_of_week %in% c(1,7), 'OP',ifelse(hour %in% c(1),'OP', 'PK'))), ]
setkey(dt, customer, usg_dt)

dt[,`:=` (X = ifelse(tou_cd %in% c('WPK','WOP')
                     ,rnorm(n=50, 45, 5)
                     ,rnorm(n=50, 90, 5)),
          X_sd = abs(ifelse(tou_cd %in% c('WPK','WOP')
                            ,rnorm(n=100, 2, 5)
                            ,abs(rnorm(n=100, 2, 5))))
), by = .(usg_dt, tou_cd)]

dt[day_of_year %in% c(1, 200, 220),][order(day_of_year)]
dt[,max(X), by = .(tou_cd)]
dt[,mean(X_sd), by = .(tou_cd)]

dt <- dt[,.(X = mean(X), X_sd = mean(X_sd)), by = .(customer, day_of_year, tou_cd, usg_dt, temp_scenario)]

fwrite(dt, 'test10k.csv', append = FALSE)

RMySQL::dbDisconnect(ch)
rm(dt)
rm(list = ls())



# https://db.rstudio.com/dplyr/

install.packages("odbc")


con <- DBI::dbConnect(RMySQL::MySQL(),
                      host = 'localhost',
                      user = 'root',
                      user = "hadley",
                      password = rstudioapi::askForPassword("Database password")
)

con <- DBI::dbConnect(odbc::odbc(),
                dbname = 'RDA_P_REVR')
