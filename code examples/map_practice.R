# map functions
search()
library(dplyr)
library(purrr)
library(data.table)
library(magrittr)
library(lubridate)

MONTH_COLS <- 1:12 # used to create BILL_CYCLE variables and for full 12 month GBC
RENUM_COLS <- 1:2 # used to calculate truncated GBC
MON_NUM_1D <- match(c('Dec','Mar'), month.abb) # month numbers with one digit
MON_NUM_2D <- sprintf('%02d', match(c('Dec','Mar'), month.abb)) # month numbers with two digits
SAMPLE_LIST <- list(paste0('sample',1:2,'/') )

DIR <- paste0('sample',1:3)
CLIMATE_ZONES <- c('t','w','x')
TEST_YEAR <- 2014
TRAIN_YEAR <- 2015
LOCATION <- '_tou_kamal_'
FILE_TYPE <- '.txt'

# http://r4ds.had.co.nz/lists.html#undefined
# map
mu <- list(5, 10, -3)
mu %>% map(rnorm, n = 10)
n <- list(100, 100, 100)
n %>% map(rnorm, n = 10)
# map2
sigma <- list(1, 5, 10)
size <- list(10, 20, 100)
from <- list(1,1,1,1,1,1)
to <- list(5, 10, 15, 20, 25, 30)



map2(.x = mu, .y = sigma, .f = rnorm, n = 10)
samples <- map2(.x = from, .y = to, .f = seq)
map(.x = n, .f = sample, size = 10)


# pmap
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>% pmap(rnorm)

# pmap with named arguments
args <- list(mean = mu, sd = sigma, n = n)
args %>% pmap(rnorm)

sample(1:100, 2)

args %>% pmap(paste)
args %>% pmap(rnorm)

# with strings
DIR <- list(paste0('sample'))
SAMPLE <- list(1:3)
DIR_SAMPLE <- paste0(DIR, 1:3)

args3 <- list(mean = mu, sd = sigma, n = n)
args3 %>% pmap(paste0)

invoke_map(list(runif, rnorm), list(list(n = 10), list(n = 5)))

invoke_map(list(sum, paste, paste0), list(list(n = 10)
                                  , list(n = c(5,3,5))
                                  , list(n = c('1','2','3'),c('one','two','three'))))

sample(100, 85)

?invoke_map()
# as data.frame
params <- dplyr::data_frame(mean = mu, sd = sigma, n = n)
params$result <- params %>% pmap(rnorm)

params %>% select(result) %>% unnest()


f <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

# Define params
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)


# Assign the simulated samples to sims
sims <- invoke_map(f, params, n = 50)


# Use walk() to make a histogram of each element in sims
sims %>% walk(~hist)


# random sample
nums <- 100
sizes <- list(seq(.1,1, by = .15))
samples <- map2(nums, sizes, `*`)
%>% (100, samples[[1]][3])
map(nums, sample, size = 2)
str(samples)
?sample()
str(sizes)
str(nums)

args <- list(mean = mu, sd = sigma, n = n)

args %>% pmap(paste)
args %>% pmap(sample)
map2(x = nums, y = size, .f = sample)
?sample()


?sample()
# list files in one directory
s1 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'TOU'
                 , full.names = TRUE, ignore.case = TRUE)

s1 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'TOU'
                 , full.names = TRUE, ignore.case = TRUE)
getwd()
dir('../CEA_DATA/interval_data/electricity_data/kamal/sample1/')
# use map for files in one directory
s2 <- map(.x = '../CEA_DATA/interval_data/electricity_data/kamal/sample1/', dir
          , pattern = 'TOU', full.names = TRUE, ignore.case = TRUE)



../CEA_DATA/interval_data/electricity_data/kamal/

s3 <- map(.x = c('../CEA_DATA/interval_data/electricity_data/kamal'), list.files
          , pattern = 'kamal.', full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
list.files()

s3 <- map(.x = c('../CEA_DATA/interval_data/electricity_data/kamal/sample1/'
                 ,'../CEA_DATA/interval_data/electricity_data/kamal/sample2/'), list.files
          , pattern = 'kamal.', full.names = TRUE, ignore.case = TRUE)

library(purrr)
library(data.table)
getwd()
interval <- list.files('../CEA_DATA/interval_data/electricity_data/kamal/sample2', full.names = TRUE)
full_inteval <- map(interval, fread, integer64 = 'character')
full_inteval <- rbindlist(full_inteval)
full_inteval[,lapply(.SD, uniqueN), by = period]

lapply(list, function)
library(dplyr)
?sample_frac()


str(sl_1)
str(sl_2)
str(full_inteval)
head(full_inteval)
full_sm <- full_inteval[1:10000]
full_inteval[,month:= data.table::month(date)]
?months()

?month()
BILL_CYCLE_MODEL <- flatten_chr(map2(list(TEST_YEAR, TRAIN_YEAR), list(MON_NUM_2D),paste0)) # bill cycle with two digits

BILL_CYCLE_MODEL <- flatten_chr(map2(list(SAMPLE_LIST), list(TEST_YEAR, TRAIN_YEAR), paste0)) # bill cycle with two digits
read_cycle <- flatten_chr(map2(SAMPLE_LIST, list(CLIMATE_ZONES),paste0)) # bill cycle with two digits

BILL_CYCLE_MODEL <- map2(list(SAMPLE_LIST), list(TEST_YEAR, TRAIN_YEAR), paste0) # bill cycle with two digits

BILL_CYCLE_MODEL <- pmap(list(list(TRAIN_YEAR, TEST_YEAR)
                              , list(LOCATION)
                              , list(CLIMATE_ZONES)
                              , list(FILE_TYPE)
                              ), paste0) # bill cycle with two digits

BILL_CYCLE_MODEL <- pmap(list(list(TRAIN_YEAR, TEST_YEAR)
                              , list(LOCATION)
                              , list(CLIMATE_ZONES)
                              , list(FILE_TYPE)
                              ), list.files) # bill cycle with two digits

BILL_CYCLE_MODEL <- pmap(list(list(TRAIN_YEAR, TEST_YEAR)
                              , list(LOCATION)
                              , list(CLIMATE_ZONES)
                              , list(FILE_TYPE)
                              ), paste0) %>% flatten_chr() %>%
  map2(list(DIR),., paste, sep = '/')# bill cycle with two digits
map3()

sapply(expand.grid(vars, vis), 1, paste, collapse=".")


str(BILL_CYCLE_MODEL)


# read files from multiple directories

r4 <- map(.x = s3[[1]], .f = fread, integer64 = 'numeric')
r5 <- pmap(.l = s3, .f = fread, sep = 'auto')
str(s3)
s4 <- pmap(.l = s3, .f = fread, header = TRUE, sep = 'auto'
           , check.names = TRUE
           , stringsAsFactors = TRUE
           , integer64 = 'character'
           , verbose = FALSE)

# s4 <- pmap(s3,fread, header = TRUE
#         , sep = 'auto'
#         , strip.white = TRUE
#         , check.names = TRUE
#         , stringsAsFactors = TRUE
#         , integer64 = 'character'
#         , verbose = FALSE
#         , na.strings = '?')



# MUTATE DF IN LIST OF DFS ------------------------------------------------
# https://stackoverflow.com/questions/40025785/mutate-within-map-of-a-list-within-list

r<-data.frame(o=runif(n = 50),m=rep(c("A","N"),25))
te<-data.frame(o=runif(n = 50),m=rep(c("G","H"),25))
aq<-list(f=list(df=r,g=0),g2=list(df=te,g=5))

map(aq, ~ list(df = dplyr::mutate(.x$df, g = .x$g), g = .x$g))


# https://stackoverflow.com/questions/46531582/using-purrrpmap-within-mutate-to-create-list-column

params <- expand.grid(param_a = c(2, 4, 6)
                      ,param_b = c(3, 6, 9)
                      ,param_c = c(50, 100)
                      ,param_d = c(1, 0)
)

df.preprocessed <- dplyr::as.tbl(params) %>%
  dplyr::mutate(test_var = purrr::map(param_a, function(x){
    rep(5, x)
  }
  ))

df.preprocessed %>% select(test_var) %>%
  unnest()

df.preprocessed <- dplyr::as.tbl(params) %>%
  dplyr::mutate(test_var = purrr::pmap(., function(param_a, param_b, ...){
    rep(5, param_a) * param_b
  })) %>%
  tidyr::unnest()

summary(df.preprocessed$test_var)


df %>%
  mutate(blue = modelObject_names[1])
hm[[1]] %>%
  mutate(model_pd = modelObject_names[1])
hm[1] %>%
  map(~ mutate(.,model_pd = modelObject_names[1])
  )
hm %>% # not quite
  map(~ mutate(.,model_pd = modelObject_names)
  )
# closer
map2(.x = hm, .y = modelObject_names, ~mutate(.x, report_pd = .y))

# closer still
hm %>%
  map2(., .y = modelObject_names, ~paste0(.x, .y))
# even closer
hm %>%
  map2(., .y = names(.), ~paste0(.x, .y))
# even closer closer
hm %>%
  map2(., .y = names(.), ~mutate(.x, report_pd = .y))

# DONE
hm <- modelObject_list %>% map(get_results) %>%
  map2(., .y = names(.), ~mutate(.x, report_pd = .y))

# create a list of scripts with list of variable --------------------------

SAMP_SIZE <- seq(from = 10, to = 125, by = 20)
rep()
tdQuery <- str_replace_all(paste("SELECT
 *
FROM RDA_P_REVR.EUA_TRAIN AS a JOIN
(
SELECT customer AS smpID
FROM RDA_P_REVR.CUST_REVR SAMPLE", SAMP_SIZE
,") AS b
ON a.customer = b.smpID"), "[\r\n]" , "")

tdQuery[1]
teradataDS <- RxTeradata(connectionString = tdConnString
                         , sqlQuery = tdQuery
                         , rowsPerRead = 50000)
map(.x = tdQuery[1], .f = RxTeradata(connectionString = tdConnString
                                     , rowsPerRead = 50000))

d <- map(.x = d_list[3], .f = fread, header = TRUE, sep = 'auto'
         , check.names = TRUE
         , stringsAsFactors = TRUE
         , integer64 = 'character'
         , verbose = FALSE)


