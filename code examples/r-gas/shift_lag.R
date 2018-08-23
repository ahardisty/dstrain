# on data.tables
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
# lag columns 'v1,v2,v3' DT by 1 and fill with 0
cols = c("v1","v2","v3")
anscols = paste("lead", cols, sep="_")
DT[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]

DT = data.table(year=rep(2010:2011, each=3), v1=1:6)
DT[, c("lag1", "lag2") := shift(.SD, 1:2), by=year]


weather_wide[is.na(temp_model),temp_model:= lapply(.SD, head, 1)
             , .SDcols = 'Temperature', by = key(weather_wide)]

weather_wide[opr_area_cd == 'VV'
             & year_day %in% c(70),.( opr_area_cd, year, year_day, Temperature, temp_model)]

setkey(interval_usage, date, opr_area_cd)
setkey(weather_wide, date, opr_area_cd)


ans_cols <- c(paste0('usage_lag',c(1,3)),paste0('temp_lag',c(1,3)))
cols <- c('usage','temp')
paste(cols, 1:3)
set
small_dt[,c(ans_cols) := shift(.SD, c(1,3), type = 'lag')
         ,by = key(small_dt),.SDcols = c('usage', 'Temperature'),]


small_dt[year_day == '10',.(prem_id, year, year_day, Temperature, temp_2015, usage, usage_2015)]


# usage_weather_daily_wide <- dcast.data.table(data = usage_weather_daily
#                                              , formula = opr_area_cd 
#                                              + climate_zone_cd + date + prem_id
#                                              + sp_id + usage
#                                              ~ type
#                                              , value.var = c('avg_val'))
# 

setnames(usage_weather_daily_wide, old = colnames(usage_weather_daily_wide)
         , new = gsub(pattern = 'Temperature', replacement = 'temp_actual_avg'
                      , x = colnames(usage_weather_daily_wide)))

usage_weather_daily_wide <- usage_weather_daily_wide[year != 2012,]


# for usage
small_dt <- usage_weather[prem_id %in% small_list,]

setkey(small_dt, prem_id, year_day)




# keep this ---------------------------------------------------------------

small_dt[, c('usage_model', 'temp_model') := lapply(.SD, tail, 1)
         , .SDcols = c('usage','Temperature'), by = key(small_dt)]


# Create a nested data frame
d2 <- list(train = split(mtcars, mtcars$cyl), test = split(mtcars, mtcars$cyl)) %>% 
  as_data_frame()
d2

d <- d %>% mutate(
  # Fit the models
  models = map(train, ~ lm(mpg ~ wt, data = .)),
  
  # Make predictions on test data
  preds = map2(models, test, predict),
  
  # Get the error
  diffs = map2(preds, test %>% map("mpg"), msd),
  
  # Get the error
  diffs2 = map2(train %>% map('mpg'), test %>% map("mpg"), msd)
)

# Evaluate mean-squared difference between predicted and actual
unlist(d$diffs)

by_model_type  <-  usage_weather %>% 
  filter(model_type == 'train') %>% 
  group_by(model_type, train_details, train_indicator, prem_id) %>% 
  nest()

in_sample <- by_model_type %>% 
  mutate(test = map(model, predict), #broom
         glance = map(model, broom::glance), #broom
         augment = map(model, broom::augment)) #broom



model_summary <- in_sample_augment_dt[,.(rmse = rmse(usg, .fitted),
                                         rsq = r_squared(y = usg, y_hat = .fitted))
                                      , by = .(model_type, train_details, train_indicator)
                                      ][order(model_type)]

model_summary_2 <- in_sample_augment_dt[train_indicator != 'TEST-2015',.(rmse = rmse(usg, .fitted),
                                                                         rsq = r_squared(y = usg, y_hat = .fitted))
                                        , by = .(model_type, prem_id, month_name, year)
                                        ][order(year)]

model_summary_3 <- in_sample_augment_dt[train_indicator != 'TEST-2015',
                                        .(usg = sum(usg),
                                          fitted = sum(.fitted))
                                        , by = .(model_type, prem_id, month_name, year)]

model_summary_3[,.(rmse = rmse(usg, fitted)
                   ,rsq = r_squared(y = usg, y_hat = fitted))
                , by = .(year)
                ][order(year)]


in_sample_augment_dt <- as.data.table(in_sample_data_aug)
# rm(in_sample_augment_dt)

add_time(in_sample_augment_dt)

rmse(y = in_sample_augment$usg, y_hat = in_sample_augment$.fitted)

in_sample_augment_dt[,rmse := rmse(usg, .fitted)
                     , by = .(model_type, train_details, train_indicator)]


str(in_sample_augment)


mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = flower_att, value = measurement, -Species)


# get first observation for each Species in iris data -- base R
mini_iris <- iris[c(1, 51, 101), ]
# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
gather(mini_iris, key = flower_att, value = measurement,
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
# same result but less verbose
gather(mini_iris, key = flower_att, value = measurement, -Species)

# repeat iris example using dplyr and the pipe operator
library(dplyr)
mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = flower_att, value = measurement, -Species)



map(mt$blue, )

glance_train = map(models, broom::glance), #broom
d %>%models map(~ models, broom::glance)
map(d$models, broom::glance)
d %>% map(~models, broom::glance)
d %>% map('models') %>% unnest()

map(.x = d$models, .f = broom::augment)

d <- d[[1]][c(1:2)] %>%
  split(.$prem_id) %>%
  models = map( ~ lm(usg ~ temp, data = .)) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(, list(broom::glance, broom::augment)) %>%  
  map(broom::glance)  
  
  map(summary) %>%
  map_dbl("r.squared")
  
  
  1:10 %>%
  %>%
    
    pmap(.f = list())
    pmap(1:10, 10:1, .f = `*`)
    map_dbl(mean)
  
    x <- list(1, 10, 100)
    y <- list(1, 2, 3)
    map2(x, y, ~ .x + .y)
    # Or just
    map2(x, y, `+`)    
    
  rnorm(n = 10, mean = 1, sd = 1)
  
  
  
  
  
# lag 5 -------------------------------------------------------------------

  # train and test usage = current year usage
  # lag_terms <- c(paste0('lag_',1:5))
  # weather_wide[,c(lag_terms) := shift(.SD, 1:length(lag_terms),0, 'lag')
  #              , .SDcols = 'temperature', by = key(weather_wide)]
  #
  # temp norma and temp actual use actual usage as placeholder
  
  
  # weather_five[,sum(lag_1,lag_2,lag_3), by = key(weather_five)]
  # weather_wide[,mean_2:=rowMeans(.SD), .SDcols = c('lag_1','lag_2'), by = key(weather_wide)]
  # weather_wide[,mean_5_3:=rowMeans(.SD), .SDcols = c('lag_3','lag_4','lag_5'), by = key(weather_wide)]
  # weather_wide[,max_5:=max(.SD), .SDcols = c('lag_1','lag_2','lag_3','lag_4','lag_5'), by = key(weather_wide)]
  # weather_wide[,min_5:=min(.SD), .SDcols = c('lag_1','lag_2','lag_3','lag_4','lag_5'), by = key(weather_wide)]
  # weather_wide[,median_5:=lapply(.SD, median), .SDcols = c('lag_1','lag_2','lag_3','lag_4','lag_5')
  #              , by = key(weather_wide)]
  
  rowSums()
  
  # gc()
  # d <- list(train = split(train_df, train_df$prem_id)
  #           , test = split(test_df, test_df$prem_id)) %>%
  #   as_data_frame()
  
  
  weather_wide[,c(paste0('LAG_',1:5)) := shift(.SD,1:5,'lag')
               , .SDcols = 'temp_actual', by = key(weather_wide)] 
  
  # in sample (TRAIN) for lagged terms uses actual temp from train year
  weather_wide[train_indicator == 'TRAIN',c(paste0('LAG_',1:5)) := shift(.SD,0,'lag')
               , .SDcols = 'temp_actual', by = key(weather_wide)] 
  
  # create single aggregated lag term
  
  wshort[,MEDIAN_1_5:= median(c(LAG_1, LAG_2, LAG_3, LAG_4, LAG_5)), by = .(date)]
  wshort[,MEAN_LAG:= mean(c(LAG_1:LAG_5)), by = .(date)]
  wshort[,MEDIAN_3_5:= median(c(LAG_3, LAG_4, LAG_5)), by = .(date)]
  wshort[,MEDIAN_1_2:= median(c(LAG_1, LAG_2)), by = .(date)]
  wshort[,MAX_1_5:= max(LAG_1, LAG_2, LAG_3, LAG_4, LAG_5), by = .(date)]
  wshort[,MIN_1_5:= min(LAG_2, LAG_3, LAG_4, LAG_1, LAG_2, LAG_3, LAG_4, LAG_5), by = .(date)]
  wshort
  example(copy)
  
    
  
  # fit and predict on usage model ------------------------------------------
  
  # train_df <- train_df %>% mutate(
  #   # Fit the models
  #   mod = data %>% map(usg_temp_lm))
  # 
  # # extract training model elements
  # train_df <- train_df %>% mutate(augment_train = map(mod, broom::augment)) #broom
  
  # # rsq_norm = map_dbl(glance_train_norm, 'r.squared'), # extract from model element to compare to below
  # # rsq_actual = map_dbl(glance_train_actual, 'r.squared'), # extract from model element to compare to below
  # rmse_norm = map2_dbl(augment_train_norm %>% map("usg"), augment_train_norm %>% map(".fitted"), rmse), #model level
  # train_df <- train_df %>% mutate(rmse = map2_dbl(augment_train %>% map("usg")
  #                                                 , augment_train %>% map(".fitted"), rmse)) #model level
  
  # Make predictions on test data
  
  # head(test_df) <- test_df %>% mutate(
  #   # Fit the models
  #   pred = data %>% map(train_df$mod, predict))
  # 
  # test_df <- test_df %>% mutate(pred_actual = map2_dbl(train_df$mod, test_df$data, predict))
  # rmse_norm = map2_dbl(augment_train_norm %>% map("usg"), augment_train_norm %>% map(".fitted"), rmse), #model level
  
  
  # Get out of sample error
  # rsq_pred_norm = map2_dbl(test %>% map("usg_actual"), pred_normal, r_squared),
  # d <- d %>% mutate(rmse_pred_actual = map2_dbl(test %>% map("usg_actual"), pred_actual, rmse))
  #
  # # Get the error
  # rsq_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), r_squared),
  # msd_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), msd),
  # rmse_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), rmse))
  
  d <- d %>% mutate(rmse_actual = map2_dbl(augment_train_actual %>% map("usg"), augment_train_actual %>% map(".fitted"), rmse)) #model level
train_actual <- d %>% select(augment_train_actual) %>%

  
  train <- d %>%
  mutate(date = TRAIN %>% map('date'),
         opr_area_cd = TRAIN %>% map('opr_area_cd'),
         usage_actual = TRAIN %>% map('usage_actual'), 
         climate_zone_cd = TRAIN %>% map('climate_zone_cd'), 
         month_name = TRAIN %>% map('month_name')
  ) %>% 
  head()
unnest() %>% 
  mutate(train_indicator = 'TRAIN') %>% 
  mutate(.resid = usg_actual - fitted)
10/22 -1
# summary(as.factor(hdd_data$train_indicator))
# # create regex pattern to extract year variables
# reg_pattern <-  c('\\w\\d{3}[-.]?\\d{3}[-.]?\\w')
# 
# # create normal and actual hdd factor for train and test
# hdd_data$reg_out <- paste(hdd_data$variable
#                           , str_extract(string = hdd_data$train_indicator, pattern = reg_pattern)
#                           , sep = '_')
# summary(as.factor(hdd_data$reg_out))
# head(hdd_dt)


# template <- melt.data.table(template, id.vars = c('customer'),measure = list(BASE_COLS, CDT_COLS, PDT_COLS)
#                         , value.name = c('BASELINE','CDT_DATE','PDT_DATE'))


# purrr map and walk functions --------------------------------------------

# write
pwalk(.l = list(df_list, paths), .f = write.csv, row.names = FALSE)

# paste
pmap(.l = list(names(df_list), paths), .f = paste0)
file_ext <- list(csv = rep(c('.csv','.txt'), each = 1))
# paste multiple things
flat <- mapply(paste0, paths, file_ext, SIMPLIFY = TRUE)

# csv_files <- paste0(OUT_LOC,names(df_list),list('.csv'))
# txt_files <- paste0(OUT_LOC,names(df_list),list('.txt'))

example(pwalk)
paths <- list(csv_files, txt_files)
pwalk(df_list, paths, length, row.names = FALSE)


map2(out3 %>% map_df('N'),out3 %>% map_df('num'),data.frame)
# create new variables; mapping to create tibbles


map2(df.list %>% map('V1'), df.list %>% map('V2'), tibble)

get_avg <- function(x){
x$num <- x$N/2
x
}

get_year <- function(x){
  x$current_year <- TEST_YEAR
  x$prior_year <- TRAIN_YEAR
  x
}

str(m_var)
pasted <- get_year(m_var)
m_var <- map(m_var, get_year)
m_var <- map(m_var, get_avg)
N2 <- map_df(m_var[1], ~.$N * 2)

out4[[3]][c(1,4, 8, 9)]

get_avg_n <- function(x, n = 'N'){
  n <- m_var %>% map_df(head)
  return(n)
  # col_num <- which(names(n) == 'N')
  # x$x_var <- x %>% map_dbl(col_num)
  # return(x)
}


out3 <- map(m_var, get_avg)

n_col <- which(colnames(m_var) == N)

m_var %>% map(colnames)





# df_list <- list(
#   billing_merge_wide[train_indicator == 'TRAIN' & usage_type == 'usage_actual',][,c(KEEP_COLS), with = FALSE], 
#   billing_merge_wide[train_indicator == 'TEST' & usage_type == 'usage_actual',][,c(KEEP_COLS), with = FALSE],
#   billing_merge_wide[train_indicator == 'TEST' & usage_type == '.pred',][,c(KEEP_COLS), with = FALSE],
#   billing_merge_wide[train_indicator == 'BASELINE'& usage_type == '.pred',][,c(KEEP_COLS), with = FALSE]
#   )

# walk2(df_list, path, write.csv, row.names = FALSE)




# CAST_COLS <- c(make_cols(x = c('CSCH__', 'TR1__','TR2__','DAY__','PDT__', 'CDT__','LNUMUNIT','NUMUNIT'), WIN_MONTHS, paste0), 'CSCHED', 'CATFLAG')


# long m ------------------------------------------------------------------
