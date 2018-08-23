
# Load packages -----------------------------------------------------------

# Data preparation script
gc()
rm(list = ls())
PACKAGES <- c('ggplot2', 
              'data.table',
              'tidyr',
              # 'plyr',
              'dplyr', 
              'randomForest', 
              'earth', 
              'lubridate',
              'rattle',
              'RCurl',
              'rpart',
              'rpart.plot',
              'haven',
              'stringr',
              'broom',
              'purrr')

# columns for manipulating interval data
CHAR_COLS <- c('sp_id', 'sa_id', 'prem_id','Meter Id', 'start_date','stop_date'
               , 'usage date', 'daily_end')

COLOR_PALETTE <- list('blue'='#1f77b4', 'orange'='#ff7f0e', 
                      'green' = '#2ca02c', 'red' = '#d62728', 
                      'purple'='#9467bd', 'brown' = '#8c564b',
                      'pink' = '#e377c2', 'grey' = '#7f7f7f', 
                      'yellow' = '#bcbd22', 'teal' = '#17becf')

HDD_BASE = 65

# source function and data formatting scripts
source('gua/r/helper_functions.R')
# Load packages
sapply(PACKAGES, get_packages)

# Load formated usage and temp data ---------------------------------------
usage_weather <- fread('data/usage_weather.csv',
                       integer64 = 'numeric',
                       header = TRUE)


# create train and test data frames  -------------------------------------------------------------

train_df <- usage_weather %>%
  filter(train_indicator == 'TRAIN') %>%
  group_by(prem_id)

test_df <- usage_weather %>%
  filter(train_indicator == 'TEST') %>%
  group_by(prem_id)

d <- list(train = split(train_df, train_df$prem_id)
          , test = split(test_df, test_df$prem_id)) %>%
  as_data_frame()

d <- d %>% mutate(
  # Fit the models
  models = map(train, ~ lm(usg ~ temp, data = .)),

  # extract training model elements
  tidy_train = map(models, broom::tidy), #broom
  glance_train = map(models, broom::glance), #broom
  augment_train = map(models, broom::augment),
  rsq_train = map_dbl(glance_train, 'r.squared'), # extract from model element to compare to below
  rsq_check = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), r_squared), #model level
  rmse_train = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), rmse), #model level
  #
  n = map(train, NROW),

  # Make predictions on test data
  pred = map2(models, test, predict),

  # Get the error
  rsq_test = map2(test %>% map("usg_actual"), pred, r_squared),
  rmse_test = map2(test %>% map("usg_actual"), pred, rmse),
  msd_test = map2(test %>% map("usg_actual"), pred, msd),
  # add rmse to both
  #
  # # Get the error
  rsq_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), r_squared),
  msd_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), msd),
  rmse_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), rmse))

# Calculate model fit -----------------------------------------------------

# extract elements from train list in data frame d
train_unnest <- d %>% select(train) %>%
unnest() %>%
select(year, prem_id, date, train_indicator, month_name, month, usg, temp, temp_actual)

train_augment <- d %>% select(augment_train) %>%
  unnest() %>%
  select(.fitted, .resid, .hat)

# calculate errors at monthly level for in sample
temp_train <- cbind(train_unnest, train_augment) %>% # combine elements of interest
  group_by(year, month_name, train_indicator) %>%
  summarise(y = sum(usg)
            , y_hat = sum(.fitted)
            # , rmse_1 = sqrt((y - y_hat)^2/n())
            , rmse = sqrt(sum(.resid)^2/n())
            , n =n()) %>% 
  gather(key = variable, value = val, c(rmse))

# extract elements from test list in data frame d
test_augment <- d %>% select(test, pred) %>%
  unnest() %>%
  select(year, prem_id, date, train_indicator, opr_area_cd, climate_zone_cd
         , month_name, month, month_day, year_day, day_name,
         temp, temp_actual, usg, usg_actual, pred) %>%
  mutate(.resid = usg_actual - pred)

# calculate errors on monthly level for out of sample
temp_test <- test_augment %>%
  group_by(year, month_name, train_indicator) %>%
  summarise(y = sum(usg)
            , y_hat = sum(pred)
            # , rmse_1 = sqrt((y - y_hat)^2/n())
            , rmse = sqrt(sum(.resid)^2/n())
            , n =n()) %>% 
  gather(key = variable, value = val, c(rmse))


# extract elements from test and train list in data frame d for baseline model comparison
train_elements <- cbind(train_unnest, train_augment) %>% 
  select(year, month_name, usg_train = usg, temp_train = temp_actual, .fitted, .resid)

test_elements <- test_augment %>% 
  select(temp_test = temp, usg_test = usg_actual, pred, temp_actual)

cons_model <- cbind(train_elements, test_elements) %>%
  mutate(train_indicator = 'CONS') %>%
  select(year, month_name, train_indicator, usg_train, usg_test, pred) %>% 
  group_by(year, month_name, train_indicator) %>%
  summarise(y = sum(usg_test)
            , y_hat = sum(usg_train)
            , rmse = sqrt((y - y_hat)^2/n())
            # , rmse = sqrt(sum(.resid)^2/n())
            , n =n()) %>% 
  gather(key = variable, value = val, c(rmse))

# combine in sample, out of sampel and baseline model details
model_comp <- rbind(cons_model, temp_test, temp_train) %>% 
  arrange(year,train_indicator, desc(month_name))

model_comp$month_name <- factor(model_comp$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

# compare changes from baseline and out of sample and baseline in sample 
model_change <- model_comp %>%
  ungroup() %>% 
  select(month_name, variable, val, train_indicator)  %>% 
  spread(key = train_indicator, value = val) %>% 
  mutate(test_diff = CONS - TEST,
         test_pct = round(test_diff/CONS * 100,1),
         train_diff = CONS - TRAIN,
         train_pct = round(train_diff/CONS * 100,1),
         model_diff = ifelse(TEST <= CONS, "Improvement", "No Improvement"))

# Plotting model fit ------------------------------------------------------
# compare model rmse with scatter plot
scatter_comp <- ggplot(data = model_comp) +
  geom_point(aes(x = month_name, y = val, color = train_indicator)) +
  facet_wrap(~variable) +
  scale_color_discrete("Comparing Model Types"
                       ,labels=c('Constant Use Model'
                                 ,'Temperature Model - out of sample'
                                 ,'Temperature Model - in sample')) +
  ggtitle("Comparing model RMSE for gas usage") +
  labs(y = 'RMSE \n(lower is better)', x = NULL)

# compare model rmse with bar plot
bar_comp <- ggplot(data = model_comp) +
  geom_bar(aes(x = month_name, y = val, fill = train_indicator), stat = 'identity', position = 'dodge') +
  facet_wrap(~variable) +
  scale_fill_discrete("Comparing Model Types"
                       ,labels=c('Constant Use Model'
                                 ,'Temperature Model - out of sample'
                                 ,'Temperature Model - in sample')) +
  ggtitle("Comparing model RMSE for gas usage") +
  labs(y = 'RMSE \n(lower is better)', x = NULL)

# percent improvement of temperature compared to baseline (constant usage) 
change_bar <- ggplot(model_change, aes(x = month_name, y = test_pct)) +
  geom_bar(aes(fill = model_diff), stat = 'identity', position = 'dodge')+
  scale_fill_discrete("Improvement Indicator"
                      ,labels=c('Better than Constant Usage'
                                ,'Worse than Constant Usage')) +
  ggtitle("Percent improvement in rmse  \nfor temperature model vs. constant use model") +
  labs(y = 'percent improvement', x = NULL) +
  geom_text(aes(label = test_pct))

# Calculate HDD comparisons ---------------------------------------------------------

# calculate train hdd
hdd_train <- d %>% select(train) %>%
  unnest() %>%
  select(date,year, opr_area_cd, prem_id, year, month_name, train_indicator
         , temp_actual= temp, season) %>%
  mutate(train_indicator = paste0(train_indicator,'_', min(year),'-', max(year))) %>%
  distinct(date, year, opr_area_cd, month_name, train_indicator, temp_actual) %>%
  arrange(date) %>%
  mutate(hdd_actual = get_hdd(temp_actual, HDD_BASE)) %>%
  group_by(opr_area_cd, year, month_name, train_indicator) %>%
  select(opr_area_cd, train_indicator, year, month_name, hdd_actual) %>%
  gather(key = variable, value = val, -c(opr_area_cd, train_indicator, year, month_name)) %>%
  group_by(opr_area_cd, train_indicator, year, month_name, variable) %>%
  summarise(val = sum(val)) %>% arrange(year, month_name, opr_area_cd)

# calculate test hdd
hdd_test <- d %>% select(test) %>%
  unnest() %>%
  select(opr_area_cd, year, train_indicator, date, year, month_name
         ,opr_area_cd
         , temp_actual
         , temp_normal = temp) %>%
  mutate(train_indicator = paste0(train_indicator,'_', min(year),'-', max(year))) %>%
  distinct(date, opr_area_cd, year, month_name, train_indicator, temp_actual, temp_normal) %>%
  arrange(date) %>%
  mutate(hdd_actual = get_hdd(temp_actual, HDD_BASE),
         hdd_normal = get_hdd(temp_normal, HDD_BASE)) %>%
  group_by(opr_area_cd, year, month_name, train_indicator) %>%
  select(opr_area_cd, train_indicator, year, month_name, hdd_actual, hdd_normal) %>%
  gather(key = variable, value = val, -c(opr_area_cd, train_indicator, year, month_name)) %>%
  group_by(opr_area_cd, train_indicator, year, month_name, variable) %>%
  summarise(val = sum(val)) %>% arrange(year, month_name, opr_area_cd)

# combine train and test hdd
hdd_data <- rbind(hdd_train, hdd_test)

hdd_data$month_name <- factor(hdd_data$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

# create regex pattern to extract year variables
reg_pattern <-  c('\\w\\d{3}[-.]?\\d{3}[-.]?\\w')

# create normal and actual hdd factor for train and test
hdd_data$reg_out <- paste(hdd_data$variable
                          , str_extract(string = hdd_data$train_indicator, pattern = reg_pattern)
                          , sep = '_')

# Plotting HDD ---------------------------------------------------

# comparing actual and normal HDD 65 for train and test periods
bar_hdd <- ggplot(data = hdd_data %>% group_by(month_name, year, reg_out) %>% 
         summarise(avg_hdd = mean(val))) +
  geom_bar(aes(x = month_name, y = avg_hdd, fill = reg_out), stat = 'identity'
           , position = 'dodge') +
  scale_fill_discrete(name="Heating Degree Days"
                      ,labels=c('Winter 2013-2014 Actual',
                                'Winter 2014-2015 Actual',
                                'Winter 2014-2015 Normal')) +
  labs(y = "Heating Degree Days", x = NULL)+
  ggtitle("Comparing Actual and Normal HDD-65")

# focus on hdd when temp model shows improvement
better_plot <- ggplot(data = hdd_data %>% group_by(month_name, year, reg_out) %>% 
         summarise(avg_hdd = mean(val)) %>% dplyr::left_join(model_change, by = 'month_name') %>% 
         select(-c(CONS, TEST, TRAIN, test_diff)) %>% 
         filter(model_diff == 'Improvement')) +
  geom_bar(aes(x = month_name, y = avg_hdd, fill = reg_out), stat = 'identity'
           , position = 'dodge') +
  facet_wrap(~model_diff) +
  scale_fill_discrete(name="Heating Degree Days"
                      ,labels=c('Winter 2013-2014 Actual',
                                'Winter 2014-2015 Actual',
                                'Winter 2014-2015 Normal')) +
  labs(y = "Heating Degree Days", x = NULL)+
  ggtitle("Differences in HDD-65 where Temp model is better")

# focus on hdd when temp model shows no improvement
worse_plot <- ggplot(data = hdd_data %>% group_by(month_name, year, reg_out) %>% 
         summarise(avg_hdd = mean(val)) %>% dplyr::left_join(model_change, by = 'month_name') %>% 
         select(-c(CONS, TEST, TRAIN, test_diff)) %>% 
         filter(model_diff != 'Improvement')) +
  geom_bar(aes(x = reorder(month_name, year), y = avg_hdd, fill = reg_out), stat = 'identity'
           , position = 'dodge') +
  facet_wrap(~model_diff) +
  scale_fill_discrete(name="Heating Degree Days"
                      ,labels=c('Winter 2013-2014 Actual',
                                'Winter 2014-2015 Actual',
                                'Winter 2014-2015 Normal')) +
  labs(y = "Heating Degree Days", x = NULL)+
  ggtitle("Differences in HDD-65 where Constant use model is better")

# Export files ------------------------------------------------------------

write.table(x = model_change, file = 'model_pct_change.txt')
write.table(x = model_comp, file = 'model_comparisions.txt')
# write.table(x = usage_weather, file = 'data/usage_weather.csv', col.names = TRUE, row.names = FALSE)



