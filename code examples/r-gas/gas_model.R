
gc()
train_test <- fread('../CEA_DATA/weather_data/train_test.txt')
hdd_dt <- fread('../CEA_DATA/weather_data/hdd_dt.txt')
baseline_usage <- fread('../CEA_DATA/weather_data/baseline_usage.txt')

# Fit and predict  -----------------------------------------------
gc()

random_customers <- train_test %>%
distinct(customer) %>%
sample_frac(1) %>%
ungroup()

# start_time <- proc.time()

train <- train_test%>%
  filter(customer %in% (random_customers$customer)) %>% 
  filter(train_indicator == 'TRAIN') %>% 
  filter(model_name == 'TEMP_NORMAL') %>%
  filter(outlier == FALSE) %>% # keep outliers out of modeling
  filter(climate_zone_cd %in% CLIMATE_ZONES) %>% # keep only climate zones of interest
  group_by(customer, prem_id, acct_id, opr_area_cd, climate_zone_cd, train_indicator) %>% 
  nest(.key = TRAIN) %>% 
  # spread(key = train_indicator, value = data)%>% 
  mutate(
    models = map(TRAIN, usg_temp_lm), # fit linear model
    augment_train = map(models, broom::augment),
    glance_train = map(models, broom::glance),
    tidy_train = map(models, broom::tidy)#model level
  )

test <- train_test %>%
  filter(customer %in% (random_customers$customer)) %>% 
  filter(train_indicator == 'TEST') %>% 
  filter(model_name == 'TEMP_NORMAL') %>%
  filter(outlier == FALSE) %>% # keep outliers out of modeling
  filter(climate_zone_cd %in% CLIMATE_ZONES) %>% # keep only climate zones of interest
  group_by(customer, prem_id, acct_id, opr_area_cd, climate_zone_cd, train_indicator) %>% 
  nest(.key = TEST) 

test <- test %>%
  mutate(
    .pred = map2(train$models, TEST, predict) # make out of sample predictions
  )

# Calculate model fit -----------------------------------------------------

# train %>% select(glance_train) %>% unnest()
# train %>% select(tidy_train) %>% unnest()
# train %>% select(models) %>% map(1)
# head(train)\1

# extract elements from train list in data frame d
train %>% select(tidy_train) %>% unnest()
train %>% select(glance_train) %>% unnest()

train_df <- train %>%
  select(customer, prem_id, acct_id, opr_area_cd, climate_zone_cd, train_indicator, 
         TRAIN, augment_train) %>%
  mutate(
    .pred = augment_train %>% map(".fitted"), #fitted values
    .resid = augment_train %>% map('.resid'),
    usg = TRAIN %>% map('usg'),
    usage_actual = TRAIN %>% map('usage_actual'), #usage values
    month_name = TRAIN %>% map('month_name'),
    year = TRAIN %>% map('year'),  #date values
    year_day = TRAIN %>% map('year_day'), #date values
    temp_normal = TRAIN %>% map('temp_normal'), #date values
    temp_actual = TRAIN %>% map('temp_actual'), #date values
    model_name = TRAIN %>% map('model_name'),
    month_day = TRAIN %>% map('month_day'),
    month = TRAIN %>% map('month'),
    date = TRAIN %>% map('date'),
    year = TRAIN %>% map('year'),  #date values
    season = TRAIN %>% map('season'), 
    sa_id = TRAIN %>% map('sa_id') #usage values
    # opr_area_cd = TRAIN %>% map('opr_area_cd'), #usage values
    # season = TRAIN %>% map('season'), #usage values
    # train_indicator = 'TRAIN'
    ) %>% 
  select(-c(TRAIN, augment_train)) %>% 
  unnest() 

train_summary <- train_df %>% 
  group_by(customer, model_name, climate_zone_cd, month_name, train_indicator) %>%
  summarise(y = sum(usage_actual)
            , y_hat = sum(.pred)
            , resid = sum(.resid)) %>% 
  group_by(model_name, climate_zone_cd, month_name, train_indicator) %>% 
  summarize(rmse = rmse(y_hat = y_hat, y = y), n = n()) %>% 
  gather(key = variable, value = model_value, c(rmse)) %>% 
  ungroup() %>% 
  as.data.frame()

test_df <- test %>%
  select(customer, prem_id, acct_id, opr_area_cd, climate_zone_cd, train_indicator, TEST, .pred) %>%
  mutate(
    usage_actual = TEST %>% map('usage_actual'), #usage values
    usage_prior = TEST %>% map('usage_prior'), #usage values
    model_name = TEST %>% map('model_name'),
    usg = TEST %>% map('usg'),
    year_day = TEST %>% map('year_day'), #date values
    temp_normal = TEST %>% map('temp_normal'), #date values
    temp_actual = TEST %>% map('temp_actual'), #date values
    month_name = TEST %>% map('month_name'),
    month_day = TEST %>% map('month_day'),
    month = TEST %>% map('month'),
    date = TEST %>% map('date'),
    year = TEST %>% map('year'),  #date values
    season = TEST %>% map('season'), 
    sa_id = TEST %>% map('sa_id')
    # climate_zone_cd = TEST %>% map('climate_zone_cd'), #usage values
    # opr_area_cd = TEST %>% map('opr_area_cd'), #usage values
    # train_indicator = 'TEST'
    ) %>% 
  select(-c(TEST)) %>% 
  unnest() %>% 
  mutate(.resid = usage_actual - .pred) 

test_summary <-  test_df %>%  
  group_by(customer, model_name, climate_zone_cd, month_name, train_indicator) %>%
  summarise(y = sum(usage_actual)
            , y_hat = sum(.pred)
            , resid = sum(.resid)) %>% 
  group_by(model_name, climate_zone_cd, month_name, train_indicator) %>% 
  summarize(rmse = rmse(y_hat = y_hat, y = y), n = n()) %>% 
  gather(key = variable, value = model_value, c(rmse)) %>% 
  ungroup() %>% 
  as.data.frame()

baseline <- baseline_usage %>%
  # rename(.pred = pred) %>% 
  filter(outlier == FALSE) %>% # keep outliers out of modeling
  filter(climate_zone_cd %in% CLIMATE_ZONES) %>% # keep only climate zones of interest
  filter(customer %in% (random_customers$customer)) %>%  
  mutate(.resid = usage_actual - .pred) %>% 
  tbl_df()

baseline_summary <- baseline %>% 
  group_by(customer, model_name, climate_zone_cd, month_name, train_indicator) %>%
  summarise(y = sum(usage_actual)
            , y_hat = sum(.pred)
            , resid = sum(.resid)) %>% 
  group_by(model_name, climate_zone_cd, month_name, train_indicator) %>% 
  summarize(rmse = rmse(y_hat = y_hat, y = y), n = n()) %>% 
  gather(key = variable, value = model_value, c(rmse)) %>% 
  ungroup() %>% 
  as.data.frame()

# combine in sample, out of sample and baseline model details
model_comp <- rbind(test_summary, train_summary, baseline_summary) %>% 
  mutate(model_name = paste0(model_name,'_',train_indicator)) %>% 
  arrange(desc(month_name), model_name) %>% 
  left_join(baseline_summary %>% select(baseline_model = model_name, baseline = model_value, climate_zone_cd, month_name)
            , by = c('climate_zone_cd','month_name'))

model_comp <- model_lift(model_comp, baseline = baseline, model_value = model_value )

model_comp <- model_comp %>%
  select(model_name, climate_zone_cd, n, month_name, train_indicator, variable, model_value, baseline:model_lift) %>% 
  filter(model_name %in% c('TEMP_NORMAL_TEST','CONS_USG_BASELINE'))


# Plotting model fit ------------------------------------------------------
# format dates for plotting
model_comp$model_name <- as.factor(model_comp$model_name)

model_comp$month_name <- factor(model_comp$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

# summary for plots
plot_details <- model_comp %>%
  filter(train_indicator == 'TEST') %>% 
  select(climate_zone_cd, n) %>% 
  group_by(climate_zone_cd, n) %>% 
  distinct() %>% 
  ungroup %>% 
  mutate(total = sum(n))

# how do the models compare to baseline on average during the winter?
rmse_comparision <- ggplot(data = model_comp %>%
                             filter(train_indicator!='TRAIN' & model_name %in% 
                                      c('CONS_USG_BASELINE','TEMP_NORMAL_TEST')) %>% 
         group_by(model_name, climate_zone_cd, month_name, train_indicator) %>% 
         summarize(average_model_pct_change = round(mean(model_pct,2)),
                   average_model_num_change = round(mean(model_delta),2),
                   average_model_value = round(mean(model_value),2),
                   average_baseline_num = mean(baseline)), aes(x = month_name, y = average_model_value, fill = model_name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~climate_zone_cd, ncol = 1) +
  geom_text(aes(label = average_model_value), position = position_dodge(1)) +
  scale_fill_discrete("Comparing Accuracy \nAcross Model Types"
                      ,label = c('Constant Usage'
                                 , 'Temperature Model'))+
  ggtitle(paste("Comparing model RMSE for gas usage for",unique(plot_details$total),'customers in'
                ,'T&W')) +
  labs(y = 'RMSE \n(lower is better)', x = NULL)

ggsave(plot = rmse_comparision, filename = 'model_rmse_comparison.png'
       , path = 'images/', width = 11, height = 8.5)

# percent improvement of temperature compared to baseline (constant usage) 
rmse_pct <- ggplot(model_comp %>% filter(model_name %in% c('TEMP_NORMAL_TEST')), 
       aes(x = month_name, y = model_pct, fill = model_name)) +
geom_bar(stat = 'identity', aes(y=model_pct, ymin=0, ymax=model_pct), position = 'dodge')+
geom_text(aes(x = month_name, y = model_pct, ymax = model_pct, label = model_pct,
vjust = ifelse(sign(model_pct)>0, 1, -1)), position = position_dodge(width = 1))+
facet_wrap(~climate_zone_cd, ncol = 1)+
scale_fill_discrete("Model Type",
                    labels = 'Temperature Model') +
ggtitle("Comparing percent improvement in RMSE  for\nnormal temperature over Constant Usage") +
labs(y = 'percent improvement (+/- over baseline)', x = NULL)

ggsave(plot = rmse_pct, filename = 'model_rmse_pct_improvement.png', path = 'images/', width = 11, height = 8.5)

# Plotting HDD comparisons ---------------------------------------------------------

# comparing actual and normal HDD 65 for train and test periods
hdd_dt$month_name <- factor(hdd_dt$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

hdd_comp <- ggplot(hdd_dt %>% filter(hdd_type != 'hdd_norm_2014'))+
  geom_bar(aes(x =month_name, y = hdd_65, fill = hdd_type), stat = 'identity'
           , position = 'dodge') +
  facet_wrap(~climate_zone_cd, ncol = 1) +
scale_fill_discrete(name="Heating Degree Days"
                    ,labels=c('2014 HDD-65 Actual',
                              '2015 HDD-65 Actual',
                              '2015 HDD-65 Normal')) +
  labs(y = "Heating Degree Days", x = NULL)+
  ggtitle("Comparing Actual and Normal HDD-65")

ggsave(plot = hdd_comp, filename = 'hdd_comparison.png', path = 'images/', width = 11, height = 8.5)

# Export files ------------------------------------------------------------
gc()

# need to map this
test_out <- test_df %>%
  select(-usage_prior) %>% 
  filter(model_name == 'TEMP_NORMAL') %>% 
  select(train_indicator, customer, prem_id, acct_id, sa_id, opr_area_cd, climate_zone_cd, .pred, .resid
         , usage_actual,model_name, usg, year_day, month_name, month_day, month, date, year, season)

train_out <- train_df %>%
  filter(model_name == 'TEMP_NORMAL') %>% 
  select(train_indicator, customer, prem_id, acct_id, sa_id, opr_area_cd, climate_zone_cd, .pred, .resid
         , usage_actual, model_name, usg, year_day, month_name, month_day, month, date, year, season)

baseline_out <- baseline %>%
  mutate(usg = usage_actual) %>% 
  select(train_indicator, customer, prem_id, acct_id, sa_id, opr_area_cd, climate_zone_cd, .pred, .resid
         , usage_actual,model_name, usg, year_day, month_name, month_day, month, date, year, season)

# need to walk this
write.table(x = test_out, file = '../CEA_DATA/model_predictions/test.txt'
            , col.names = TRUE, row.names = FALSE)

write.table(x = train_out, file = '../CEA_DATA/model_predictions/train.txt'
            , col.names = TRUE, row.names = FALSE)

write.table(x = baseline_out, file = '../CEA_DATA/model_predictions/baseline.txt'
            , col.names = TRUE, row.names = FALSE)

# 
# rm(train_test)
# rm(test)
# rm(train)
# rm(baseline_usage)
