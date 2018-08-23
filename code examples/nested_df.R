nested data frame

# using nested data frames ------------------------------------------------

# subset and fit
# gc()

rm(list = ls(pattern = '_tib'))
train <- usage_weather %>%
  filter(train_indicator == 'TRAIN') %>%
  group_by(prem_id) %>%
  nest() %>% 
  
  # %>% prem_train_tib <- prem_train_tib %>%
  mutate(models = map(data, usg_temp_lm)) %>% 
  
  # prem_train_tib <- prem_train_tib %>%
  mutate(tidy_train = map(models, broom::tidy), #broom
         glance_train = map(models, broom::glance), #broom
         augment_train = map(models, broom::augment),
         rsq_train = map_dbl(glance_train, 'r.squared'),
         rmse_train = map2(augment_train %>% map("usg")
                           , augment_train %>% map(".fitted"), rmse))

# create test set
test <- usage_weather %>%
  filter(train_indicator == 'TEST') %>%
  group_by(prem_id) %>%
  nest() %>% 
  
  # predict
  mutate(pred = map2(train$models, data, predict)) %>% 
  
  # Get the error
  mutate(rsq_test = map2(data %>% map("usg_actual"), pred, r_squared),
         rmse_test = map2(data %>% map("usg_actual"), pred, rmse),
         msd_test = map2(data %>% map("usg_actual"), pred, msd))

map2(train$data[1], 'usg_actual', 'usg', `-`)
yars <- map(train$data, 'year', summary)
yars <- map2(train$data, 'year','climate_zone_cd')
?map()

train(map2(train$data[1], 'usg_actual','usg', `-`)
      
      
      
      train %>% map(data %>% map('usg_actual'), sd)
      # create baseline comparisons
      
      train %>% 
        select(prem_id, data) %>% 
        # unnest(data) %>% 
        map(data %>% map('usg_actual'), sd) %>% 
        head()
      map(train$data[1], unnest)
      example(unnest)
      #   #
      #   # # Get the error
      #   rsq_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), r_squared),
      #   msd_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), msd),
      #   rmse_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), rmse))
      # 
      # 
      # 
      # str(prem_train_tib$y_hat[1])
      # str(prem_train_tib$y[1])
      # str(prem_train_tib$rmse[1])
      # str(prem_train_tib$rsq[1])
      # str(prem_train_tib$rs_check[1])
      # 
      # summary(prem_train_tib %>%
      #   select(rmse))
      # 
      # 
      # prem_test_tib <- prem_test_tib %>% 
      #   # filter(prem_id %in% c('0035459639','0058576447')) %>%
      #   mutate(rsq_out = map2(data %>% map("usg_actual"), pred, r_squared),
      #          msd_out = map2(pred, data %>% map("usg_actual"), msd), 
      #          rmse_out = map2(.x= data %>% map("usg_actual"), .y = pred, .f = rmse)) %>% 
      #   unnest(rsq_out, rmse_out) %>% 
      #   select(prem_id, pred, rmse_out, data, rsq_out)
      # 
      # missing_preds <- prem_test_tib_rn %>% unnest(data) %>% 
      #   select(date, usg, prem_id, temp, usg_actual, temp_actual) %>% 
      #   distinct(prem_id)
      # 
      # prem_test_tib_verify <- prem_test_tib %>%
      #   select(prem_id, data, pred, rsq_out) %>% 
      #   unnest(data, pred) %>% 
      #   select(prem_id, date, train_indicator, month_name, year, pred, usg, usg_actual, rsq_out) %>% 
      #   group_by(prem_id) %>% 
      #   summarise(rs_verify = r_squared(y = usg_actual, y_hat = pred),
      #             rsq_out = mean(rsq_out)) %>% 
      #   mutate(good_calc = (rs_verify == rsq_out)) 
      #   
      # summary(prem_test_tib_verify$good_calc)
      
      # nested model comparison -------------------------------------------------
      
      # plot model output -------------------------------------------------------
      
      # plot r.squared
      gc()
      # rm(list = ls(pattern = 'in_sample'))
      ggplot(data = in_sample_augment, aes(x = usg, y = .resid)) +
        # geom_jitter(aes(color = model_type, shape = train_details))
        geom_smooth(aes(color = train_indicator)) +
        facet_wrap(~model_type)
      
      gc()
      colnames(in_sample_augment)
      augment_long <- gather(data = in_sample_augment
                             , key = var, value = val
                             , c(usg, temp, .fitted, .resid, .hat
                                 , .sigma, .std.resid))
      
      ggplot(data = augment_long,
             aes(x = val)) +
        facet_wrap(~var, scales = 'free') +
        geom_histogram(aes(fill = train_indicator))
      
      # change model
      
      