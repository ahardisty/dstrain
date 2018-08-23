# model quality

library(readxl)
getwd()
electric_accuracy <- read_excel('../CEA_DATA/model_output/electricity_model_quality.xlsx',sheet = 1,)
gas_accuracy <- read.csv('../CEA_DATA/model_output/gas_model_quality.csv')

gas_accuracy <- gas_accuracy %>% filter(variable == 'av_score')

colnames(electric_accuracy)[6] <- gsub(pattern = ' ','_', colnames(electric_accuracy)[6])

electric_av_plot <- ggplot(electric_accuracy %>%
                                   rename(Temperature_Model = new_impact
                                          , Constant_Usage = old_impact
                                          , Temperature_Model_Lift = Percent_Change) %>% 
         gather(key = impact_measure, value = impact_value
                , -c(sample, deriv_baseline_terr_cd, month)) %>% 
         mutate(impact_value = round(impact_value,2)) 
       , aes(fill = impact_measure, x = deriv_baseline_terr_cd, y=impact_value)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(sample~month)+
  # aes(x = month_name, y = model_pct, ymax = model_pct, label = model_pct,
  geom_text(aes(ymax = impact_value, label = impact_value), hjust = .5, position = position_dodge(1))+
  scale_fill_discrete('TP(1-FP) Scores',
                      limits = c('Constant_Usage', 'Temperature_Model','Temperature_Model_Lift'),
                      labels = c('Constant Usage Model'
                                 , 'Temperature Model'
                                 , 'Percent Change')) +
  labs(y = 'TP(1-FP)', x = NULL) +
  ggtitle('Comparing TP(1-FP) between Constant Usage and Temperature Model')

gas_av_plot <- ggplot(gas_accuracy %>%
                              filter(model_index_model_value == 'TEMP_NORMAL_.pred') %>% 
                              select(-model_lift, -model_index_model_value, -model_index_baseline
                                     , -BILL_CYCLE) %>% 
         rename(Temperature_Model = model_value
                , Constant_Usage = baseline
                , Temperature_Model_Lift = model_pct
                , deriv_baseline_terr_cd = climate_zone_cd
                , month = month_name) %>% 
         gather(key = impact_measure, value = impact_value
                , -c(variable, deriv_baseline_terr_cd, model_delta, month)) %>% 
         mutate(impact_value = round(impact_value,2))
       , aes(fill = impact_measure, x = deriv_baseline_terr_cd, y=impact_value)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~month)+
  geom_text(aes(ymax = impact_value, label = impact_value), hjust = .5, position = position_dodge(1))+
  scale_fill_discrete('TP(1-FP) Scores',
                      limits = c('Constant_Usage', 'Temperature_Model','Temperature_Model_Lift'),
                      labels = c('Constant Usage Model'
                                 , 'Temperature Model'
                                 , 'Percent Change')) +
  labs(y = 'TP(1-FP)', x = NULL) +
  ggtitle('Comparing TP(1-FP) between Constant \nUsage and Temperature Model')    
  
