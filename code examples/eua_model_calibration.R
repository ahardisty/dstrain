# IN_FILE <- 'E:/CEA_DATA/model_export/shifted_tou_predictions/'
# IN_FILE_LIST_MONTHLY <- dir('E:/CEA_DATA/model_export/shifted_tou_predictions/', pattern = 'monthly', full.names = TRUE)
OUT_FILE <- 'E:/CEA_DATA/model_export/shifted_tou_predictions/'
IN_FILE_LIST_DAILY <- dir('E:/CEA_DATA/model_export/shifted_tou_predictions/', pattern = 'daily', full.names = TRUE)
SAMPLE_SIZE <- 10
SAMPLE_NUMS <- grep(pattern = SAMPLE_SIZE, x = IN_FILE_LIST_DAILY)

billing_input_compare_list <- map(.x = IN_FILE_LIST_DAILY[SAMPLE_NUMS], .f = fread, header = TRUE, sep = 'auto'
                     , check.names = TRUE
                     , stringsAsFactors = TRUE
                     , integer64 = 'character'
                     , select = SELECT_COLS
                     , verbose = FALSE)


# billing_input_compare[,train_indicator:= ifelse(year == TRAIN_YEAR, 'train'
#                                         , ifelse(year == TEST_YEAR & measure_type == 'CONS_USG'
#                                                  , 'CONS_USG','test'))]


#   billing_input_daily <- map(.x = IN_FILE_LIST_DAILY[[2]], .f = fread, header = TRUE, sep = 'auto'
#              , check.names = TRUE
#              , stringsAsFactors = TRUE
#              , integer64 = 'character'
#              # , select = SELECT_COLS
#              , verbose = FALSE) %>% rbindlist()



# what is the scenario performance over baseline --------------------------

# load predictions
billing_input_compare <- list(
  monthly_train = billing_input_compare_list[grep(pattern = 'train'
                                                  , x = IN_FILE_LIST_DAILY[SAMPLE_NUMS])] %>% rbindlist(),
  monthly_test = billing_input_compare_list[grep(pattern = 'test', 
                                                 x = IN_FILE_LIST_DAILY[SAMPLE_NUMS])] %>% rbindlist(),
  monthly_baseline = billing_input_compare_list[grep(pattern = 'baseline'
                                                     , x = IN_FILE_LIST_DAILY[SAMPLE_NUMS])]%>% 
    rbindlist() %>% setcolorder(SELECT_COLS)
)
# create test and baseline
monthly_test <- billing_input_compare$monthly_test
monthly_baseline <- billing_input_compare$monthly_baseline



# monthly quality ---------------------------------------------------------


monthly_test_summary <- copy(monthly_test)
# monthly_test_summary[,error:=sqrt(sum((usage - predicted)^2))]
monthly_test_summary <- monthly_test_summary[,.(actual = sum(usage), predicted = sum(predicted),.N),
                                             by = .(month, year, month_name, model_name
                                                    , measure_type, climate_zone_cd, nrml_yyyymm),]
monthly_test_summary[,rmse := sqrt((actual - predicted)^2/N)
                     , by = .(month, year, month_name, model_name
                              , measure_type, climate_zone_cd, nrml_yyyymm)]

monthly_baseline_summary <- copy(monthly_baseline)
monthly_baseline_summary <- monthly_baseline_summary[,.(actual = sum(usage), predicted = sum(predicted),.N),
                                                     by = .(month, year, month_name, model_name
                                                            , measure_type, climate_zone_cd, nrml_yyyymm),]

monthly_baseline_summary[,rmse := sqrt((actual - predicted)^2/N)
                         , by = .(month, year, month_name, model_name
                                  , measure_type, climate_zone_cd, nrml_yyyymm)]


# annual comparisons ------------------------------------------------------
# baseline
annual_baseline_summary <- copy(monthly_baseline)[,.(actual = sum(usage), predicted = sum(predicted),.N),
                                                     by = .(year
                                                            , model_name
                                                            , measure_type
                                                            , climate_zone_cd),]

annual_baseline_summary[,rmse := sqrt((actual - predicted)^2/N)
                        , by = .(year
                                 , model_name
                                 , measure_type
                                 , climate_zone_cd)]

# model
annual_test_summary <- copy(monthly_test)[,.(actual = sum(usage), predicted = sum(predicted),.N),
                                                     by = .(year
                                                            , model_name
                                                            , measure_type
                                                            , climate_zone_cd),]
annual_test_summary[,rmse := sqrt((actual - predicted)^2/N)
                         , by = .(year
                                  , model_name
                                  , measure_type
                                  , climate_zone_cd)]

# write annual_test_summary to a file
# write annual_baseline_summary to a file



# MERGE FOR COMPARISIONS --------------------------------------------------
# monthly comparision
setkey(monthly_test_summary, month, month_name, year, climate_zone_cd, nrml_yyyymm) 
setkey(monthly_baseline_summary, month, month_name, month_name, year, climate_zone_cd, nrml_yyyymm) 

# monthly_model_comp <- merge(monthly_test_summary, monthly_baseline_summary, suffixes = c(x = '_model', y='_baseline'))
# model_comp_monthly <- model_comp_monthly[, improvement :=  (rmse_baseline - rmse_model)/(rmse_baseline)
#            ][,.(month, year, climate_zone_cd, nrml_yyyymm, month_name
#                 , measure_type_model, rmse_model, rmse_baseline, improvement)]
# model_comp_monthly[, model_pct:= ifelse(improvement == 0, 'even', ifelse(improvement <0, 'worse', 'better')) ]
# model_comp_monthly <- model_comp_monthly[, improvement := round(improvement,2)]
# 


## annual comparisions
setkey(annual_test_summary, year, climate_zone_cd) 
setkey(annual_baseline_summary, year, climate_zone_cd) 

annual_model_comp <- merge(annual_test_summary, annual_baseline_summary, suffixes = c(x = '_model', y='_baseline'))

annual_model_comp <- annual_model_comp[, improvement :=  (rmse_baseline - rmse_model)/(rmse_baseline)
           ][,.(climate_zone_cd, measure_type_model, rmse_model, rmse_baseline, improvement)]

annual_model_comp[, model_pct:= ifelse(improvement == 0, 'even', ifelse(improvement <0, 'worse', 'better')) ]

annual_model_comp[, improvement := round(improvement,2)]
# annual_model_comp[,improvement:=abs(improvement)]
annual_model_comp <- subset(annual_model_comp, model_pct == 'better') # only for better
annual_model_comp[,rank:=min_rank(improvement),by = .(climate_zone_cd)] # rank better
annual_model_comp[rank %in% c(1,2),] # first and second for each climate_zone_cd


# what does shift tou do for baseline? ------------------------------------

levels(hourly_predictions$month_name) <- factor(hourly_predictions$month_name, levels = month.abb)
levels(hourly_predictions$day_name) <- c('Sun','Mon','Tues','Wed','Thur','Fri','Sat')

hourly_predictions_sm <- hourly_predictions[season == 'summer',]

hourly_prediction_list <- hourly_predictions_sm %>%  
  split(list(.$climate_zone_cd, .$month_name), drop = TRUE)

DAILY_BASELINE_PLOT <- hourly_prediction_list %>% 
map(~ggplot(.,aes(x = day_name,  y = resid, fill = model_name)) +
      facet_wrap( ~ month_name) +
      geom_boxplot() +
      facet_wrap(~ opr_area_cd) +
      labs(x = NULL, y = 'residual') +
      ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone'
                    ,.$climate_zone_cd,'in',.$month_name))
    )

DAILY_BASELINE_PATH <- paste0('images/cons_usg_tou_shift_improvements_',tolower(names(hourly_prediction_list)),'.png')
walk2(DAILY_BASELINE_PATH, DAILY_BASELINE_PLOT, ggsave, width = 11, height = 8.5) # works


MONTH_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = model_name)) +
        facet_wrap( ~ month_name) +
        geom_boxplot() +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd,'by month')))


# how does baseline compare to temp ----------------------------------------
sample_customer <- as.character(model_constraint[,unique(customer)])
setkey(baseline_tou, model_name, customer)
baseline_tou <- baseline_tou[.('TOU_shift', sample_customer)]
baseline_tou[,nrml_yyyymm:=as.character(sprintf('%s%02d', year, month))]

baseline_tou[date == '2015-01-01' & climate_zone_cd == 'W',]

baseline_performance <-  baseline_tou[model_name == 'TOU_shift', .(usage = sum(usage), predicted = sum(predicted)) # include mean temp for measure_type
                                     , by = c(MONTHLY_BILLING_COLS),][order(month)][,comp_type := 'CONS_USAGE']
                                                                                    ]
baseline_performance_monthly <- baseline_performance[,.(error = rmse(usage, predicted))
                                                     , by = .(climate_zone_cd, month_name, month, model_name, measure_type, comp_type)]

baseline_performance_monthly[month_name == 'Dec' & climate_zone_cd == 'W',]
test_tou[month_name == 'Dec' & climate_zone_cd == 'W',]

str(test_tou)
colnames(test_tou)

setkey(baseline_performance_monthly, climate_zone_cd, month_name, month, model_name)
setkey(model_performance, climate_zone_cd, month_name, month, model_name)

model_comp <- merge(model_performance, baseline_performance, suffixes = c(x = '_model',y = '_baseline'))
model_comp[, improvement :=  (error_baseline - error_model)/(error_baseline)]
model_comp[, model_pct:= ifelse(improvement == 0, 'even', ifelse(improvement <0, 'worse', 'better')) ]
model_comp[, improvement :=  round(improvement, 2)]
model_comp[, rank :=  rank(improvement, 2)]
model_comp[,rank:=factor(min_rank(improvement)),by = .(climate_zone_cd, month_name, month)]


# model_comp[month_name == 'Aug',]
# COMP_BASELINE <- ggplot(data = model_comp[, lapply(.SD, function(x) round(mean(x),2)), .SDcols = 'improvement'
#                          , by = .(climate_zone_cd, month_name, model_pct)],aes(x = month_name, y = improvement, fill = model_pct)) +
#   geom_bar(stat = 'identity', position = 'identity') +
#   scale_fill_manual(values = c(better = CP$blue,  even = CP$green, worse = CP$red),
#                     'Model Performance',
#                     labels = c('Temp better than constant'
#                                ,'Temp even to constant'
#                                , 'Temp worse than constant')) +
#   geom_text(aes(x = month_name, y = improvement, label = improvement)) +
#   facet_wrap(~climate_zone_cd, ncol = 1) +
#   ggtitle('Montly Temperature Model Performance compared to Constant Usage')
# 
# ggsave(filename = 'images/tou_temperature_improvements.png',COMP_BASELINE, width = 11, height = 8.5)
# 

MODEL_LIST <- model_comp %>%  
  split(list(.$climate_zone_cd), drop = TRUE)

# map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%

ggplot(w_aug , aes(x = measure_type_model, y = improvement, fill = model_pct)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(better = CP$blue, worse = CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               , 'Temp worse than constant'))
  geom_text(aes(x = measure_type_model, y = improvement, label = improvement))

MONTH_PLOT_FACET <- MODEL_LIST %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = measure_type_model,  y = improvement, fill = model_pct)) +
        facet_wrap( ~ month_name) +
        geom_bar(stat = 'identity', position = 'dodge') +
        scale_fill_manual(values = c(better = CP$blue,  even = CP$green, worse = CP$red),
                          'Model Performance',
                          labels = c('Temp better than constant'
                                     ,'Temp even to constant'
                                     , 'Temp worse than constant')) +
        labs(x = 'temp used for prediction', y = 'percent improvement over baseline') +
        coord_flip() +
        ggtitle(paste('Improvment Percentage over baseline by temp type',.$climate_zone_cd,'by month')))

MONTH_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_month_',tolower(names(MODEL_LIST)),'.png')
walk2(MONTH_PLOT_FACET_PATH, MONTH_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

TEMP_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        # facet_grid(model_name ~ month_name) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by temperature type')))
TEMP_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_temp_type_'
                               ,tolower(names(model_list)),'.png')
walk2(TEMP_PLOT_FACET_PATH, TEMP_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ season, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by season')))
SEASON_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_season_'
                                 ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET_MONTH <- model_list %>%
  map(. %>% filter((month_name %in% c('Dec','Aug','May','Mar')))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ month_name, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by season and month')))
SEASON_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_season_month_'
                                 ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET_MONTH, ggsave, width = 11, height = 8.5) # works


# print annual_model_comp to file -----------------------------------------


# plot annual_model_comp --------------------------------------------------
model_long <- annual_model_comp %>% 
  filter(rank <=2) %>% 
  gather(key = key, value = value, c(rmse_baseline, rmse_model))%>% 
  mutate(value = round(value,1))


ggplot(data = model_long, aes(x = measure_type_model, y = value, fill = key))+
  geom_bar(aes(x = measure_type_model), stat = 'identity', position = 'dodge') +
  facet_grid(climate_zone_cd~.) +
  coord_flip() +
  geom_text(aes(label = value), stat = 'identity', position = position_dodge(widt = 1))+
  scale_fill_discrete('Temp Scenario Type\nimprovement over CONS usg') +
  labs(x = 'top 2 scenarios'
       , y = 'Temp Scenario pct better than CONS usg') +
  ggtitle(paste('Best Moddel Selectio for based on pct improvement\nover CONS_usg'))
                





# resid plots -------------------------------------------------------------


plot <- ggplot(data = baseline_daily, aes(x = day_name, y = resid)) +
  geom_boxplot(aes(fill = model_name)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~climate_zone_cd, ncol = 1) +
  ggtitle('Residual Range in Constant Usage Model after TOU Period Shift') +
  labs(x = NULL, y = 'residual')
ggsave('images/tou_baseline_improvements.png', plot, width = 11, units = 'in')

plot <- ggplot(data = baseline_daily %>% filter(month_name %in% c('Aug','May','Dec')), aes(x = reorder(day_name, day), y = resid)) +
  geom_boxplot(aes(fill = model_name)) +
  facet_wrap(~month_name + climate_zone_cd) +
  geom_hline(yintercept = 0) +
  ggtitle('Residual Range in Constant Usage Model after TOU Period Shift') +
  labs(x = NULL, y = 'residual')
ggsave('images/tou_baseline_improvements_aug.png', plot, width = 11, units = 'in')

# split model on facets for better plots

model_list <- model_daily %>%  
  split(list(.$climate_zone_cd), drop = TRUE)

MONTH_PLOT <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = model_name)) +
        # facet_wrap( ~ period) +
        geom_boxplot() +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd)))

MONTH_PLOT_PATH <- paste0('images/tou_temperature_improvements_',tolower(names(model_list)),'.png')
walk2(MONTH_PLOT_PATH, MONTH_PLOT, ggsave, width = 11, height = 8.5) # works

MONTH_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = model_name)) +
        facet_wrap( ~ month_name) +
        geom_boxplot() +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd,'by month')))

MONTH_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_month_',tolower(names(model_list)),'.png')
walk2(MONTH_PLOT_FACET_PATH, MONTH_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

TEMP_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        # facet_grid(model_name ~ month_name) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by temperature type')))
TEMP_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_temp_type_'
                               ,tolower(names(model_list)),'.png')
walk2(TEMP_PLOT_FACET_PATH, TEMP_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET <- model_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ season, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by season')))
SEASON_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_season_'
                               ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET_MONTH <- model_list %>%
  map(. %>% filter((month_name %in% c('Dec','Aug','May','Mar')))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ month_name, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by season and month')))
SEASON_PLOT_FACET_PATH <- paste0('images/tou_temperature_improvements_by_season_month_'
                               ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET_MONTH, ggsave, width = 11, height = 8.5) # works

    

# final decision - use TOU shift for comparision but the real differences are at the temperature type



# combine baseline and model -----------------------------------------------------
model_comp_list <- model_comp %>%
  filter(model_name == 'TOU_shift')%>% # only using TOU_shift
  split(list(.$climate_zone_cd), drop = TRUE)
  
MONTH_PLOT <- model_comp_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        # facet_wrap( ~ period) +
        geom_boxplot() +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range for temp types in Climate Zone',.$climate_zone_cd)))

MONTH_PLOT_PATH <- paste0('images/temp_baseline_comparison_',tolower(names(model_list)),'.png')
walk2(MONTH_PLOT_PATH, MONTH_PLOT, ggsave, width = 11, height = 8.5) # works

MONTH_PLOT_FACET <- model_comp_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ month_name) +
        geom_boxplot() +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range for temp types in Climate Zone',.$climate_zone_cd,'by month')))

MONTH_PLOT_FACET_PATH <- paste0('images/temp_baseline_comparison_by_month_',tolower(names(model_list)),'.png')
walk2(MONTH_PLOT_FACET_PATH, MONTH_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET <- model_comp_list %>%
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ season, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range for temp types in Climate Zone',.$climate_zone_cd
                      ,'by season')))
SEASON_PLOT_FACET_PATH <- paste0('images/temp_baseline_comparison_by_season_'
                                 ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET, ggsave, width = 11, height = 8.5) # works

SEASON_PLOT_FACET_MONTH <- model_list %>%
  map(. %>% filter((month_name %in% c('Dec','Aug','May','Mar')))) %>%
  map(~ggplot(.,aes(x = day_name,  y = resid, fill = measure_type)) +
        facet_wrap( ~ month_name, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual') +
        ggtitle(paste('Daily Residual Range in Temp Model for Climate Zone',.$climate_zone_cd
                      ,'by season and month')))

SEASON_PLOT_FACET_PATH <- paste0('images/temp_baseline_comparison_by_season_month_'
                                 ,tolower(names(model_list)),'.png')
walk2(SEASON_PLOT_FACET_PATH, SEASON_PLOT_FACET_MONTH, ggsave, width = 11, height = 8.5) # works

PREDICT_PLOT <- model_list %>%
  map(. %>% filter((month_name %in% c('Dec','Aug','May','Mar')))) %>%
  map(~ggplot(.,aes(x = day_name,  y = predicted, fill = measure_type)) +
        facet_wrap( ~ month_name, ncol = 1) +
        geom_boxplot() +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = 'residual'))


# Model Error -------------------------------------------------------------
baseline_monthly <- monthly_predictions[[1]] %>% mutate(comp_type = 'CONS_USG')
model_monthly <- monthly_predictions[[2]] %>% mutate(comp_type = 'TEMPERATURE')

baseline_monthly[, nrml_yyyymm := as.character(sprintf('%s%02d', year, month))]
baseline_monthly[, day := wday(date)]

COMP_COLS <- c('model_name', 'month', 'month_name', 'season', 'period', 'year'
               , 'day_name', 'climate_zone_cd', 'usage', 'predicted', 'nrml_yyyymm')

baseline_monthly <- baseline_monthly[model_name == 'TOU_shift',COMP_COLS, with = FALSE]
model_monthly <- model_monthly[,COMP_COLS, with = FALSE]


metrics <- test_monthly[month == 'May' | month == 'Aug'
                        , .(rmse_new = rmse(actual_test, pred_norm)
                            , rmse_old = rmse(actual_test, pred_usage_lag)
                            , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                        , by = .(month, period)]


?summarise()
# multiple samples --------------------------------------------------------


w_files <- c(sample1 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'w.txt', full.names = TRUE),
sample2 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample2/', pattern = 'w.txt', full.names = TRUE),
sample3 <- dir('E:/CEA_DATA/interval_data/electric/kamal/sample3/', pattern = 'w.txt', full.names = TRUE)
)

w_files <- list(sample1 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample1/', pattern = 'w.txt', full.names = TRUE),
sample2 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample2/', pattern = 'w.txt', full.names = TRUE),
sample3 = dir('E:/CEA_DATA/interval_data/electric/kamal/sample3/', pattern = 'w.txt', full.names = TRUE)
)

tou_interval <- map(w_files, .f = fread, header = TRUE, sep = 'auto'
     , check.names = TRUE
     , stringsAsFactors = TRUE
     , integer64 = 'character'
     , verbose = FALSE) %>% rbindlist()


setkey(tou_interval, date)
tou_interval[,date := as.IDate(date)]

tou_interval <- add_time(tou_interval)

str(tou_interval)

tou_interval[month == 'Aug',.N, by = .(year, date, month, period)]

str(tou_interval)

# Model quality on customer level -----------------------------------------


test_monthly <- fread('CEA_DATA/interval_data/electricity/kamal/predictions_monthly_w.csv')
test_daily <- fread('CEA_DATA/interval_data/electricity/kamal/predictions_daily_w.csv')

monthly_metrics <- test_monthly[month == 'May' | month == 'Aug'
                        , .(rmse_new = rmse(actual_test, pred_norm)
                            , rmse_old = rmse(actual_test, pred_usage_lag)
                            , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                        , by = .(month, period)]

poor_period <- monthly_metrics[improvement <.1,][,unique(period)]

customer_metrics <- test_daily[month == 'Aug' 
                                 , .(rmse_new = rmse(actual_test, pred_norm)
                                     , rmse_old = rmse(actual_test, pred_usage_lag)
                                     , improvement = (rmse(actual_test, pred_usage_lag) - rmse(actual_test, pred_norm))/rmse(actual_test, pred_usage_lag))
                                 , by = .(customer, date, month, day)][order(date)
                                                                       ][,resid := round((rmse_new - rmse_old),2),]

# customer_metrics[,five_num:=.(list(fivenum(improvement)))]
# fivenum(customer_metrics$improvement)
# model calibration
summary(customer_metrics$resid)
customer_metrics[resid > -.009]
customer_metrics[resid < .009,]

customer_metrics[,over_under:= ifelse(resid > 0, 'worse'
                                      , ifelse(resid < 0, 'better'
                                               ,'even'))]

customer_metrics[,breaks:=cut(improvement, include.lowest = TRUE, right = TRUE
                              , breaks = unique(quantile(improvement
                                                         , probs = seq(0, 1,.05))))]


customer_metrics[, .N, by = .( breaks, over_under)]


# Comparing predictions at daily customer level ---------------------------

daily_plot <- ggplot(data = customer_metrics[,.N, by = .(day, over_under, month)][order(over_under)]
       , aes(x = factor(day), y = N, fill = over_under)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~month ) +
  scale_fill_manual(values = c(better = CP$blue, even = CP$green, worse = CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               , 'Temp even with constant'
                               , 'Temp worse than constant')) +
  geom_hline(data = melt(customer_metrics[,.(sample75 = uniqueN(customer)*.75
                                             , sample50 = uniqueN(customer)*.50
                                             , sample25 = uniqueN(customer)*.25), by = .(month)
                                          ],id.vars = 'month',variable.name = 'line',value.name = 'N')
             ,aes(yintercept = N, linetype = line)
             , color = 'black', size = 1.5) +
  scale_linetype_discrete(name = 'Expected Breaks'
                          , labels = c('.75 of sample'
                                       , '.50 of sample'
                                       , '.25 of sample')) +
  labs(x = NULL, y = '# of Customers per day') +
  ggtitle('Count and Quality of daily predictions in W compared to baseline model')


# comparing the weight of predictions at daily level ----------------------
daily_average <- ggplot(data = customer_metrics[,lapply(.SD, function(x) round(mean(x),2)), by = .(day, over_under, month)
                               , .SDcols = 'improvement'][order(over_under)]
       , aes(x = factor(day), y = improvement, fill = over_under)) +
  geom_bar(stat = 'identity', position = 'identity') +
  facet_wrap(~month ) +
  scale_fill_manual(values = c(CP$blue, CP$green, CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               , 'Temp even with constant'
                               , 'Temp worse  than constant')) +
  labs(x = NULL, y = '% improvement over baseline on daily level') +
  geom_text(aes(label = improvement)) +
  ggtitle('Average improvement for daily predictions in W compared to baseline model')



# Distribution of actual values -------------------------------------------

breaks <- levels(unique(customer_metrics$breaks))
break_plot <- c(1, 2, 10, 15, 20)
bad_days <- c(2,9,16,10)
select_breaks <- breaks[break_plot]

customer_metrics[breaks %in% c(select_breaks) ,.N, by = .(breaks)]

imp_disribution_plot <- ggplot(data = customer_metrics[day %in% c(bad_days) 
                                                       & breaks %in% c(select_breaks)
                                                       & improvement > -1,][order(-over_under)]
       ,aes(x = factor(day), y = improvement)) +
  # facet_wrap(~over_under, nrow = 1, scales = 'free') +
         geom_boxplot(outlier.colour = 'red'
                      , notch = TRUE) +
  labs(x = "Selected days in August", y = 'range of % improvement over baseline on daily level') +
  ggtitle('Actual improvement for daily predictions in W compared to baseline model')



# customer_metrics[,`:=`(impr_min = lapply(five_num, `[`, 1), 
#                        impr_q1 = lapply(five_num, `[`, 2),
#                        impr_med = lapply(five_num, `[`, 3),
#                        impr_q3 = lapply(five_num, `[`, 4),
#                        impr_max = lapply(five_num, `[`, 5))]

# customer_metrics[,`:=` (min_floor = as.numeric(impr_min)-1
#                           , max_ceil = as.numeric(impr_max)+.0001)]


# customer_metrics[,bucket:= cut(improvement, include.lowest = TRUE
#                                , right = TRUE
#                                , breaks = unique(quantile(improvement, c(0, .2,.5,.75,.95,1))))]

ggplot(data = customer_metrics, aes(y = improvement, x = period)) +
  geom_boxplot()


eua_cross_ref <- fread('../CEA_DATA/demograhic_data/eua_cross_ref.txt',
                header = 'auto',
                sep='auto',
                na.strings = '?',
                integer64 = 'character',
                stringsAsFactors = TRUE,
                # colClasses = dem_classes,
                showProgress = FALSE )
eua_cross_ref[,customer:= paste0(prem_id,'_',acct_id)]
setkey(eua_cross_ref, prem_id)
setkey(eua_cross_ref, customer)

DT_Dem <- fread('../CEA_DATA/demograhic_data/cea_demographics.txt',
                header = 'auto',
                sep='auto',
                na.strings = '?',
                integer64 = 'character',
                stringsAsFactors = TRUE,
                # colClasses = dem_classes,
                showProgress = FALSE )
DT_Dem <- DT_Dem[TENUR_acct_yrs !=0,]
DT_Dem[,customer:= as.factor(paste0(prem_id,'_',PREMI_acct_id))]


DEMO <- '(customer|AGE|ANNUA|DWELL_prem|ENGAG|FUEL|HOME|NUMBE|PROGR|RATES|RESID|pty)'
MELT <- '(AGE|ANNUA|DWELL_prem|ENGAG|FUEL|HOME|NUMBE|PROGR|RATES|RESID)'

DEMO_VARS <- grep(pattern = DEMO,x = names(DT_Dem))

num_cols <- which(sapply(DT_Dem,is.numeric))
int_cols <- which(sapply(DT_Dem,is.integer))
char_cols <- which(sapply(DT_Dem,is.character))

customer_metrics[,climate_zone_cd:='W']
# change to factors
customer_metrics[,c('customer','prem_id', 'month','period','over_under','climate_zone_cd','bucket'):=lapply(.SD, as.factor)
           , .SDcols = c('customer','prem_id', 'month','period','over_under','climate_zone_cd','bucket')]

setkey(DT_Dem, customer)
setkey(customer_metrics, customer)

customer_metrics_m <- merge(customer_metrics, DT_Dem[,DEMO_VARS, with = FALSE]
                            , suffixes = c(x = '_metrics', y ='_cross'))

str(customer_metrics_m)
# MELT_VARS <- grep(pattern = MELT,x = names(customer_metrics_m))
ID_VARS <- c(colnames(customer_metrics))
MEAS_COLS <- c('rmse_new','rmse_old','improvement')

id_cols <- which(sapply(customer_metrics,is.factor))
pty_id <- which(colnames(customer_metrics_m)=='pty_id')
id_cols <- c(id_cols, pty_id)
meas_cols <- which(sapply(customer_metrics,is.numeric))

factor_cols <- which(sapply(customer_metrics_m,is.factor))
num_cols <- which(sapply(customer_metrics_m,is.numeric))
int_cols <- which(sapply(customer_metrics_m,is.integer))

num_cols <- num_cols[!num_cols %in% meas_cols]
num_cols <- num_cols[!num_cols %in% int_cols]
int_cols <- int_cols[!int_cols %in% num_cols]
factor_cols <- factor_cols[!factor_cols %in% id_cols]

customer_metrics_factor <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(factor_cols), variable.name = 'measure', value.name = 'value')


customer_metrics_integer <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(int_cols), variable.name = 'measure', value.name = 'value'
                                      , na.rm = TRUE)


customer_metrics_num <- melt.data.table(customer_metrics_m
                                           , id.vars = c(id_cols, meas_cols)
                                      , measure.vars = c(num_cols), variable.name = 'measure', value.name = 'value'
                                      )
customer_metrics_num[,.N, by = .(measure)]
customer_metrics_integer[,.N, by = .(measure)]
customer_metrics_factor[,.N, by = .(measure)]
str(customer_metrics_numeric$value)



# plots -------------------------------------------------------------------

factor_sub <- subset(customer_metrics_factor, measure == 'DWELL_prem_typ_cd' & over_under == 'worse')
customer_metrics_factor[,lapply(.SD, mean), .SDcols = 'improvement', by = .(measure, over_under, value)]

ggplot(data = factor_sub, aes(x = bucket, y = improvement)) +
  facet_wrap(~period) +
  geom_boxplot(aes(fill = value))

