
# get data ----------------------------------------------------------------

library(purrr)  
library(dplyr)
library(haven)
library(data.table)

ba_cols <- col_types('../CEA_DATA/billing_data/input/', pat = '.pred', n = 5, sep ='\t')
billing_merge <- rbindlist(map(PATH[2:3], fread))



# NON SAS Bill calculations -------------------------------------------------------
# use long billing data for non SAS calculations

billing_merge[,care_ind := ifelse(CARE == 'N', '(Non-CARE)','(CARE)'),] # recreate CARE indicator to merge with billing
setkey(gas_rates, care_ind, BILL_CYCLE) # key for merge
setkey(billing_merge, care_ind, BILL_CYCLE) # key for merge

# merge tariffs and calculate bills
billing_merge <- merge(billing_merge, gas_rates, suffixes = c('billing','rates'))
billing_merge[,TR1_BILL:=TR1__*tier1,]
billing_merge[,TR2_BILL:=TR2__*tier2,]
billing_merge[,CURRENT_BILL := TR1_BILL+TR2_BILL]
billing_merge[CURRENT_BILL == 0, CURRENT_BILL := .0001]

setkey(billing_merge, customer, climate_zone_cd, BILL_CYCLE
       , train_indicator, model_name, month_name, usage_type, month_group)

billing_merge <- billing_merge[,lapply(.SD, sum)
                               , by = key(billing_merge)
                               , .SDcols = c('CURRENT_BILL','TR1__','TR2__')]

billing_list <- list( # create actual list of subset data tables
  billing_merge[train_indicator == 'TRAIN' & usage_type == 'usage_actual',],
  billing_merge[train_indicator == 'TEST' & usage_type == 'usage_actual',],
  billing_merge[train_indicator == 'TEST' & usage_type == '.pred',],
  billing_merge[train_indicator == 'BASELINE'& usage_type == '.pred',]
)


billing_names <- c('TRAIN_ACTUAL','TEST_ACTUAL','TEST_PREDICTED','CONSTANT_PREDICTED')
names(billing_list) <- billing_names


bill_impact <- rbindlist(billing_list)
setkey(bill_impact, customer, month_name)

bill_impact[, PRIOR_BILL := lapply(.SD, head, 1), by = .(customer, month_name)
            , .SDcols = 'CURRENT_BILL']

# Calculate the percent differences
bill_impact <- bill_impact[train_indicator != 'TRAIN',] # remove 2014
bill_impact[, diff:= CURRENT_BILL - PRIOR_BILL] # calculate bill difference
bill_impact[, diff_pct:= (CURRENT_BILL - PRIOR_BILL)/PRIOR_BILL][order(-diff_pct)]



# SAS billing calculations ------------------------------------------------
getwd()
billing_input <- list.files('../CEA_DATA/billing_data/output/', full.names = TRUE)
billing_input <- rbindlist(map(.x = billing_input, read_sas))
billing_input <- rbindlist(billing_input)
?read_sas()
str(billing_input)
billing_input <- copy(billing_input[SA_ID == '9935171594', ])


dig <- sprintf('%10i', billing_input$PREM_ID) # leading 0 for id numbers
gsub(pattern = ' ', replacement = 0, dig)

nchar('0008117489')

# discrete values for impact ----------------------------------------------

bill_impact[,diff_break := as.factor(bucket(x = diff, type = 'absolute'))]
bill_impact[,diff_pct_break := as.factor(bucket(x = diff_pct, type = 'percent'))]
bill_impact[is.na(diff_break), diff_break := as.factor('<$10'), ]
bill_impact[is.na(diff_pct_break), diff_pct_break := as.factor('<$10'), ]

bill_impact[,uniqueN(customer)]

# Calculate the high impacts 

bill_impact[, impact:= as.factor(get_impact(diff_pct, diff))]

# compare predicted and actual (baseline) impacts
model_predictions <- melt.data.table(bill_impact
                                     , measure.vars = c('diff_break','diff_pct_break','impact')
                                     , value.name = 'prediction'
                                     , variable.factor = TRUE
                                     , value.factor = TRUE)

model_baseline <- melt.data.table(subset(bill_impact, train_indicator == 'TEST' & usage_type == 'usage_actual'
)[,.(customer, month_name, BILL_CYCLE, CURRENT_BILL, diff_break, diff_pct_break, impact, usage_type)]
, id.vars = c('customer','BILL_CYCLE','month_name','CURRENT_BILL')
, measure.vars = c('diff_break','diff_pct_break','impact')
, value.name = 'actual'
, variable.factor = TRUE
, value.factor = TRUE)

setkey(model_predictions, customer, BILL_CYCLE, month_name, variable) # set key for merging
setkey(model_baseline, customer, BILL_CYCLE, month_name, variable) # set key for merging

bill_merge <- merge(model_predictions, model_baseline, suffixes = c('_prediction','_actual'))[,model_index:=paste0(model_name,'_',usage_type)]

HIGH_LEVELS <- bill_merge[,lapply(.SD, tail, 1), by = .(variable), .SDcols = 'actual', ][,(actual)]
LOW_LEVELS <- bill_merge[,lapply(.SD, head, 1), by = .(variable), .SDcols = 'actual', ][,(actual)]
ALL_LEVELS <- bill_merge[,(unique(actual))]
OTHER_LEVELS <- bill_merge[!actual %in% HIGH_LEVELS][,(unique(actual))]
HIGH_MID <- bill_merge[!actual %in% c(as.character(LOW_LEVELS)),][,(unique(actual))]

bill_merge[,c('LEVEL_actual','LEVEL_pred') := lapply(.SD, function(x) match(x, ALL_LEVELS))
           ,.SDcols = c('actual','prediction'),] # assign numeric value to factor levels

# create model lift comparison summary
model_predictions_summary <- bill_merge[model_index%in% c('TEMP_NORMAL_.pred','CONS_USG_.pred') 
                                        & variable == 'impact',.(accuracy = accuracy(actual = actual, prediction = prediction),
                                                                 precision = precision(actual = actual, prediction = prediction),
                                                                 specificity = specificity(actual = actual, prediction = prediction),
                                                                 true_positive = true_positive(actual = actual, prediction = prediction),
                                                                 false_positive = false_positive(actual = actual, prediction = prediction),
                                                                 rmse = rmse(CURRENT_BILL_actual, CURRENT_BILL_prediction))
                                        , by = .(climate_zone_cd, month_name, BILL_CYCLE, variable, model_index)
                                        ][,av_score_mult:=round(1-false_positive,3)
                                          ][,av_score:=round(true_positive*av_score_mult,3)]

model_baseline_summary <- bill_merge[model_index == 'CONS_USG_.pred'
                                     & variable == 'impact',.(accuracy = accuracy(actual = actual, prediction = prediction),
                                                              precision = precision(actual = actual, prediction = prediction),
                                                              specificity = specificity(actual = actual, prediction = prediction),
                                                              true_positive = true_positive(actual = actual, prediction = prediction),
                                                              false_positive = false_positive(actual = actual, prediction = prediction),
                                                              rmse = rmse(CURRENT_BILL_actual, CURRENT_BILL_prediction))
                                     , by = .(climate_zone_cd, month_name, BILL_CYCLE, variable, model_index)
                                     ][,av_score_mult:=round(1-false_positive,3)
                                       ][,av_score:=round(true_positive*av_score_mult,3)
                                         ][av_score == 0, av_score:=.0001,]

model_predictions_summary_mlt <- melt.data.table(model_predictions_summary
                                                 , id.vars = c('climate_zone_cd','BILL_CYCLE','month_name','model_index')
                                                 , measure.vars = c('accuracy','precision','specificity','true_positive','false_positive','rmse','av_score')
                                                 , value.name = 'model_value'
                                                 , variable.factor = TRUE
                                                 , value.factor = TRUE)

model_baseline_summary_mlt <- melt.data.table(model_baseline_summary
                                              , id.vars = c('climate_zone_cd','BILL_CYCLE','month_name','model_index')
                                              , measure.vars = c('accuracy','precision','specificity','true_positive','false_positive','rmse','av_score')
                                              , value.name = 'baseline'
                                              , variable.factor = TRUE
                                              , value.factor = TRUE)

setkey(model_predictions_summary_mlt, climate_zone_cd, month_name, BILL_CYCLE, variable)
setkey(model_baseline_summary_mlt, climate_zone_cd, month_name, BILL_CYCLE, variable)
model_comp <- merge(model_predictions_summary_mlt, model_baseline_summary_mlt, suffixes = c('_model_value','_baseline'))

model_comp <- data.table(model_lift_pct(model_comp))
model_comp[baseline == 0, model_pct := model_delta]
overall_summary <- model_comp[model_index_model_value == 'TEMP_NORMAL_.pred'
                              & variable %in% c('accuracy') 
                      ][model_pct >100, model_pct:=model_value/model_delta ][order(month_name)]

pct_inc <- model_comp[model_index_model_value == 'TEMP_NORMAL_.pred'
                      & variable %in% c('av_score','true_positive')
                      
                      ][model_pct >100, model_pct:=model_value/model_delta ]

write.csv(x = model_comp,file = )

write.csv(x = pct_inc, file = '../CEA_DATA/model_output/gas_model_quality.csv', row.names = FALSE)


# create heat map summaries

bill_merge_summary <- bill_merge[,.N ,by = .(climate_zone_cd, month_name
                                             , variable, model_index, actual, prediction
                                             , LEVEL_actual, LEVEL_pred)
                                 ][,True_Positive:=sum(N), by =.(climate_zone_cd, month_name # true positive deno
                                                                 , variable, model_index, actual)
                                   ][,False_Positive:=sum(N), by =.(climate_zone_cd, month_name # true positive deno
                                                                    , variable, model_index, prediction)
                                     ][,True_Negative :=sum(N), by =.(LEVEL_actual == LEVEL_pred
                                                                      , climate_zone_cd, month_name
                                                                      , variable, model_index)
                                       ][,False_Negative :=sum(N), by =.(LEVEL_actual != LEVEL_pred
                                                                         , climate_zone_cd, month_name
                                                                         , variable, model_index)
                                         ][,Precision:=sum(N), by =.(climate_zone_cd, month_name, variable # precision deno
                                                                     , model_index, prediction)
                                           ][,`:=`(True_Positive_pct = round(N/True_Positive, 2),
                                                   Precision_pct = round(N/Precision, 2))
                                             ][,FN_FP_N:=sum(N[LEVEL_actual != LEVEL_pred])
                                               , by=.(climate_zone_cd, month_name, variable,model_index, actual, prediction),
                                               ][,TN_N:=ifelse(LEVEL_actual == LEVEL_pred, True_Negative - N, 0)
                                                 ][,FN_N:=ifelse(LEVEL_actual == LEVEL_pred
                                                                 , True_Positive - N, 0),
                                                   ][,TP_N:=sum(N[LEVEL_actual == LEVEL_pred]), by=.(climate_zone_cd, month_name
                                                                                                     , variable,model_index, prediction, actual),
                                                     ][,FP_N:=ifelse(LEVEL_actual == LEVEL_pred
                                                                     , Precision - N, 0),]


# Plotting ----------------------------------------------------------------

# comparing prediction and actual
gc()
levels(bill_merge_summary$variable) <- c('Dollar Change','Percent Change', 'Bill Impact')

m_var <- bill_merge_summary %>% 
  filter(month_name %in% c('Dec','Mar')) %>%
  # filter(climate_zone_cd %in% 'T') %>%
  filter(variable == 'Bill Impact') %>%
  select(climate_zone_cd
         , month_name
         , variable
         , model_index
         , actual
         , prediction
         , LEVEL_actual
         , LEVEL_pred
         , N
         , FN_FP_N
         , TN_N
         , FN_N
         , TP_N
         , FP_N
         , Precision_pct
         , True_Positive_pct
  ) %>% gather(key = measure_type, value = measure_value
               , -c(climate_zone_cd
                    , month_name
                    , variable
                    , model_index
                    , actual
                    , prediction
                    , LEVEL_actual
                    , LEVEL_pred
               )) %>% 
  mutate(current_year = TEST_YEAR, prior_year = TRAIN_YEAR) %>%
  mutate(measure_type_print = gsub(x = measure_type, pattern = '_', ' ')) %>% 
  split(list(.$variable, .$measure_type, .$climate_zone_cd), drop = TRUE)

names(m_var) <- tolower(gsub(pattern = REG_PATTERN, '_', names(m_var))) # rename

PCT_PATTERN <- '_(pct)_'
TWO_MODEL_PATTERN <- '_(fp|fn|tp_n|tn)_'

PCT_MODEL <- grep(pattern = PCT_PATTERN,x = names(m_var))
TWO_MODEL <- grep(pattern = TWO_MODEL_PATTERN,x = names(m_var))
rm(plots)
all_plots <- m_var %>% # no filters for comparisions
  # map(. %>% filter((measure_value != 1.00 | model_index != 'TEMP_NORMAL_usage_actual'))) %>%
  map(~ggplot(.,aes(x = prediction,  y = actual, fill = measure_value)) +
        geom_tile() +
        facet_grid(month_name ~ model_index) +
        scale_fill_gradient(low='#56B1F7', high='#132B43', trans = 'log') +
        geom_text(aes(label = measure_value), color = 'white') +
        guides(fill = "none") +
        ggtitle(paste(.$measure_name,.$variable,.$measure_type_print,'for Climate Zone',.$climate_zone_cd,
                      'comparing',.$prior_year,'to',.$current_year))
  )

pct_comparision <- m_var[PCT_MODEL] %>% # two plots, by comparision
  map(. %>% filter((model_index !='TEMP_NORMAL_usage_actual') 
                   & measure_value != 0)) %>%
  map(~ggplot(.,aes(x = prediction,  y = actual, fill = measure_value)) +
        geom_tile() +
        facet_grid(month_name ~ model_index) +
        scale_fill_gradient(low='#56B1F7', high='#132B43', trans = 'log') +
        geom_text(aes(label = measure_value), color = 'white') +
        guides(fill = "none") +
        ggtitle(paste(.$measure_name,.$variable,.$measure_type_print,'for Climate Zone',.$climate_zone_cd,
                      'comparing',.$prior_year,'to',.$current_year))
  )

two_comparison <-  m_var[TWO_MODEL] %>% # two plots, by comparision
  map(. %>% filter((measure_value != 0))) %>%
  map(~ggplot(.,aes(x = prediction,  y = actual, fill = measure_value)) +
        geom_tile() +
        facet_grid(month_name ~ model_index) +
        scale_fill_gradient(low='#56B1F7', high='#132B43', trans = 'log') +
        geom_text(aes(label = measure_value), color = 'white') +
        guides(fill = "none") +
        ggtitle(paste(.$measure_name,.$variable,.$measure_type_print,'for Climate Zone',.$climate_zone_cd,
                      'comparing',.$prior_year,'to',.$current_year))
  )

plotlist <- list(pct_comparision, two_comparison)


# plotting model comparisons
model_comp$model_index_model_value <- as.factor(model_comp$model_index_model_value)

model_comp$month_name <- factor(model_comp$month_name, levels = c('Nov','Dec','Jan','Feb','Mar'))

# summary for plots
plot_details <- model_comp %>%
  filter(train_indicator == 'TEST') %>% 
  select(climate_zone_cd, n) %>% 
  group_by(climate_zone_cd, n) %>% 
  distinct() %>% 
  ungroup %>% 
  mutate(total = sum(n))

# percent improvement of temperature compared to baseline (constant usage) 
rmse_pct <- ggplot(model_comp %>% filter(model_name %in% c('TEMP_NORMAL_TEST')), 
                   aes(x = month_name, y = model_pct, fill = model_name)) +
  geom_bar(stat = 'identity', aes(y=model_pct, ymin=0, ymax=model_pct), position = 'dodge')+
  geom_text(aes(x = month_name, y = model_pct, ymax = model_pct, label = model_pct,
                vjust = ifelse(sign(model_pct)>0, 1, -1)), position = position_dodge(width = 1))+
  facet_wrap(~climate_zone_cd, ncol = 1)+
  scale_fill_discrete("Model Type") +
  ggtitle("Comparing percent improvement in RMSE  for\nnormal temperature over constant usage") +
  labs(y = 'percent improvement (+/- over baseline)', x = NULL)

# write files -------------------------------------------------------------
plots %>% map(names) %>% unlist()

ALL_PATH <- paste0('images/',names(all_plots),'.png')
TWO_PATH <- paste0('images/',names(two_comparison),'.png')
PCT_PATH <- paste0('images/',names(pct_comparision),'.png')

walk2(ALL_PATH, all_plots, ggsave, width = 11, height = 8.5) # works
walk2(TWO_PATH, two_comparison, ggsave, width = 11, height = 8.5) # works
walk2(PCT_PATH, pct_comparision, ggsave, width = 11, height = 8.5) # works

print('Script Complete')
print(proc.time() - start_time)