# rm(list = ls())

# constants ---------------------------------------------------------------
library(stringr)

# Plotting billing distributions

# READ AND MANIPULATE DATA ------------------------------------------------

BILL_LOCATION <- '../CEA_DATA/billing_data/electric'

x <- dir(BILL_LOCATION, recursive = TRUE, pattern = '.sas', full.names = TRUE)

bills <- map(.x = x[1:2], .f = function(x){
  d = read_sas(x) %>%
    mutate(sample_number = str_extract(x,'\\d+')) %>% 
    as.data.table()
  sp = d[,.N, by = .(prem_id, sa_id, sample, bill_dt, percentile)][N ==1][,unique(prem_id)]
  d = d[prem_id %in% c(sp)]
  sa = d[,.N, by = .(bill_dt, prem_id, percentile)][N!=1][,unique(prem_id)]
  d = d[!prem_id %in% c(sa)]
})%>% rbindlist()


BILL_COLS <- c('sa_id', 'cmprs_rt_sched_cd', 'sample', 'percentile'
               , 'deriv_baseline_terr_cd', 'bill_dt', 'rt_scnro_id'
               , 'bill_ipct_scnro_id', 'acct_id', 'prem_id'
               ,'usg_amt', 'usg_uom', 'bill_amt', 'nbr_of_months'
               , 'max_tier_n','res_ind')

formula <- sample_number + sa_id + deriv_baseline_terr_cd + cmprs_rt_sched_cd + prem_id + acct_id +month +percentile  ~ sample

bills[, care_ind := ifelse(grepl('L', cmprs_rt_sched_cd), 'Y', 'N')]
bills[, month := factor(month.abb[month(bill_dt)])] # get month
bills[,.N, by = .(sample, sample_number, prem_id, percentile, bill_dt)][N!=1] # check .N sample and percentile
bills[,uniqueN(prem_id)]
bills[,lapply(.SD, uniqueN), by = .(sample_number)]
bills[,mean(bill_amt), by = .(prem_id, sample, sample_number)]
bills[,mean(usg_amt), by = .(prem_id, sample, sample_number)]

setkey(bills, sample, sample_number, percentile, deriv_baseline_terr_cd, bill_dt
       , sa_id, bill_ipct_scnro_id, acct_id, prem_id, usg_uom, res_ind, care_ind, month)
bills[,.N, by = key(bills)][order(-N)]

x_l <-  copy(bills) %>% split(list(.$sample, .$percentile), drop = TRUE) # split on sample and percentile
rm(bills)

# model comparision data
temp <- rbindlist(list( # temperature predicted impact
  test_normal = x_l$tespre.normal_temperature,
  test_low = x_l$tespre.temperature_low_10,
  test_mid = x_l$tespre.temperature_mid_high_70,
  test_high = x_l$tespre.temperature_high_90
))
# constant usage comparision data
cons <- rbindlist(list( # constant usage predicted impact
  cons = x_l$conpre.CONS_USG
))
# create baseline 2014_comparision data
baseline_14 <- rbindlist(list( # temperature predicted impact
  train_actual = x_l$traact.model_baseline
))
# create baseline 2015 comparision data
baseline_15 <- rbindlist(list( # temperature predicted impact
  test_actual = x_l$tesact.model_baseline
))
# rm(x_l)
# remove duplicates baseline data
str(baseline_15)
setnames(baseline_14, old = CHANGE_COLS, new = CHANGE_COLS_2014)
setnames(baseline_15, old = CHANGE_COLS, new = CHANGE_COLS_2015)

DC_14 <- baseline_14[,.N, by = .(sample_number, prem_id, acct_id, sa_id_2014, month)][N != 1][,(unique(prem_id))]
DC_15 <- baseline_15[,.N, by = .(sample_number, prem_id, acct_id, sa_id_2015, month)][N !=1 ][,(unique(prem_id))]

baseline_14 <- baseline_14[!prem_id %in% DC_14]
baseline_15 <- baseline_15[!prem_id %in% DC_15]

FULL_PREM <- baseline_15[,(unique(prem_id))]
baseline_14 <- baseline_14[prem_id %in% c(FULL_PREM)]

setkey(baseline_14, prem_id, acct_id, month, sample_number, deriv_baseline_terr_cd) # set key for merge
setkey(baseline_15, prem_id, acct_id, month, sample_number, deriv_baseline_terr_cd) # set key for merge
# merge to calculate actual impact scenario

baseline <- merge(baseline_14, baseline_15[,c('prem_id'
                                              , 'sample_number'
                                              , 'acct_id'
                                              , 'deriv_baseline_terr_cd'
                                              , 'month'
                                              , MERGE_COLS_2015)
                                           , with = FALSE])

DOUBLE_COUNT <- baseline[,.N, by = .(sample_number, prem_id, month)][N!=1][,unique(prem_id)]
baseline <- baseline[!prem_id %in% c(DOUBLE_COUNT)]

# calculate impact metrics
baseline[,`:=`(diff = bill_amt_2015  - bill_amt_2014),]
baseline[,`:=`(diff_pct = diff / bill_amt_2014),]
baseline[, `:=`(diff_break = bucket(diff, 'absolute')
                , diff_pct_break = bucket(diff_pct, 'percent'))]

# clean up missing values
baseline[is.na(diff_break),]
baseline[is.na(diff_pct_break),]

baseline[is.na(diff_break) & diff <0,diff_break:='<$10']
baseline[is.na(diff_pct_break)& diff_pct <0, diff_pct_break:='<10%']

# Calculate the actual impacts 
baseline[,`:=`(impact = get_impact(diff_pct, diff, care_ind_2014)),]

# constant accounts for all comps
FULL_PREM <- baseline[,(unique(prem_id))]

# temp model predicted bills
temp <- temp[prem_id %in% FULL_PREM,]
setkey(temp, sa_id, sample, sample_number, percentile, deriv_baseline_terr_cd, bill_dt
       , sa_id, bill_ipct_scnro_id, acct_id, prem_id, usg_uom, res_ind, care_ind, month)
temp <- temp[,unique(temp)] # make sure there are no double counts missed in script
temp_w <- dcast.data.table(data = temp
                           , formula = formula
                           , value.var = c('bill_amt','usg_amt')
                           , fun.aggregate = sum) # Due to changing SA ids

setnames(temp_w, old = c('sa_id','bill_amt_sum_tespre','usg_amt_sum_tespre','cmprs_rt_sched_cd','percentile')
         , new = c('sa_id_2015','bill_amt_2015','usg_amt_2015','cmprs_rt_sched_cd_2015','percentile_2015'))

setkey(temp_w, prem_id, acct_id, month) # set key to get prior year data
setkey(baseline, prem_id, acct_id, month) # set key to get prior year data

temp_w <- merge(temp_w, baseline[,BASELINE_MERGE_COLS, with = FALSE], allow.cartesian = TRUE)

# calculate impact metrics
temp_w[,`:=`(diff = bill_amt_2015  - bill_amt_2014),]
temp_w[,`:=`(diff_pct = diff / bill_amt_2014),]
temp_w[, `:=`(diff_break = bucket(diff, 'absolute')
              , diff_pct_break = bucket(diff_pct, 'percent'))]

# clean up missing values
temp_w[is.na(diff_break),]
temp_w[is.na(diff_pct_break),]
temp_w[is.na(diff_break) & diff <0,diff_break:='<$10']
temp_w[is.na(diff_pct_break)& diff_pct <0, diff_pct_break:='<10%']

# Calculate the predicted impacts 
temp_w[,`:=`(impact = get_impact(diff_pct, diff, care_ind_2014)),]

setkey(temp_w, prem_id, acct_id, month) # set key to get actual impact
temp_w <- merge(temp_w,baseline[,(BASELINE_COMP_COLS), with = FALSE]
                , suffixes = c(x = '_PREDICTED', y = '_ACTUAL'), allow.cartesian = TRUE)


# constant usage predicted bills
cons <- cons[prem_id %in% FULL_PREM,]
setkey(cons, sa_id, sample, sample_number, percentile, deriv_baseline_terr_cd, bill_dt
       , sa_id, bill_ipct_scnro_id, acct_id, prem_id, usg_uom, res_ind, care_ind, month)
cons <- cons[,unique(cons)]

cons_w <- dcast.data.table(data = cons
                           , formula = formula
                           , value.var = c('bill_amt','usg_amt')
                           , fun.aggregate = sum) # Due to changing SA ids

setnames(cons_w, old = c('sa_id','bill_amt_sum_conpre','usg_amt_sum_conpre','cmprs_rt_sched_cd','percentile')
         , new = c('sa_id_2015','bill_amt_2015','usg_amt_2015','cmprs_rt_sched_cd_2015','percentile_2015'))

setkey(cons_w, prem_id, acct_id, month) # set key to get prior year data
cons_w <- merge(cons_w,baseline[,(BASELINE_MERGE_COLS), with = FALSE])

# calculate impact metrics
cons_w[,`:=`(diff = bill_amt_2015  - bill_amt_2014),]
cons_w[,`:=`(diff_pct = diff / bill_amt_2014),]
cons_w[, `:=`(diff_break = bucket(diff, 'absolute')
              , diff_pct_break = bucket(diff_pct, 'percent'))]

# clean up missing values
cons_w[is.na(diff_break) & diff <0,diff_break:='<$10']
cons_w[is.na(diff_pct_break)& diff_pct <0, diff_pct_break:='<10%']

# Calculate the predicted impacts 
cons_w[,`:=`(impact = get_impact(diff_pct, diff, care_ind_2014)),]

setkey(cons_w, prem_id, acct_id, month) #get actual bill amount
cons_w <- merge(cons_w,baseline[,(BASELINE_COMP_COLS), with = FALSE]
                , suffixes = c(x = '_PREDICTED', y = '_ACTUAL'))

BAD_PREM_CONS <- cons_w[,.N, by = .(prem_id, sample_2014, sample_number, month, sa_id_2015
                                    ,percentile_2014, sa_id_2014)][N!=1][,unique(prem_id)]
BAD_PREM_TEMP <- temp_w[,.N, by = .(prem_id, sample_2014, sample_number, month, sa_id_2015
                                    ,percentile_2015, sa_id_2015)][N!=1][,unique(prem_id)]
BAD_PREM_BASE <- baseline[,.N, by = .(prem_id, sample_number, month
                                      ,percentile_2014, sa_id_2014)][N!=1][,unique(prem_id)]

cons <- copy(cons_w <- cons_w[!prem_id %in% BAD_PREM_CONS,])
temp <- copy(temp_w[!prem_id %in% BAD_PREM_CONS,])
baseline <- baseline[!prem_id %in% BAD_PREM_CONS,]


# Calculate impact metrics ------------------------------------------------

# Get the count of TP/TN/FP/FN
model_bills <- rbind(cons, temp)
# rm(bills)
mb_save <- copy(model_bills)
# check follow up 

model_bills[,.N, by = .(deriv_baseline_terr_cd, percentile_2015, sample_number)]

model_bills_sm <- mb_save[sample_number == 5000 
                           & percentile_2015 %in% c('CONS_USG', 'normal_temperature')
                           & month == 'Aug'
                           & deriv_baseline_terr_cd == 'W',]

# do this by 

model_bills <- model_bills_sm

model_bills[, `:=`(actual_binary = impact_ACTUAL == 'high'
                   , predicted_binary = impact_PREDICTED == 'high')]

model_bills[,pred_rank_binary:=min_rank(desc(predicted_binary)), by = .(percentile_2015)]
model_bills[,pred_rank_percent:=min_rank(desc(diff_pct_PREDICTED)), by = .(pred_rank_percent, percentile_2015)]
model_bills[,actual_rank_binary:=min_rank(desc(actual_binary)), by = .(percentile_2015)]
model_bills[,actual_rank_percent:=min_rank(desc(diff_pct_ACTUAL)), by = .(actual_rank_binary)]

# model_bills <- model_bills[predicted_binary == 'TRUE',] #re
# model_bills[,lapply(.SD, head, 10), by = .(percentile_2015) ]
setkey(model_bills, actual_rank_percent)
model_bills[predicted_binary == 'TRUE',.(predicted_binary
                                      , actual_binary
                                      , percentile_2015
                                      , pred_rank_binary
                                      , actual_rank_binary
                                      , pred_rank_percent
                                      , actual_rank_percent
                                      , diff_pct_PREDICTED
                                      , diff_pct_ACTUAL
                                                  )]

head(model_bills)
metrics_monthly <- model_bills[, .(TP = sum(predicted_binary == 'TRUE' & actual_binary == 'TRUE')
                                   , FP = sum(predicted_binary == 'TRUE' & actual_binary == 'FALSE')
                                   , TN = sum(predicted_binary == 'FALSE' & actual_binary == 'FALSE')
                                   , FN = sum(predicted_binary == 'FALSE' & actual_binary == 'TRUE'))
                               , by = c('percentile_2015'
                                        , 'deriv_baseline_terr_cd'
                                        , 'month'
                                        , 'sample_number')]

metrics_monthly[, `:=`(TP_rate = TP / (TP + FN), FP_rate = FP / (FP + TN))]
metrics_monthly[, amitava_accuracy := TP_rate * (1 - FP_rate)]

metrics_annual <- model_bills[, .(TP = sum(predicted_binary == 'TRUE' & actual_binary == 'TRUE')
                                  , FP = sum(predicted_binary == 'TRUE' & actual_binary == 'FALSE')
                                  , TN = sum(predicted_binary == 'FALSE' & actual_binary == 'FALSE')
                                  , FN = sum(predicted_binary == 'FALSE' & actual_binary == 'TRUE'))
                              , by = c('percentile_2015', 'deriv_baseline_terr_cd','sample_number')]

metrics_annual[, `:=`(TP_rate = TP / (TP + FN), FP_rate = FP / (FP + TN))]
metrics_annual[, amitava_accuracy := TP_rate * (1 - FP_rate)]



# Write out results -------------------------------------------------------
write.table(model_bills, '../CEA_DATA/model_comparisions/model_customer_quality.txt', row.names = FALSE, sep = ',')
write.table(metrics_annual, '../CEA_DATA/model_comparisions/model_quality_monthly.txt', row.names = FALSE, sep = ',')
write.table(metrics_monthly, '../CEA_DATA/model_comparisions/model_quality_annual.txt', row.names = FALSE, sep = ',')

# new heatmaps ------------------------------------------------------------
baseline <- baseline[,impact_ACTUAL := impact]
setnames(baseline, old = c('impact','percentile_2014')
         , new = c('impact_PREDICTED','percentile_2015'))

baseline[,DATA_TYPE:=ifelse(percentile_2015 == 'model_baseline','Reference'
                            ,'Comparison')]
model_bills[,DATA_TYPE:=ifelse(percentile_2015 == 'model_baseline','Reference'
                               ,'Comparison')]

baseline$month <- factor(baseline$month, levels = month.abb)
model_bills$month <- factor(model_bills$month, levels = month.abb)

model_bills$percentile_2015 <- factor(model_bills$percentile_2015,
                                      levels = c('model_baseline'
                                                 , 'CONS_USG'
                                                 , 'normal_temperature'
                                                 , 'temperature_low_10'
                                                 , 'temperature_mid_high_70'
                                                 , 'temperature_high_90'))


# baselinee reference (actual) confusion matrix data
BASELINE_MATRIX_MONTH <- baseline[,.N, by = .(DATA_TYPE
                                              , percentile_2015
                                              , sample_number
                                              , month
                                              , deriv_baseline_terr_cd
                                              , impact_PREDICTED
                                              , impact_ACTUAL)]

BASELINE_MATRIX_ANNUAL <- BASELINE_MATRIX_MONTH[,sum(N)
                                                ,by = .(DATA_TYPE
                                                        , percentile_2015
                                                        , sample_number
                                                        # , month
                                                        , deriv_baseline_terr_cd
                                                        , impact_PREDICTED
                                                        , impact_ACTUAL)]

# model confusion matrix
MODEL_MATRIX_MONTH <- model_bills[,.N, , by = .(DATA_TYPE
                                                , percentile_2015
                                                , sample_number
                                                , month
                                                , deriv_baseline_terr_cd
                                                , impact_PREDICTED
                                                , impact_ACTUAL)]

MODEL_MATRIX_ANNUAL <- MODEL_MATRIX_MONTH[,sum(N)
                                          ,by = .(DATA_TYPE
                                                  , percentile_2015
                                                  , sample_number
                                                  # , month
                                                  , deriv_baseline_terr_cd
                                                  , impact_PREDICTED
                                                  , impact_ACTUAL)]
setnames(BASELINE_MATRIX_ANNUAL,'V1', 'N')
setnames(MODEL_MATRIX_ANNUAL,'V1', 'N')

BILL_COMP_MONTHLY <- rbind(BASELINE_MATRIX_MONTH, MODEL_MATRIX_MONTH)
BILL_COMP_ANNUAL <- rbind(BASELINE_MATRIX_ANNUAL, MODEL_MATRIX_ANNUAL)


BILLS_PLOT_LIST_MONTHLY <-  BILL_COMP_MONTHLY %>% 
  split(list(.$deriv_baseline_terr_cd, .$month, .$DATA_TYPE, .$sample_number), drop = TRUE)

BILLS_PLOT_LIST_ANNUAL <-  BILL_COMP_ANNUAL %>% 
  split(list(.$deriv_baseline_terr_cd, .$DATA_TYPE), drop = TRUE)

# monthly plots
BILLS_PLOTS_MONTHLY <- BILLS_PLOT_LIST_MONTHLY %>% # no filters for comparisons
  map(~ggplot(.,aes(x = impact_PREDICTED,  y = impact_ACTUAL, fill = N)) +
        geom_tile() +
        facet_wrap(~ percentile_2015) +
        scale_fill_gradient(low='#56B1F7', high='#132B43', trans = 'log') +
        geom_text(aes(label = N), color = 'white') +
        guides(fill = "none") +
        ggtitle(paste(.$DATA_TYPE,'of Temp Scenarios\nfor sample #'
                      ,.$sample_number,'in Climate Zone'
                      ,.$deriv_baseline_terr_cd,'in',.$month)) +
        labs(x = 'predicted impact', y = 'actual impact'))
BILLS_PLOTS_MONTHLY$T.Aug.Reference.5000
BILLS_PLOTS_MONTHLY$T.Aug.Comparison.5000

# annual plots
BILLS_PLOTS_ANNUAL <- BILLS_PLOT_LIST_ANNUAL %>% 
  map(~ggplot(.,aes(x = impact_PREDICTED,  y = impact_ACTUAL, fill = N)) +
        geom_tile() +
        facet_grid(sample_number~ percentile_2015) +
        scale_fill_gradient(low='#56B1F7', high='#132B43', trans = 'log') +
        geom_text(aes(label = N), color = 'white') +
        guides(fill = "none") +
        ggtitle(paste(.$DATA_TYPE,'of Temp Scenarios\nfor Climate Zone'
                      ,.$deriv_baseline_terr_cd,'for 2015 Bill Impact')) +
        labs(x = 'predicted impact', y = 'actual impact'))

BILLS_PLOTS_ANNUAL$W.Comparison

# plot ar score -----------------------------------------------------------
NCUST <- model_bills[,uniqueN(prem_id)]

metrics_monthly[,aa_rank:=min_rank(amitava_accuracy)
                , by = .(deriv_baseline_terr_cd, month, sample_number)]

metrics_annual[,aa_rank:=min_rank(amitava_accuracy)
               , by = .(deriv_baseline_terr_cd, sample_number)]

RANK_LONG_MONTH <- melt.data.table(metrics_monthly, 
                                   id.vars = c('percentile_2015',
                                               'deriv_baseline_terr_cd'
                                               ,'month'
                                               ,'sample_number')
                                   ,measure.vars = c('TP_rate'
                                                     , 'FP_rate'
                                                     , 'amitava_accuracy'))


RANK_LONG_ANNUAL <- melt.data.table(metrics_annual, 
                                    id.vars = c('percentile_2015',
                                                'deriv_baseline_terr_cd'
                                                ,'sample_number')
                                    ,measure.vars = c('TP_rate'
                                                      , 'FP_rate'
                                                      , 'amitava_accuracy'))

RANK_LONG_MONTH[,value := round(value, 1)]
RANK_LONG_ANNUAL[,value := round(value, 1)]


ANNUAL_RANKINGS_PLOT <- ggplot(RANK_LONG_ANNUAL, aes(x = percentile_2015
                                                     , y = value
                                                     , fill = variable)) +
  facet_wrap(~deriv_baseline_terr_cd, ncol = 1) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = value), position = position_dodge(1)) +
  # coord_flip() +
  labs(x = 'Temperature Scenario', y = 'Metric Value') +
  ggtitle(paste('Comparing Annual Impact Metrics for',NCUST,'customers across Climate Zone Codes'))

MONTHLY_RANKINGS_PLOT <- ggplot(RANK_LONG_MONTH, aes(x = percentile_2015
                                                     , y = value
                                                     , fill = variable)) +
  facet_wrap(~month, ncol = 1) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = value), position = position_dodge(1)) +
  # coord_flip() +
  labs(x = 'Temperature Scenario', y = 'Metric Value') +
  ggtitle(paste('Comparing Annual Impact Metrics for',NCUST,'customers across Climate Zone Codes'))

write.xlsx(x = metrics_monthly
           , file = '../CEA_DATA/model_comparisions/metrics_monthly.xlsx'
           ,sheetName = 'monthly' )
write.xlsx(x = metrics_annual
           , file = '../CEA_DATA/model_comparisions/metrics_annual.xlsx'
           ,sheetName = 'annual' )
write.csv(x = metrics_annual, file = 'CEA/model_export/annual_rankings.csv')


# number of TP per month per climate zone code (the actual # of actuals, then calculate with that.)



MET[,tp_rank:=min_rank(desc(TP_rate)),by = .(deriv_baseline_terr_cd)] # rank better
MET[,aa_rank:=min_rank(amitava_accuracy),by = .(deriv_baseline_terr_cd)] # rank better
MET[tp_rank %in% c(1, 5),]
MET[aa_rank %in% c(1, 5),]
ggplot(metrics)


# Plot the heatmaps -------------------------------------------------------
sample_nos <- c(1, 2, 3)
for (sample_no in sample_nos){
  sample_current <- paste0('sample', sample_no)
  sample_no <- unique(bills$percentile)
  # Constant usage predictions
  print(plot_heatmap(bills
                     , c('old_diff_pct_break', 'old_diff_break')
                     , paste0('Constant Model Predictions (Sample ', sample_no, ')'
                              , paste0('constant_usage_predictions_sample_', sample_no)) +
                       xlab('') +
                       ylab(''))
        bills
        # Temperature model predictions
        print(plot_heatmap(bills[sample == sample_current]
                           , c('new_diff_pct_break', 'new_diff_break')
                           , paste0('Temperature Model Predictions (Sample ', sample_no, ')')
                           , paste0('temperature_predictions_sample_', sample_no)) + 
                xlab('') + 
                ylab('')
        )
        
        # Constant Usage confusion matrix
        print(plot_heatmap(bills[sample == sample_current]
                           , c('old_impact', 'actual_impact')
                           , paste0('Constant Usage Model Confusion Matrix (Sample ', sample_no, ')')
                           , paste0('constant_usage_confusion_matrix_sample_', sample_no)
        )
        )
        
        # Temperature model confusion matrix
        print(plot_heatmap(bills[sample == sample_current]
                           , c('new_impact', 'actual_impact')
                           , paste0('Temperature Model Confusion Matrix (Sample ', sample_no, ')')
                           , paste0('temperature_confusion_matrix_sample_', sample_no)
        )
        <<<<<<< HEAD
        ) 
        =======
  )
  >>>>>>> 8098c44733e9e7a80e0e70a4ae501743b0e5baf8
  
}