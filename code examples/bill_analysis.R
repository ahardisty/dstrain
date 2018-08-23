source('r/functions.R')

# Load constants ----------------------------------------------------------
USER <- 'A3HE'
setwd(paste0('C:/Users/',USER,'/Documents/'))
SUBSET_CLIMATE <- c('W (Desert/Mountains)','T (Coast)','X (Hills)')
# SUBSET_CLIMATE <- c('W (Desert/Mountains)')
SUBSET_MONTH <- c('Aug','May')
# SUBSET_MONTH <- c('Aug')
# SAMPLE_SET <- c('sample1')
SAMPLE_SET <- c('sample1','sample2','sample3')
CARE_IND <- 'N'
TEMP_TYPE <- 'temperature_normal'

IN_FILE_ALL <- dir(paste0('C:/Users/',USER,'/Documents/data/temp_scenario_bill_output')
               , pattern = '.sas7bdat', full.names = TRUE)

IN_FILE <- IN_FILE_ALL[grep(pattern = TEMP_TYPE, x = IN_FILE_ALL)]
METRICS_DATA_LOC <- paste0('C:/Users/',USER,'/Documents/data/model_quality/phase3/billing/')

BILL_COLS <- c('sa_id', 'cmprs_rt_sched_cd', 'sample', 'percentile'
               , 'deriv_baseline_terr_cd', 'bill_dt', 'rt_scnro_id'
               , 'bill_ipct_scnro_id', 'acct_id', 'prem_id'
               ,'usg_amt', 'usg_uom', 'bill_amt', 'nbr_of_months'
               , 'max_tier_n','res_ind')

# Calculate impact metrics - ranked remove zero -------------------------------------------------------------
bills <- read_bills(IN_FILE)
# bills[,lapply(.SD, uniqueN),]
# 21680 unique prem id in all three samples

bills[,lapply(.SD, uniqueN), by = .(deriv_baseline_terr_cd)] 
# deriv_baseline_terr_cd care_ind sample prem_id acct_id month actual_test actual_train forecast_new forecast_old
# 1:              T (Coast)        2      3    7065    7053     2       14312        14132        14311        14296
# 2:   W (Desert/Mountains)        2      3    6927    6905     2       15097        15446        15105        15104
# 3:              X (Hills)        2      3    7688    7658     2       15040        15007        15045        15035

bills[,uniqueN(prem_id), by = .(actual_train == 0)] 
bills[,uniqueN(prem_id), by = .(actual_train == 0, deriv_baseline_terr_cd)] 
bills[,uniqueN(prem_id), by = .(actual_test == 0)] 
bills[,uniqueN(prem_id), by = .(actual_test == 0, deriv_baseline_terr_cd)] 

# IDENTIFY ZERO BILL AMOUNT
ZERO_BILL_PREM <- as.character(bills[(actual_train == 0 | actual_test == 0)
                                     ,unique(prem_id)]) # obs with no zero bill values

## REMOVE ZERO BILL AMOUNT CUSTOMERS
bills <- bills[!prem_id %in% c(ZERO_BILL_PREM)]

# SUBSET FOR DETAILED INVESTIGATION
bills <- subset(bills 
                , deriv_baseline_terr_cd %in% c(SUBSET_CLIMATE)
                & month %in% c(SUBSET_MONTH)
                & sample %in% c(SAMPLE_SET)
                & care_ind %in% c(CARE_IND)
                )
# ESTABLISH BASELINE MEASUREMENTS
baseline <- bills[,.(care_ind, sample, deriv_baseline_terr_cd, prem_id, acct_id
                     , month, actual_train, actual_test)]

# Calculate absolute differences for baseline
baseline[,`:=`(diff = actual_test - actual_train)]

# Calculate the percent differences for baseline
baseline[, `:=`(diff_pct = diff / actual_train)]

# Discretize bill differences and bill percentages for baseline
baseline[, `:=`(diff_break = bucket(diff, 'absolute')
                , diff_pct_break = bucket(diff_pct, 'percent'))]

# Calculate the high impacts for baseline
baseline[,`:=`(impact = get_impact(diff_pct, diff, care_ind))]

# create rank indicator for baseline
baseline[, `:=`(binary_rank = ifelse(min_rank(desc(impact)) != 1, 0, min_rank(desc(impact))))]
# baseline[, `:=`(pct_rank = min_rank(desc(diff_pct)))
#          , by = .(sample, deriv_baseline_terr_cd, month)]
baseline <- baseline[order(-diff_pct)]

# verify binary rank assignment
baseline[,.N, by = .(deriv_baseline_terr_cd, month, binary_rank, sample, care_ind, impact)]

# establish N for prediction ranking cut off at monthly level
baseline[,pct_rank_n:=sum(impact == 'high'), by = .(deriv_baseline_terr_cd
                                                    , month
                                                    , sample)]
baseline[,.N, by = .(sample, pct_rank_n, deriv_baseline_terr_cd, month, impact)]
baseline[,unique(pct_rank_n), by = .(sample, deriv_baseline_terr_cd, month)][order(sample)]
baseline[,impact:= factor(impact, levels = c('high','medium','low','savers'))]

# ESTABLISH MODEL MEASUREMENTS
bills <- bills[,.(care_ind, sample, deriv_baseline_terr_cd, prem_id, acct_id
                  , month, forecast_old, forecast_new)]

# melt for easier comparision
bills_long <- melt.data.table(bills, measure.vars = c('forecast_old','forecast_new')
                              , variable.name = c('model_type')
                              , value.name = 'predicted')

# merge to calculate differences
bills_long <- merge(bills_long, baseline[,.(care_ind, sample, deriv_baseline_terr_cd
                                            , prem_id, acct_id, month, actual_train, pct_rank_n)]
                    , by = c('sample','deriv_baseline_terr_cd','prem_id','acct_id','month')
                    , suffixes = c('_2015', '_2014'))

# bills_long[month == 'Aug', unique(pct_rank_n), by = .(sample)]
bills_long[,`:=`(diff = predicted - actual_train)] # Calculate absolute differences
bills_long[, `:=`(diff_pct = diff / actual_train)] # Calculate the percent differences

# Discretize bill differences and bill percentages
bills_long[, `:=`(diff_break = bucket(diff, 'absolute'), diff_pct_break = bucket(diff_pct, 'percent'))]

# Calculate the high impacts 
bills_long[,`:=`(impact = get_impact(diff_pct, diff, care_ind_2014))]

# create binary indicator
bills_long[, `:=`(binary_rank = min_rank(desc(impact))),
           by = .(sample, deriv_baseline_terr_cd, month, model_type)
           ][,binary_rank:= ifelse(binary_rank != 1, 0, binary_rank)] #create high impact binary rank

bills_long[binary_rank == 1L,.N, by = .(month, sample, binary_rank, model_type, deriv_baseline_terr_cd, impact),] # check distribution
# binary_rank   model_type deriv_baseline_terr_cd impact   N
# 1:           1 forecast_new   W (Desert/Mountains)   high 218
# 2:           1 forecast_old   W (Desert/Mountains)   high  90

# SET FACTOR LEVELS / ORDER
bills_long[,impact:= factor(impact, levels = c('high','medium','low','savers'))]

bills_long[, `:=`(pct_rank = min_rank(desc(diff_pct)))
           , by = .(sample, month, deriv_baseline_terr_cd, model_type, impact)]

bills_long <- bills_long[order(impact, pct_rank)]

# take only meaningfull variables for comparison
bills_long <- bills_long[,.(sample, prem_id, deriv_baseline_terr_cd, month, model_type
                            , care_ind_2014, diff_pct, diff_pct_break
                            , impact, binary_rank, pct_rank)][order(-diff_pct)]

setkey(bills_long, sample, deriv_baseline_terr_cd, month, prem_id)
setkey(baseline, sample, deriv_baseline_terr_cd, month, prem_id)

bills_long <- merge(bills_long, baseline[,.(sample, deriv_baseline_terr_cd, month
                                            , prem_id, acct_id, diff_pct
                                            , impact, binary_rank, pct_rank_n
                                            )],
                    suffixes = c(x = '_PREDICTED', y = '_ACTUAL'))[order(impact_ACTUAL, pct_rank)]
bills_long <- bills_long[order(-diff_pct_PREDICTED)]

bills_by_prem <- copy(bills_long)

bills_by_prem[,model_type := factor(model_type, labels = c('Constant_Usage','Temperature')),] 
bills_by_prem[,deriv_baseline_terr_cd := factor(deriv_baseline_terr_cd)]
bills_by_prem[,temp_type:=factor(TEMP_TYPE)]
bills_by_prem[,.N, by = .(deriv_baseline_terr_cd, temp_type, month, model_type)]


# ADJUSTED PERFORMANCE METRICS ---------------------------------------------------------------

bills_by_prem <- bills_by_prem[order(model_type, impact_PREDICTED)]

# bills_by_prem_rank <-bills_by_prem[prem_id %in% c(test_prem)]
bills_by_prem[,adjusted_impact_class_rank := 1:.N
                   , by = .(sample, deriv_baseline_terr_cd, month, temp_type, model_type)]

bills_by_prem[prem_id == '6751390944',]
bills_by_prem[adjusted_impact_class_rank == 640L]
bills_by_prem[,.N, by = .(impact_PREDICTED)]

bills_by_prem[, impact_class_ADJUSTED := ifelse(adjusted_impact_class_rank %between% c(1,pct_rank_n),'high','non_high')]
bills_by_prem[, binary_rank_ADJUSTED:= ifelse(impact_class_ADJUSTED == 'high',1,0)]


bills_by_prem[prem_id %in% c('7870068189')
           & month == 'Aug']

bills_by_prem[adjusted_impact_class_rank == 639L
           & month == 'Aug']

metrics <- bills_by_prem[,.(TP = sum(binary_rank_ADJUSTED == 1 & binary_rank_ACTUAL == 1),
                                 FN = sum(binary_rank_ADJUSTED == 0 & binary_rank_ACTUAL == 1),
                                 FP = sum(binary_rank_ADJUSTED == 1 & binary_rank_ACTUAL == 0),
                                 TN = sum(binary_rank_ADJUSTED == 0 & binary_rank_ACTUAL == 0)
), by = .(sample, temp_type, model_type, month, deriv_baseline_terr_cd)]

metrics[, `:=`(TP_rate = round(TP / (TP + FN),2), FP_rate = round(FP / (FP + TN),2)),]
metrics[, AM := round(TP_rate * (1 - FP_rate),2)]

# CREATE BASELINE
metrics_baseline <- merge(metrics[model_type == 'Temperature']
                          , metrics[model_type == 'Constant_Usage'
                                  ,.(sample, temp_type, month, deriv_baseline_terr_cd, TP_rate, FP_rate, AM)]
                          , by = c('sample','temp_type','month','deriv_baseline_terr_cd')
      , suffixes = c(x = '_model', y = '_baseline'))

metrics_baseline[,AM:= round(-(AM_baseline - AM_model)/AM_baseline,2)]
metrics_baseline[,TP_rate:= round(-(TP_rate_baseline - TP_rate_model)/TP_rate_baseline,2)]
metrics_baseline[,FP_rate:= round(-(FP_rate_baseline - FP_rate_model)/FP_rate_baseline,2)]
metrics_baseline[,model_type:= 'Percent_Change']

metrics <- rbind(metrics, metrics_baseline[,.(sample, temp_type, month, deriv_baseline_terr_cd, model_type
                                   , TP, FN, FP, TN, TP_rate, FP_rate, AM)])

# TRANSFORM TO LONG
METRIC_COMP <- melt.data.table(metrics, 
                               id.vars = c('sample','temp_type','model_type', 'month', 'deriv_baseline_terr_cd')
                                           , measure.vars = c('TP_rate','FP_rate','AM')
                               , variable.name = 'metric_name'
                               , value.name = 'metric_value')

# PLOT METRICS ------------------------------------------------------------
BILLS_PLOT_METRIC_COMP <-  METRIC_COMP %>% 
  # filter(metric_name == 'AM') %>% 
  # split(list(.$month, .$deriv_baseline_terr_cd), drop = TRUE)
  split(list(.$metric_name, .$temp_type), drop = TRUE)

# WITH MAP

ALL_PLOTS <- BILLS_PLOT_METRIC_COMP %>%
  # map(. %>% filter(metric_name == 'AM')) %>%
  map(~ ggplot(., aes(x = deriv_baseline_terr_cd, y = metric_value, fill = model_type)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  facet_grid(sample~month) +
  scale_fill_discrete('Model Types'
                      , labels = c('Constant Usage'
                                   ,'Temperature Model'
                                   , 'Percent Change')) +
    labs(x = 'Climate Zone', y = 'metric score') +
  geom_text(aes(label = metric_value), position = position_dodge(width = 1)) +
  ggtitle(paste('Comparing',.$metric_name, 'between Constant Usage and Temperature Model\nfor'
                ,'predictions made with',.$temp_type)))

ALL_PLOTS[[1]]
ALL_PLOTS[[2]]
ALL_PLOTS[[3]]

OUT_NAME <- paste0(gsub(pattern = '\\.',replacement = '_', x = names(ALL_PLOTS)),'.png')
OUT_PATH <- paste0(METRICS_DATA_LOC,OUT_NAME)
walk2(OUT_PATH, ALL_PLOTS, ggsave, width = 11, height = 8.5)

# PLOT HEAT MAPS ----------------------------------------------------------

baseline[,model_type := 'ACTUAL']
baseline[,temp_type := TEMP_TYPE]
baseline[,impact_class_ADJUSTED := impact]
setnames(baseline, old = c('impact'), new = 'impact_ACTUAL')

baseline_summary <- baseline[,.N, by = .(sample, model_type, deriv_baseline_terr_cd
                             , month, temp_type, impact_ACTUAL
                             , impact_class_ADJUSTED)]

model_summary <- bills_by_prem[,.N, by = .(sample, model_type, deriv_baseline_terr_cd
                         , month, temp_type, impact_ACTUAL
                         , impact_class_ADJUSTED)]

HEATMAP_COMP <- rbind(baseline_summary, model_summary)
HEATMAP_COMP[,impact_class_ADJUSTED:=factor(impact_class_ADJUSTED, levels = c('non_high', 'savers','low','medium','high'))]
HEATMAP_COMP[,impact_ACTUAL:=factor(impact_ACTUAL, levels = c('savers','low','medium','high'))]
HEATMAP_COMP[,split_climate_zone:=substr(deriv_baseline_terr_cd, start = 1,1)]
levels(factor(HEATMAP_COMP$impact_class_ADJUSTED))
levels(factor(HEATMAP_COMP$impact_ACTUAL))

BILLS_PLOT_HEATMAP_COMP <-  HEATMAP_COMP %>% 
  split(list(.$split_climate_zone, .$temp_type, .$month), drop = TRUE)

# WITH MAP
ALL_PLOTS <- BILLS_PLOT_HEATMAP_COMP %>%
  # map(. %>% filter(metric_name == 'AM')) %>%
  map(~ ggplot(., aes(x = impact_ACTUAL, y = impact_class_ADJUSTED, fill = N)) +
        facet_grid(sample~model_type) +
        geom_tile() +
        geom_text(aes(label = N), color = 'white') +
        xlab('Impact Forecast / Adjusted Impact Forecast') + 
        ylab('Actual Impact') + 
        scale_fill_gradient(low='#56B1F7', high='#132B43') +
        ggtitle(paste('Confusion Matrix for Climate Zone',.$deriv_baseline_terr_cd, '\n'
                      ,.$month,'Bill Impact predictions made with',.$temp_type)) +
        # theme_bw() +
        theme(legend.position="none",
              # text = element_text(size = 16), 
              axis.line = element_line(colour = "black")
              # panel.grid.major = element_blank()
              # panel.grid.minor = element_blank(),
              # panel.border = element_blank(),
              # panel.background = element_blank())
      ))
ALL_PLOTS[[1]]
OUT_NAME <- tolower(paste0(gsub(pattern = '\\.',replacement = '_', x = names(ALL_PLOTS)),'_heat_map.png'))
OUT_PATH <- paste0(METRICS_DATA_LOC,OUT_NAME)
walk2(OUT_PATH, ALL_PLOTS, ggsave, width = 11, height = 8.5)


      
# RAW OUTPUT FOR CALIBRATION ------------------------------------------------------------
# keep long results
bills_by_prem_rank <- bills_by_prem[,.(temp_type, month, sample, prem_id, model_type, deriv_baseline_terr_cd
                                  , diff_pct_ACTUAL
                                  , diff_pct_PREDICTED
                                  # , pct_rank_ACTUAL
                                  , pct_rank_n
                                  , binary_rank_ACTUAL
                                  , pct_rank
                                  , impact_ACTUAL
                                  , impact_PREDICTED
                                  , impact_class_ADJUSTED
                                  , binary_rank_ADJUSTED)]

bills_by_prem_rank[,sample:= factor(sample)]
MODEL_COMP_SPLIT <- bills_by_prem_rank[month %in% c('Aug','May')] %>%
  split(list(.$sample, .$model_type, .$month), drop = TRUE)

PHASE_NAME <- 'phase3'
DATA_LOC <- paste0('C:/Users/',USER,'/Documents/data/model_quality/',PHASE_NAME,'/billing/')
FILE_NAME <- 'am_raw_data'
OUT_NAME <- paste0(DATA_LOC,FILE_NAME,'.xlsx')
 
# # write out wide results
# write.xlsx(x = MODEL_COMP_SPLIT[[1]]
#            , file = OUT_NAME
#            , row.names = FALSE
#            , sheetName = tolower(names(MODEL_COMP_SPLIT[1]))
#            )
# 
# write.xlsx(x = MODEL_COMP_SPLIT[[2]]
#            , file = OUT_NAME
#            , row.names = FALSE
#            , sheetName = tolower(names(MODEL_COMP_SPLIT[2]))
#            , append = TRUE
#            )
# write.xlsx(x = MODEL_COMP_SPLIT[[3]]
#            , file = OUT_NAME
#            , row.names = FALSE
#            , sheetName = tolower(names(MODEL_COMP_SPLIT[3]))
#            , append = TRUE
#            )
# 
# write.xlsx(x = MODEL_COMP_SPLIT[[4]]
#            , file = OUT_NAME
#            , row.names = FALSE
#            , sheetName = tolower(names(MODEL_COMP_SPLIT[4]))
#            , append = TRUE
# )

# walk(MODEL_COMP_SPLIT, write.xlsx, file = OUT_NAME, row.names = FALSE, append = TRUE)

pwalk(.l = list(x = MODEL_COMP_SPLIT
                , sheetName = names(MODEL_COMP_SPLIT)
                , file = OUT_NAME)
      , .f = write.xlsx, row.names = FALSE, append = TRUE)

write.xlsx(file = )
write.xlsx()
ALL_MODELS <- MODEL_COMP_SPLIT[1] %>%
  # map(. %>% filter(metric_name == 'AM')) %>%
  walk(~ write.xlsx(., file = OUT_NAME, row.names = FALSE, append = TRUE
                    , sheetName = .$sample))

  map(~ ggplot(., aes(x = impact_ACTUAL, y = impact_class_ADJUSTED, fill = N)) +
        facet_grid(sample~model_type) +
        geom_tile() +
        geom_text(aes(label = N), color = 'white') +
        xlab('Impact Forecast / Adjusted Impact Forecast') + 
        ylab('Actual Impact') + 
        scale_fill_gradient(low='#56B1F7', high='#132B43') +
        ggtitle(paste('Confusion Matrix for Climate Zone',.$deriv_baseline_terr_cd, '\n'
                      ,.$month,'Bill Impact predictions made with',.$temp_type)) +
        # theme_bw() +
        theme(legend.position="none",
              # text = element_text(size = 16), 
              axis.line = element_line(colour = "black")
              # panel.grid.major = element_blank()
              # panel.grid.minor = element_blank(),
              # panel.border = element_blank(),
              # panel.background = element_blank())
        ))


model

dim(MODEL_COMP_SPLIT) 

# recreate the findings ---------------------------------------------------
# library(xlsx)

# bills_by_prem_rank <- read.xlsx(file = '../Documents/eua/documentation/Metrics guidelines Aaron-H-Nov-9-2016.xlsx', sheetName = 'Temperature'
#                                 , endRow = 1625, colIndex = c(1:12)) %>% as.data.table()
# # bills_by_prem_rank <- read.xlsx(file = OUT_NAME, sheetName = 'constant_usage') %>% as.data.table()
# # 
# test_prem <- c('7955333848','2751979591','1116459772','6862240104')
# head(bills_by_prem_rank)
# # 
# bills_by_prem_rank[,month := 'Aug']
# bills_by_prem_rank[,.N, by = .(impact_temperature_normal)]
# bills_by_prem_rank[,lapply(.SD, uniqueN)]
# bills_by_prem_rank[,.N, by = model_type]
# # 
# # # set as factors for sorting
# # 
# bills_by_prem_rank$impact_temperature_normal <- factor(bills_by_prem_rank$impact_temperature_normal
#                                                        , levels = c('high','medium','nlow','savers')
#                                                        , labels = c('high','medium','low','savers'))
# bills_by_prem_rank <- bills_by_prem_rank[order(impact_temperature_normal, pct_rank_temperature_normal)]
# # 
# bills_by_prem_rank[,adjusted_impact_class_rank := 1:.N
#                    , by = .(sample, month, deriv_baseline_terr_cd)]
# 
# # bills_by_prem_rank[,adjusted_impact_class_rank_2 := min_rank(desc(diff_pct_temperature_normal))
# #            , by = .(month, deriv_baseline_terr_cd, sample, impact_temperature_normal)] # create 1:N ranking
# # 
# # bills_by_prem_rank[,adjusted_impact_class_rank_alt := min_rank(desc(diff_pct_temperature_normal))
# #            , by = .(month, deriv_baseline_terr_cd)] # create 1:N ranking
# # bills_by_prem_rank[rank == adjusted_impact_class_rank_2,]
# # 
# bills_by_prem_rank[prem_id %in% c(test_prem),]
# bills_by_prem_rank[prem_id == '6751390944',]
# bills_by_prem_rank[rank == 640L]
# bills_by_prem_rank[,.N, by = .(impact_temperature_normal)]
# 
# bills_by_prem_rank[, adj_impact_class := ifelse(adjusted_impact_class_rank %between% c(1,pct_rank_n_aug)
#                                                 ,'high','non_high')]
# # 
# # bills_by_prem_rank[, adj_impact_class_alt := ifelse(adjusted_impact_class_rank_alt %between% c(1,pct_rank_n_aug)
# #                                                 ,'high','non_high')]
# # 
# bills_by_prem_rank[,adjusted_impact_binary:= ifelse(adj_impact_class == 'high',1,0)]
# # bills_by_prem_rank[,adjusted_impact_binary_alt:= ifelse(adj_impact_class_alt == 'high',1,0)]
# # 
# # bills_by_prem_rank[adj_impact_class_alt == adj_impact_class,]
# bills_by_prem_rank[prem_id %in% test_prem,.(model_type
#                                             , prem_id
#                                             , diff_pct_temperature_normal
#                                             , rank
#                                             , adjusted_impact_class_rank
#                                             , adj_impact_class
#                                             , impact_temperature_normal)][order(adjusted_impact_class_rank)]
# #
# metrics <- bills_by_prem_rank[,.(TP = sum(adjusted_impact_binary == 1 & binary_rank_ACTUAL == 1),
#                          FN = sum(adjusted_impact_binary == 0 & binary_rank_ACTUAL == 1),
#                          FP = sum(adjusted_impact_binary == 1 & binary_rank_ACTUAL == 0),
#                          TN = sum(adjusted_impact_binary == 0 & binary_rank_ACTUAL == 0)
# ), by = .(model_type, sample, month, deriv_baseline_terr_cd)]
# # 
# metrics[, `:=`(TP_rate = round(TP / (TP + FN),2), FP_rate = round(FP / (FP + TN),2)),]
# metrics[, am := round(TP_rate * (1 - FP_rate),2)][,am_type:='std']
# # 
# # 
# # # cons_usg_metrics <- copy(metrics)
# # # temp_metrics <- rbind(metrics_alt, metrics)
# # cons_metrics <- rbind(metrics_alt, metrics)
# # metrics <- rbind(cons_metrics, temp_metrics)
# # 
# # # metrics_all <- rbind(temp_metrics, cons_metrics)
# # ggplot(data = melt.data.table(metrics,
# #                               , measure.vars = c('TP_rate','FP_rate','am')),
# #        aes(x = model_type, y = value, fill = am_type)) +
# #   facet_grid(variable~sample) +
# #   geom_bar(stat = 'identity', position = 'dodge') +
# #   geom_text(aes(label = value), position = position_dodge(width = 1)) +
# #   scale_fill_discrete('AM ranking type',
# #                       labels = c('% change rank all predictions'
# #                                  ,'% change rank within impact classes')) +
# #   ggtitle('Comparing two ranking methods for AM scores')
