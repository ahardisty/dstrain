# IN_FILE <- 'E:/CEA_DATA/model_export/shifted_tou_predictions/'
# IN_FILE_LIST_MONTHLY <- dir('E:/CEA_DATA/model_export/shifted_tou_predictions/', pattern = 'monthly', full.names = TRUE)
# OLD_INPUT <- readxl::read_excel('E:/CEA_DATA/billing_data/electric/tiered_billing_results_tou.xlsx') %>% as.data.table()

KEEP_BILLS <- c('conact','conpre', 'tesact', 'tespre', 'traact')
CORRECT_OUT_COLS <- c('sp_id', 'sa_id', 'nrml_yyyymm', 'percentile', 'month', 'baseline_usg_kwh', 
                      'summer_tier_1_kwh', 'winter_tier_1_kwh', 'summer_tier_2_kwh', 
                      'winter_tier_2_kwh', 'summer_tier_3_kwh', 'winter_tier_3_kwh', 
                      'summer_tier_4_kwh', 'winter_tier_4_kwh', 'summer_tier_5_kwh', 
                      'winter_tier_5_kwh', 'tot_usg_kwh', 'Bill Determinant Identifier', 
                      'Scenario Name', 'snapshot_id', 'acct_id', 'all_elec_unit_qty', 
                      'billing_sys_cd', 'bill_days', 'bpp_ind', 'care_ind', 'care_unit_qty', 
                      'care_usg_kwh', 'char_prem_id', 'cmprs_rt_sched_cd', 'cont_da_ind', 
                      'cpp_chg_amt', 'cpp_cr_kwh', 'cpp_kwh', 'cpp_nbr_evt', 'cpp_non_cr_amt', 
                      'cpp_non_cr_kwh', 'cpp_partp_cr_amt', 'cpp_sta', 'crs_typ_cd', 
                      'deriv_baseline_terr_cd', 'deriv_tot_nbr_unit_qty', 'deriv_usg_prfl_cd', 
                      'dwell_unit_disc_amt', 'elec_enduse_cd', 'emp_disc_amt', 'emp_ind', 
                      'esp_cd', 'fera_ind', 'fera_unit_qty', 'fraction_of_summer', 
                      'fran_fee_exmt_cr_amt', 'fran_fee_surchrg_amt', 'gap_ind', 'legislative_disc_ind', 
                      'max_kw', 'max_tier', 'med_allot_qty', 'med_usg_kwh', 'merge_ind', 
                      'metrofi_acct_ind', 'min_bill_ind', 'mtr_cust_chrg_cd', 'nbr_of_months', 
                      'nbr_unit_qty', 'nem_typ_cd', 'net_mtr_ind', 'nrml_end_dt', 'nrml_start_dt', 
                      'pay_plan_ind', 'read_cyc_cd', 'revn_acct_cd', 'revn_amt', 'rt_sched_cd', 
                      'sm_opt_out_sta_cd', 'sm_sta_cd', 'solar_ind', 'split_ind', 'summer_bill_days', 
                      'summer_kw', 'summer_kwh', 'summer_off_pk_hrs', 'summer_off_pk_kw', 
                      'summer_off_pk_kwh', 'summer_off_tier_1_kwh', 'summer_off_tier_2_kwh', 
                      'summer_off_tier_3_kwh', 'summer_off_tier_4_kwh', 'summer_off_tier_5_kwh', 
                      'summer_on_pk_hrs', 'summer_on_pk_kw', 'summer_on_pk_kwh', 'summer_on_tier_1_kwh', 
                      'summer_on_tier_2_kwh', 'summer_on_tier_3_kwh', 'summer_on_tier_4_kwh', 
                      'summer_on_tier_5_kwh', 'summer_partial_pk_hrs', 'summer_partial_pk_kw', 
                      'summer_partial_pk_kwh', 'summer_partial_tier_1_kwh', 'summer_partial_tier_2_kwh', 
                      'summer_partial_tier_3_kwh', 'summer_partial_tier_4_kwh', 'summer_partial_tier_5_kwh', 
                      'summer_revn_amt', 'summer_usage_kwh', 'tier_1_kwh', 'tier_2_kwh', 
                      'tier_3_kwh', 'tier_4_kwh', 'tier_5_kwh', 'tot_nbr_unit_qty', 
                      'tou_ind', 'tou_sum', 'uniq_sa_id', 'vintage_yr', 'winter_bill_days', 
                      'winter_kw', 'winter_kwh', 'winter_off_pk_hrs', 'winter_off_pk_kw', 
                      'winter_off_pk_kwh', 'winter_off_tier_1_kwh', 'winter_off_tier_2_kwh', 
                      'winter_off_tier_3_kwh', 'winter_off_tier_4_kwh', 'winter_off_tier_5_kwh', 
                      'winter_on_pk_hrs', 'winter_on_pk_kw', 'winter_on_pk_kwh', 'winter_on_tier_1_kwh', 
                      'winter_on_tier_2_kwh', 'winter_on_tier_3_kwh', 'winter_on_tier_4_kwh', 
                      'winter_on_tier_5_kwh', 'winter_partial_pk_hrs', 'winter_partial_pk_kw', 
                      'winter_partial_pk_kwh', 'winter_revn_amt', 'winter_partial_tier_1_kwh', 
                      'winter_partial_tier_2_kwh', 'winter_partial_tier_3_kwh', 'winter_partial_tier_4_kwh', 
                      'winter_partial_tier_5_kwh', 'winter_usage_kwh', 'sample')


# Prepare usage simulations for bill calculation --------------------------
OUT_FILE <- 'E:/CEA_DATA/model_export/shifted_tou_predictions/' 
IN_FILE_LIST_DAILY <- dir('E:/CEA_DATA/model_export/shifted_tou_predictions/', pattern = 'daily', full.names = TRUE)
# IN_FILE_LIST_DAILY <- dir('../CEA_DATA/model_export/shifted_tou_predictions/', pattern = 'daily', full.names = TRUE)
SAMPLE_SIZE = 7500
SAMPLE_NUMS <- grep(pattern = SAMPLE_SIZE, x = IN_FILE_LIST_DAILY)
billing_input <- map(.x = IN_FILE_LIST_DAILY[SAMPLE_NUMS], .f = fread, header = TRUE, sep = 'auto'
                     , check.names = TRUE
                     , stringsAsFactors = TRUE
                     , integer64 = 'character'
                     , select = SELECT_COLS
                     , verbose = FALSE)%>% rbindlist()

billing_input[,train_indicator:= ifelse(year == TRAIN_YEAR, 'train'
                                        , ifelse(year == TEST_YEAR & measure_type == 'CONS_USG'
                                                 , 'CONS_USG','test'))]
unique_acct <- as.character(billing_input[,unique(acct_id)])

# LOAD TEMPLATE --------------------------------------------------------

template <- fread('E:/CEA_DATA/billing_data/electric/eua_billing_determinants.txt'
                         , header = TRUE
                         , na.strings = '?'
                         , colClasses = c('sa_id' = 'character'
                                          , 'sp_id' = 'character'
                                          , 'acct_id' = 'character'
                                          , 'char_prem_id' = 'character'
                                          , 'snapshot_id' = 'character'
                                          , 'nrml_yyyymm' = 'character'
                                          , 'nem_typ_cd' = 'factor'
                                          , 'sm_opt_out_sta_cd' = 'factor')
                         , stringsAsFactors = TRUE
                         , integer64 = 'numeric'
                         , verbose = FALSE)

# template <- subset(template, acct_id %in% unique_acct)
# template_save <- copy(template)
# template <- template_save

# PREP BILL INPUT ---------------------------------------------------------  
  # Extract relevant parts of period (CAN THIS BE IMPROVED?)
  billing_input[, period := str_extract(period, '[(summer_)(winter_)]*[(off)|(partial)|(peak)]*')]
  
  # check for 1:1 comparision (accounting for outlier removal in modeling step)
  full_customer <- as.character(billing_input[train_indicator!='CONS_USG',unique(customer)])
  billing_input <- subset(billing_input, customer %in% full_customer)
  # billing_input[,uniqueN(customer), by = .(train_indicator)]

  # Replace from string
  billing_input[, period := str_replace(period, 'peak', 'on')]
  setnames(billing_input, old = 'usage', new = 'actual')
  
  # split to train actual and test actual
  billing_input <- melt(billing_input
                        # , id.vars = c('train_indicator','acct_id', 'prem_id', 'nrml_yyyymm', 'month', 'period','percentile')
                        , measure.vars = c('actual','predicted') # melt variables for variable category
                        , variable.name = 'usage_type' # rename variable category
                        , value.name = 'usage') # rename variable value
  
  billing_input[,sample := tolower(paste0(substr(train_indicator,1,3)
                                          , substr(usage_type, 1,3)))]
  
  bill_calculations <- billing_input[sample %in% c('tespre','conpre')]
  bill_calculations[,mean(usage), by = .(sample, measure_type)]
  
  bill_baseline <- billing_input[sample %in% c('tesact','traact')
                                 & measure_type %in% c('normal_temperature','temperature'),
                                 ][,measure_type := 'model_baseline']
  
  billing_input <- rbind(bill_calculations, bill_baseline)
  # billing_input <- billing_input[customer == '8874612962_6164259859',]
  setkey(billing_input, customer, date)
  
  # Sum to the TOU period monthly level for each prediction, actual and temp scenario
  # Required because of weekend/weekday differences
  billing_input <- billing_input[, lapply(.SD, sum) 
                                 , by = c('sample','year','model_name','train_indicator','customer'
                                          ,'acct_id', 'prem_id','measure_type'
                                 , 'nrml_yyyymm','month', 'period','usage_type')
                                 , .SDcols = c('usage')]
#   ggplot(billing_input, aes(x = usage)) +
#     geom_histogram(aes(fill = measure_type), binwidth = 10) +
#     facet_wrap(~period, ncol = 1)
  # rename to match billing format
  setnames(billing_input, old = c('measure_type'), new = c('percentile'))
  

# MERGE BILLING DATA --------------------------------------------------

  # Get the baseline usage from the template. Loss of rows here(???)
  billing_input <- merge(x = billing_input, 
                         y = template[,.(acct_id, char_prem_id, sa_id, nrml_yyyymm, baseline_usg_kwh)], 
                         by.x = c('acct_id','prem_id','nrml_yyyymm'), 
                         by.y = c('acct_id', 'char_prem_id', 'nrml_yyyymm'))
  
  # billing_input[,lapply(.SD, uniqueN),]

  # Create baselines that are specific to TOU periods for each variable category (actual & predictd)
  hours <- fread('E:/CEA_DATA/billing_data/electric/hours.csv')
  hours[, total_month := sum(N), by = .(year, month)]
  hours[, weight := N / total_month]
  hours[, nrml_yyyymm := as.character(sprintf('%s%02i', year, as.numeric(match(month, month.abb))))]
  hours[, `:=` (year = NULL, month = NULL)]
  
  # Merge baseline info. into main dataframe
  billing_input <- merge(x = billing_input, y = hours, by = c('nrml_yyyymm', 'period'))

  # Create TOU/weighted baseline
  billing_input[, baseline_usg_kwh_wtd := baseline_usg_kwh * weight]
  
  # Determine billing tiers
  billing_input[,`:=`(tier_1_kwh = tier(usage, baseline_usg_kwh_wtd, 0, 1),
                      tier_2_kwh = tier(usage, baseline_usg_kwh_wtd, 1, 1.3),
                      tier_3_kwh = tier(usage, baseline_usg_kwh_wtd, 1.3, 2),
                      tier_4_kwh = tier(usage, baseline_usg_kwh_wtd, 2, 3),
                      tier_5_kwh = tier(usage, baseline_usg_kwh_wtd, 3, 100))]
  
  billing_input[,season := ifelse(as.numeric(match(month, month.abb)) %in% c(4:10),
                                  'summer', 'winter')]
  
  # Remove duplicate account ids
  ## why are there duplicates? ##
  # duplicate_acct_ids <- billing_input[, .N, by = .(acct_id, prem_id, month, percentile, period)][N > 1, unique(acct_id)]
  # billing_input <- billing_input[!(acct_id %in% duplicate_acct_ids)]
  
  # Case into wide format
  form <- train_indicator + sample + prem_id + acct_id + sa_id + month + nrml_yyyymm + usage_type  + percentile + baseline_usg_kwh ~ period
  billing_input <- dcast(data = billing_input, formula = form, value.var = paste0('tier_',1:5,'_kwh'), fill = 0)
  #   
  # Create total column
  make_total <- c(colnames(select(billing_input, dplyr::contains('tier'))))
  billing_input[ , tot_usg_kwh := rowSums(.SD, na.rm = TRUE), .SDcols = make_total]
#   billing_input[percentile %in% c('normal_temperature','model_baseline') 
#                 & nrml_yyyymm == '201508',]
#   
  # Change column names
  new_cols <- unname(sapply(make_total, change_col_name))

  setnames(billing_input, old = make_total, new = new_cols)
  
  # Remove columns that have been filled in from the old template
  template <- template[,.SD, .SDcols =!c(new_cols, 'tot_usg_kwh', 'baseline_usg_kwh')]
  
  gc()
  
  # Delete old information (CAN IT BE IMPROVED? DONT LIKE THE WARNINGS)
  make_blank <- unique(c(colnames(select(template, dplyr::contains('summer'))), 
                         colnames(select(template, dplyr::contains('winter'))), 
                         colnames(select(template, dplyr::contains('tier')))))
  template[,c(make_blank) := as.integer(make_blank), with = FALSE]
  template[is.na(template)] <- '' # NA as blank
  
  # Add new data into the main template
  setkey(billing_input, acct_id, prem_id, nrml_yyyymm)
  setkey(template, acct_id, char_prem_id, nrml_yyyymm)
  billing_input <- merge(x = billing_input 
                         , y = template
#                          , by.x = c('acct_id','prem_id','nrml_yyyymm') 
#                          , by.y = c('acct_id', 'char_prem_id', 'nrml_yyyymm')
                         )
  
  # One last column name change
  # str(billing_input$nrml_yyyymm)
  billing_input[,nrml_yyyymm:=as.numeric(as.character(nrml_yyyymm))] # change to numeric for bill calculator
  # colnames(billing_input)
  setnames(billing_input, old = c('sa_id.x','prem_id'), new = c('sa_id','char_prem_id'))
  billing_input[,sa_id.y:=NULL]
  
  # Change everything to E6 or E6L
  billing_input[, cmprs_rt_sched_cd := ifelse(care_ind == 'Y', 'E6L', 'E6')]
  
  billing_input[percentile %in% c('normal_temperature','model_baseline') 
                                & nrml_yyyymm == '201508',]

  # Check that the column names match up
  
#   missing_cols <- CORRECT_OUT_COLS[!(CORRECT_OUT_COLS %in% colnames(billing_input))]
#   
#   if(length(missing_cols) > 0){
#     # Add missing columns if needed
#     billing_input[, c(missing_cols) := '']
#   }
#   billing_input[,.N, by = .(sample, percentile)]
  # Reorder columns for SAS calculator
  billing_input <- billing_input[, .SD, .SDcols = CORRECT_OUT_COLS]
  
  # return(billing_input)

# # Process files and write output
# samples <- c('sample1', 'sample2', 'sample3')
# climate_zones <- c('w', 't', 'x')
# d <- data.frame()
# for(i in samples){
#   for(j in climate_zones){
#     d_temp <- format_bills(subdir = i, climate_zone = j, master_template)
#     d <- rbind(d, d_temp)
#     print(proc.time() - start_time)
#   }
# }

# read / write file --------------------------------------------------------------
getwd()
  SAMPLE_SIZE <- 7000
  OUT_PATH <- paste0('E:/CEA_DATA/billing_data/electric/',SAMPLE_SIZE)
  OUT_PATH <- paste0('../CEA_DATA/billing_data/electric/',SAMPLE_SIZE)
  
  BILL_TOU_PATH <- paste0(OUT_PATH,'tiered_billing_results_tou','_',SAMPLE_SIZE,'.csv')
  
 write.table(billing_input, BILL_TOU_PATH, row.names = FALSE, quote = FALSE, sep = ';')
#            
# is there a way to FTP to server via r script?
 # rm(billing_input)


