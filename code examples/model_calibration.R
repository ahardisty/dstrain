# model calibration
MONTH_COLS <- 1:12 # used to create BILL_CYCLE variables and for full 12 month GBC
RENUM_COLS <- 1:5 # used to calculate truncated GBC
MON_NUM_1D <- match(c('Dec','Mar'), month.abb) # month numbers with one digit
MON_NUM_2D <- sprintf('%02d', match(c('Dec','Mar'), month.abb)) # month numbers with two digits
BILL_CYCLE_MODEL <- flatten_chr(map2(list(TEST_YEAR, TRAIN_YEAR), list(MON_NUM_2D),paste0)) # bill cycle with two digits
TIER_PATTERN <- 'REVENUE|TIER-|USAGE|AMOUNT|CARE_UNITS'


# Forecast calibration ----------------------------------------------------

# what was the actual bill impact from 2014 to 2015?
# actual norm bills 2014
# billing determinant baseline amounts data for tier calculations
ba_cols <-col_types('../CEA_DATA/billing_data/', pat = '.billing', n = 5, sep ='\t'
                    , fctr_cols = c('BILL CYCLE','SMARTMETER STATUS CODE','SERVICE AGREEMENT ID'
                                    ,'ACCOUNT ID','PREMISE ID','SERVICE POINT ID','UNIQUE SERVICE AGREEMENT ID'))

BA_COLS <- c(BD_COLS, 'CLIMATE BAND', 'MAXIMUM USAGE TIER', 'REVENUE AMOUNT','WINTER THERMS', 'TIER-1 WINTER THERMS'
             , 'TIER-2 WINTER THERMS', 'TIER-1 THERMS','TIER-2 THERMS', 'USAGE', 'WINTER USAGE', 'NUMBER OF WINTER DAYS')


# read in billing determinants
bill_actual <- rbindlist(map(ba_cols[['files']], function(x){ # read pivot data into list of data tables
  fread(x, header = TRUE
        , strip.white = TRUE
        , check.names = TRUE
        , stringsAsFactors = TRUE
        , integer64 = 'character'
        , verbose = FALSE
        , select = c(BA_COLS)
        , colClasses = ba_cols[['classes']]
        , na.strings = '?')
}[,customer := paste0(`PREMISE ID`,'_',`ACCOUNT ID`) # create customer variable
  ][,`CORE-TRANSPORT FLAG`:=ifelse(`CORE-TRANSPORT FLAG` == 'BUNDLED', 1,0)])) # change transport variable to binary from character

setnames(bill_actual, old = colnames(bill_actual), new = gsub(pattern = ' ', '_', colnames(bill_actual)))

# setkey(bill_actual, customer, PRIOR_READ_DATE) # set key for filter and variable creation

bill_actual[,c('CURRENT_READ_DATE','PRIOR_READ_DATE') := lapply(.SD, function(x) as.IDate(x, format = '%m/%d/%Y'))
            , .SDcols = c('CURRENT_READ_DATE','PRIOR_READ_DATE')]

bill_actual <- bill_actual[BILL_CYCLE %in% BILL_CYCLE_MODEL,] # limit to bill cycles in Dec and Mar
bill_actual[, `:=` (year = str_sub(BILL_CYCLE,1,4) # create bill year
                  , month = str_sub(BILL_CYCLE,5,6))] # create bill month

bill_actual[,train_indicator := ifelse(year == TRAIN_YEAR,'TRAIN' # assign TRAIN / TEST variable
                                     , ifelse(year == TEST_YEAR,'TEST','out of sample')),]


FULL_OBS <- bill_actual[,.N, by = .(CLIMATE_BAND, customer, month, SERVICE_AGREEMENT_ID)][N == 2][,.(customer, SERVICE_AGREEMENT_ID)] # should have this many customers
FULL_CUSTOMER <- FULL_OBS[,unique(customer)]
bill_actual <- bill_actual[customer %in% c(FULL_CUSTOMER),]
BINARY_VARS <- c('NUMBER_OF_CARE_UNITS','MAXIMUM_USAGE_TIER')
TIER_COLS <- grep(pattern = TIER_PATTERN,x = colnames(bill_actual))
COMPARE_COLS <- dput(c(colnames(bill_actual)[TIER_COLS][1:7]))

bill_actual[,CARE_INDICATOR := as.character(CARE_INDICATOR)] # change to 1 and 0
bill_actual[,CARE_INDICATOR := ifelse(CARE_INDICATOR == 'Y', 1, 0)]

bill_actual_melt <- melt.data.table(bill_actual,
                id.vars = c('customer','SERVICE_AGREEMENT_ID','CLIMATE_BAND','PREMISE_ID','year','train_indicator','month','BILL_CYCLE')
                , measure.vars = c(COMPARE_COLS)
                , variable.name = 'bill_variable'
                , value.name = 'variable_value'
                , variable.factor = TRUE
                , value.factor = FALSE
                , verbose = FALSE)

FORM <- customer + SERVICE_AGREEMENT_ID + PREMISE_ID + month + CLIMATE_BAND + bill_variable ~ train_indicator

bill_impact_actual <- dcast.data.table(data = bill_actual_melt
                                       , formula = FORM
                                       , value.var = c('variable_value')
                                       , drop = TRUE,sep = '')



bill_impact_actual[,.N, by = .(bill_variable == BINARY_VARS, bill_variable)]
bill_impact_actual[is.na(TEST), TEST:=0]
bill_impact_actual[is.na(TRAIN), TRAIN:=0]
bill_impact_actual[!bill_variable %in% c(BINARY_VARS) & TRAIN == 0, TRAIN:=.10]
bill_impact_actual[!bill_variable %in% c(BINARY_VARS) & TEST == 0, TEST:=.10]
bill_impact_actual[, diff:= (TEST - TRAIN)]
bill_impact_actual[!bill_variable %in% c(BINARY_VARS), diff_pct:= (diff)/TRAIN]
bill_impact_actual[bill_variable %in% c(BINARY_VARS), diff_pct:= diff,]

# bill_impact_actual[,.N, by = .(is.na(diff_pct),is.na(diff_pct),is.infinite(diff_pct))] # check for missing values

bill_impact_actual[!bill_variable %in% c(BINARY_VARS),diff_break := as.factor(bucket(x = diff, type = 'absolute'))]
bill_impact_actual[!bill_variable %in% c(BINARY_VARS),diff_pct_break := as.factor(bucket(x = diff_pct, type = 'percent'))]

bill_impact_actual[diff == min(diff), diff_break:= levels(bill_impact_actual$diff_break)[[1]],]
bill_impact_actual[diff_pct == min(diff_pct), diff_pct_break:= levels(bill_impact_actual$diff_pct_break)[[1]],]
                   

bill_impact_actual[,.N, by = .(is.na(diff_break),is.na(diff_pct_break), bill_variable)] # check for missing values
bill_impact_actual[bill_variable %in% c(BINARY_VARS),.N, by = .(diff_break, diff),] # check for missing values

bill_impact_actual[bill_variable %in% c(BINARY_VARS) & diff %in%c(2,-2),]

no_usage <- bill_impact_actual[bill_variable == 'USAGE',lapply(.SD, sum)
                   , by = .(customer, month), .SDcols = c('TEST','TRAIN')][TEST <1 & TRAIN <1,]

bill_impact_actual[, impact:= as.factor(get_impact(diff_pct, diff))]


bill_impact_actual[bill_variable %in% c(BINARY_VARS[1])
                   , c('diff_break','diff_pct_break','impact'):= lapply(.SD, function(x) case_when(x < 0 ~ 'Lost CARE',
                                                                                          x == 0 ~ 'No Change',
                                                                                          x > 0 ~ 'Added CARE')),.SDcols = 'diff']

bill_impact_actual[bill_variable %in% c(BINARY_VARS[2])
                   , c('diff_break','diff_pct_break','impact'):= lapply(.SD, function(x) case_when(x < 0 ~ 'Max Tier Decrease',
                                                                                          x == 0 ~ 'No Change',
                                                                                          x > 0 ~ 'Max Tier Increase')),.SDcols = 'diff']

write.csv(x = bill_impact_actual, file = 'actual_impact_csv')



# Load full EUA population ------------------------------------------------

eua_2015 <- f

# plotting results --------------------------------------------------------
m_var <- bill_impact_actual %>% select(customer, SERVICE_AGREEMENT_ID, PREMISE_ID, month
                                       , CLIMATE_BAND, bill_variable, impact) %>% 
  # filter(bill_variable %in% c('REVENUE_AMOUNT')) %>%
  group_by(CLIMATE_BAND, month,bill_variable, impact) %>% 
  summarize(n=n())%>%
  split(list(.$bill_variable, drop = TRUE))

all_plots <- m_var %>% # no filters for comparisions
  map(~ggplot(.,aes(x = impact, y = n, fill = bill_variable)) +
        geom_bar(stat='identity') +
        facet_grid(CLIMATE_BAND~month) + 
        geom_text(aes(label = n), color = 'black') +
        ggtitle(paste('actual','BILL IMPACT for', .$bill_variable,'for Climate Zones T & W')
  ))
all_plots$REVENUE_AMOUNT.TRUE




# visualizing population --------------------------------------------------
rm(interval_usage)
head(pop_tenure)
pop_tenure[,sd(sa_change), by = .(climate_zone_cd)]
pop_tenure[,sd(acct_yrs), by = .(climate_zone_cd)]
pop_tenure[acct_yrs == 1 & climate_zone_cd == 'T',]
pop_tenure[,list(var(sa_change)), by = .(climate_zone_cd)]

pop_t <- subset(pop_tenure, climate_zone_cd == 'T')
ggplot(pop_t)

