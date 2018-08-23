# This script utilizes tree-based methods to segment 
# customers based on their engagement and participation variables.
# Load the packages

packList <- c("dplyr", "ggplot2", "randomForest", "rpart", "rpart.plot", "caret", "lubridate", "tidyr")


if (length(setdiff(packList, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packList, rownames(installed.packages())))  
}

sapply(packList, library, character.only = TRUE, verbose = FALSE) # load packages for analysis

d <- read.table("./SourceData/FRESNO_DEMOGRAPHICS_SEGMENT.txt", strip.white = TRUE ,
                sep = "\t", stringsAsFactors = FALSE, fill = FALSE, header = TRUE)


du <- read.csv("./SourceData/FRESNO_USAGE_TEMP.txt", 
                         stringsAsFactors = FALSE, header = TRUE, strip.white = TRUE )

# Tidy Demographic and Usage Data ---------------------------------------------------------------

# only unique TENUR_sp_id
d <- d %>% 
  # distinct(TENUR_sp_id) %>% 
  filter(sampleGroup %in% c("1","2")) # sample group 1 and 2

# create dummy variables - PROGR_elec_enduse_cd
da$num <- 1

ee_spread <- spread(data = da[c('PROGR_elec_enduse_cd','num')], 
                    key = PROGR_elec_enduse_cd, value = num, fill = 0)[2:5]
head(ee_spread)
colnames(ee_spread) <- sapply(colnames(ee_spread), 
                              function(x) paste0("PROGR_elec_enduse_cd_",x, sep = ""))

dim(ee_spread)

rs_spread <- spread(data = da[c('RATES_cmprs_rt_sched_cd','num')], 
                    key = RATES_cmprs_rt_sched_cd, value = num, fill = 0)[2:19]
head(rs_spread)
colnames(rs_spread) <- sapply(colnames(rs_spread), 
                              function(x) paste0("RATES_cmprs_rt_sched_cd_",x, sep = ""))


ee_spread[,1:4] <- sapply(ee_spread[,1:4], as.factor)
sapply(ee_spread, class)
da <- cbind(d, ee_spread, rs_spread)
d <- subset(d, select = -c(num))

dateListD <- colnames(select(d, contains("_dt"), contains("_date"))) # date for demo
dateListU <- colnames(select(du, contains("_dt"), contains("_date"))) # date for usage

# ====== IDENTIFY THE TIMES OF DAY FOR GROUPING =========

# apply as.date to date files
du <- du[du$sampleGroup %in% c("1","2"),] #sample group 1 and 2
du[dateListU] <- lapply(du[dateListU], function(x) as.Date(x, format = "%m/%d/%Y"))
du$day <- weekdays(du$usage_date, abbreviate = FALSE)
du$weekday <- ifelse(du$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
du$yr <- format(du$usage_date,"%Y")


# Split usage data by the year
du_train <- du[(du$yr == "2013"),]
du_test <- du[(du$yr == "2014"),]
du_time_day <- du_train %>% 
  group_by(weekday, sp_id, usage_hour) %>% 
  summarize(
    usage = median(usage)
  ) %>% mutate(
    usage = (usage - mean(usage)) / sd(usage) 
  )

# Plot the usage against time of day
ggplot(du_time_day, aes(usage_hour, usage)) + 
  geom_point(position = "jitter", alpha = 1/4) + 
  geom_smooth(size = 1.1, se = FALSE) +
  annotate("rect", xmin = -0.5, xmax = 6, ymin = -2.5, ymax = 2.8, alpha = 0.2) +
  annotate("rect", xmin = 16, xmax = 23.5, ymin = -2.5, ymax = 2.8, alpha = 0.2) +
  facet_wrap(~weekday) + 
  ggtitle("Average Usage Pattern") + 
  xlab("Hour of the Day") + 
  ylab("Standardized Usage")

# Choosing breaks at 6am and 4pm
du$hour_group <- cut(x = du$usage_hour, breaks = c(-1,6,16,24),
                               labels = c("0-6","6-16","16-24"))


# ====== CREATE AND VISUALIZE DECISION TREES =========

# Create aggregate for each hourly group and user
da <- du %>% 
  group_by(yr, hour_group, TENUR_sp_id = sp_id) %>% 
  summarize(
    usage = mean(usage),
    temp = mean(mean_temp)
  )

# merge with in the demographic data

# da <- merge(da, v, by = "sp_id", all.x = TRUE, all.y = FALSE)
da <- left_join(da, d, by = "TENUR_sp_id")


# # code dwelling type 0 or 1
# da_wide <- spread(data = demoData_wide, DWELL_dwg_typ_cd, num, fill = 0)
# head(demoData_wide)
# da$DWELL_dwg_typ_cd
# lapply(da[dwell_List], function(x) summary(factor(x)))

# has child as 0 or 1
da$has_chld <- ifelse(da$CHILD_hsehd_chld_cnt != 0, 1, 0)

# code pool flasg as 0 or 1
da$HOME_swimming_pool_flg <- binary_Lut[da$HOME_swimming_pool_flg]


# Create attribute lists for joined information
dg <- ungroup(da)
colnames(dg)
ageList <- colnames(select(da[-c(1,2)], starts_with("AGE")))
annualList <- colnames(select(da, starts_with("ANNU")))
toNum <- which(colnames(da) %in% annualList)

da[,toNum] <- sapply(da[,toNum], as.numeric)
sapply(da[,toNum], class)

childList <- colnames(select(da[-c(1,2)], starts_with("CHILD")))



dwell_List <- colnames(select(da[-c(1,2)], starts_with("DWELL")))
engagList <- colnames(select(da[-c(1,2)], starts_with("ENGAG")))
fuel_List <- colnames(select(da, starts_with("FUEL")))
fuelNum <- which(colnames)
home_List <- colnames(select(da[-c(1,2)], starts_with("HOME")))
numbeList <- colnames(select(da[-c(1,2)], starts_with("NUMB")))
premiList <- colnames(select(da[-c(1,2)], starts_with("PREM")))
progrList <- colnames(select(da[-c(1,2)], starts_with("PROG")))
rateList <- colnames(select(da[-c(1,2)], starts_with("RATE")))
residList <- colnames(select(da[-c(1,2)], starts_with("RESID")))
tenurList <- colnames(select(da[-c(1,2)], starts_with("TENUR")))

year_List <- colnames(select(da[-c(1,2)], starts_with("YEAR")))

to <- which(colnames(da)%in%engagList)

class(da[,toAge[2]])
class(da[,toAge[1]])
class(da$AGE_G_adlt_age)
numericLIst <- colnames(select(da, ends_with("age"), ends_with("nbr"), ends_with("cnt")))
factorList <- colnames(select(da, matches("cd"), matches("desc")
                              , contains("_id"), contains("hour_group"), 
                              ends_with("_ind")))

toNumeric <- which(colnames(da)%in% c(numericLIst, fuel_List, numbeList))
da[,toNumeric] <- sapply(da[,toNumeric], as.numeric)
sapply(da[,toNumeric], class)

catUn
# create binary look up tables
factor_Lut <- c("H" = 1, "C" = 1, "R" = 0, "U" = 0, " " = 0) # use lookup table for binary variable (homeowner O,1)
binary_Lut <- c("Y" = 1, "U" = 0, "N" = "0", " " = 0) #
binary_Lut <- c("Y" = 1, "N" = 0)
#
missing_Lut <- c("?" = " " )

# variables to factors
da[,factorList] <- sapply(da[factorList], as.factor)
summary(da[factorList])


da$DWELL_dwg_typ_cd

# engagment binary
# da$ENGAG_pty_email <- factor(ifelse(test = da$ENGAG_pty_email == "?", 0,1))
da[,engagList] <- sapply(da[engagList], function(x) ifelse(x == "?", "N",
                                                                   ifelse(x == "N", "N", "Y")))

da[,engagList] <- sapply(da[engagList], function(x) ifelse(x == "0", 0, 1))

summary(da[,engagList])

sapply
da[engagList]

da['ENGAG_pty_email'] <- ifelse(da['ENGAG_pty_email'] == "?", "N", "Y")

sapply(da[engagList], function(x) summary(factor(x)))

# program variables
sapply(da[progrList], function(x) summary(factor(x)))

# Rate variables
sapply(da[rateList], function(x) summary(factor(x)))


# home variables
home_List

da$HOME_hm_ac_typ_dsc <- ifelse(da$HOME_hm_ac_typ_dsc == ".", "Other", da$HOME_hm_ac_typ_dsc)

da[c('HOME_hm_ac_typ_dsc','HOME_hm_ac_typ_dsc')]

sapply(da[home_List], function(x) round(prop.table(table((x))),digits = 2))


# Format variables (Warning messages are OK)
da$segment <- 100

# Split data again by the year
d_train <- da[(da$yr == 2013),]
d_test <- da[(da$yr == 2014),]

# Select columns for inclusion
dput(colnames(da[progrList]))
dput(colnames(da[rateList]))
dput(colnames(da[engagList]))
dput(PROG_INCLUDE)

PROG_INCLUDE <- c(
  # "PROGR_elec_enduse_cd"
  "PROGR_emp_ind"
  , "PROGR_esap_typ_cd"
  # , "PROGR_mtr_rdg_rte_cd"
  # , "PROGR_net_mtr_ind"
  # , "PROGR_sm_endDate"
  # , "PROGR_sm_opt_out_end_dt"
  # , "PROGR_sm_opt_out_sta_cd"
  # , "PROGR_sm_opt_out_start_dt"
  , "PROGR_sm_pgm_cd"
  # , "PROGR_sm_startDate"
  , "PROGR_smart_ac_ind"
  , "PROGR_solar_ind"
  , "PROGR_elec_enduse_cd_H"
  , "PROGR_elec_enduse_cd_M"
  , "PROGR_elec_enduse_cd_S"
  )

RATE_INCLUDE <- c(
  # "RATE_nem_typ_cd"
  # , "RATE_revn_acct_cd"
  # , "RATE_sa_typ_cd"
   # "RATES_cmprs_rt_sched_cd"
  # , "RATES_deriv_rt_sched_cd"
  # , "RATES_deriv_usg_prfl_cd"
  # , "RATES_svc_typ_cd" 
   "RATES_cmprs_rt_sched_cd_E1L"
  , "RATES_cmprs_rt_sched_cd_E6"
  , "RATES_cmprs_rt_sched_cd_E7"
  , "RATES_cmprs_rt_sched_cd_E7L"
  , "RATES_cmprs_rt_sched_cd_E8"
  , "RATES_cmprs_rt_sched_cd_E8L"
  , "RATES_cmprs_rt_sched_cd_EVB"
  )


keepList <- c("AGE_G_adlt_age","ANNUA_avg_scorex_plus_nbr", 
              "ANNUA_estmtd_hsehd_incm", "CHILD_hsehd_chld_cnt",
              "ENGAG_care_ind", "ENGAG_fera_ind", "ENGAG_my_ener_enrl_ind", 
              "ENGAG_pay_plan_ind", "ENGAG_pty_email", "FUEL_coal_heat_pct", 
              "FUEL_elec_heat_pct", "FUEL_kerosene_oil_heat_pct", 
              "FUEL_lp_gas_heat_pct", "FUEL_othr_heat_pct", "FUEL_solar_heat_pct", 
              "FUEL_util_gas_heat_pct", "FUEL_wood_heat_pct", "HOME_hm_ac_typ_cd", 
              "HOME_hm_ac_typ_dsc", "HOME_hm_htg_typ_cd", "HOME_hm_htg_typ_dsc", 
              "HOME_swimming_pool_flg", "HOME_bathrooms_nbr", "HOME_bedrooms_nbr",
              "HOME_hm_bas_sq_ft_nbr", "HOME_rooms_nbr", "HOME_stories_nbr", 
              "NUMBE_hsehd_adlt_cnt", "PREMI_Built_Medn_Yr", "PREMI_cec_climate_zone_cd", "PREMI_cec_climate_zone_desc", 
              "PREMI_census_blk_grp", "PREMI_cty", "PREMI_Hh_Cnt", "PREMI_Hsg_Unit_Cnt", 
              "PREMI_Hsg_Unit_Ocpy_Cnt", "PREMI_Hsg_Unit_Owner_Ocpy_Cnt", "PREMI_Hsg_Unit_Rntr_Ocpy_Cnt", 
              "PREMI_Hsg_Unit_Vcnt_Cnt", "PREMI_Incm_Hh_Medn_Amt", "PREMI_Incm_Hh_Tot_Amt", 
              "PREMI_Incm_Per_Cap_Amt", "PREMI_opr_area_cd", "PREMI_Val_100K_149K_Cnt", 
              "PREMI_Val_150K_199K_Cnt", "PREMI_Val_200K_299K_Cnt", "PREMI_Val_300K_499K_Cnt", 
              "PREMI_Val_500K_999K_Cnt", "PREMI_Val_50K_99K_Cnt", "PREMI_Val_Over_1M_Cnt", 
              "PREMI_Val_Und_50K_Cnt", "PREMI_wea_stn_cd", "PREMI_zip_cd_12",
              "PROGR_elec_enduse_cd", "PROGR_esap_typ_cd", "PROGR_smart_ac_ind", 
              "PROGR_solar_ind", "RATE_sa_typ_cd", "RATES_cmprs_rt_sched_cd", 
              "RATES_deriv_rt_sched_cd", "RATES_deriv_usg_prfl_cd", "RESID_homeowner_ind",
              "TENUR_sp_id", "TENUR_acct_id", "TENUR_duration", "TENUR_prem_id",
              "TENUR_sa_id", "YEAR_hse_age", "YEAR_yr_built")
              
              c("ageList", "annualList", "childList", "dwell_List", "engagList", 
                "fuel_List", "home_List", "listList", "numbeList", "premiList", 
                "progrList", "rateList", "residList", "tenurList", "year_List"
              )

sapply(da[progrList], function(x) summary(factor(x)))  

summary(da$PREMI_Built_Medn_Yr)
  
str(listList)

cols <- c("usage", engagList, progrList[progrList%in%PROG_INCLUDE], 
          rateList[rateList%in%RATE_INCLUDE])

# check summary of without usage
lapply(da[cols][-1], function(x) round(prop.table(table((x))),digits = 3))
lapply(da[home_List], function(x) round(prop.table(table((x))),digits = 2))

lab <- c("0-6", "6-16", "16-24")

ct_list <- list()
for (i in 1:3){
  
  # Set subsetting condition
  cond_train <- d_train$hour_group == lab[i]
  cond_test <- d_test$hour_group == lab[i]

  # Fit Tree
  ct <- rpart(usage ~., d_train[cond_train, cols], 
              control = rpart.control(maxdepth = 4, 
                                      minbucket = 15))

  
  # Save the trees in a list
  ct_list[[i+1]] <- ct
  
  # Assign group membership
  ct$frame$target <- ct$frame$yval
  ct$frame$yval <- as.numeric(row.names(ct$frame))
  d_train[cond_train,]$segment <- predict(ct, d_train[cond_train,])
  d_test[cond_test,]$segment <- predict(ct, d_test[cond_test,cols])

  }


# Plot The Trees
for (i in 1:3){
  prp(ct_list[[i+1]], extra = 1, type = 2, left = FALSE, varlen = 0, nn=TRUE)
  title(lab[i], cex=.3, line = 0, adj = 0)
  }

# ====== PLOT USAGE VS. TEMPERATURE FOR EACH GROUPING =========

# Create dataframe with sp_id to usage_hour-segment groupings
colnames(d_train)
segments <- unique(d_train[,c("TENUR_sp_id", "hour_group", "segment")])
# segments <- read.csv(paste0(fl, "tree_segments.csv"))
write.csv(segments, "tree_segments.csv", row.names = FALSE)

# Assign each data point a segment
d <- merge(d, segments , by = c("sp_id", "hour_group"), all = TRUE)

# Split data again by the year
d_train <- d[(d$yr == 2013),]
d_test <- d[(d$yr == 2014),]

# Create predictions for each segment and hour group
fit <- dlply(d_train, c("hour_group", "segment"), function(x) 
  earth(usage ~ mean_temp, data = x, Scale.y = FALSE))
d_test <- ddply(d_test, c("hour_group", "segment"), function(x) 
  transform(x, yHat = predict(fit[[paste(x[1,c("hour_group", "segment")], collapse=".")]], 
                              newdata = x)))

# Create charts for the estimated lines for each group
ggplot(d_test, aes(mean_temp, 
               usage.1,
               group = factor(segment),
               colour =factor(hour_group))) + 
  geom_line(size = .75) +
  facet_wrap(~usage_hour, ncol = 4) + 
  scale_y_continuous("Predicted Electricity Usage") + 
  scale_x_continuous("Temperature") + 
  theme(legend.title=element_blank()) + 
  ggtitle("Estimated Electricity Usage and Temperature By Customer Segment And Time Of Day")

# ====== COMPUTE RF IMPORTANCE AND PLOT BY HOUR OF DAY ========
lab <- c("0-6", "6-16", "16-24")
scores_rf <- data.frame()
  for (j in 1:3){
    cond <- (da$hour_group == lab[j]) 
    rf <- randomForest(usage ~., da[cond, cols], na.action = na.omit)
    tmp <- data.frame(imp = unname(rf$importance),
                      hour_group = lab[j],
                      vars = row.names(rf$importance))
    scores_rf <- rbind(scores_rf, tmp)
  }  

da$PROGR_elec_enduse_cd_S <- as.factor(da$PROGR_elec_enduse_cd_S)

summary(da[cond, cols])
da[progrList] <- as.factor(da[progrList][1:7])

scores_rf$

# Format the hour group variable
scores_rf$hour_group <- factor(scores_rf$hour_group, 
                               levels = c("0-6", "6-16", "16-24"),
                               labels = c("Morning", "Day", "Evening"))

# Visualize Importance Scores
ggplot(scores_rf, aes(imp, vars, color = hour_group)) +
  geom_point(size = 4) + 
  xlab("Importance Scores") + 
  ylab("Variables") + 
  geom_line(aes(group = hour_group), stat="vline", xintercept="mean", size = 0.9) + 
  # facet_grid(~year) + 
  ggtitle("Variable Importance Scores for Random Forest")
