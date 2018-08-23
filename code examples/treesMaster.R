# This script utilizes tree-based methods to segment 
# customers based on their average electricty usage.

# Load the packages
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)

# Clear the working space
rm(list = ls()); gc()

# ====== READ DATA AND FORMAT =========
fl <- "C:/Users/J8DG/Documents/data/"
fn <- "fresno_1_2_new.txt"
d <-read.csv(paste0(fl, fn),sep="\t")
d$usage_date <- as.Date(d$usage_date, format = "%m/%d/%Y")
d$sp_id <- as.factor(d$sp_id)
d$day <- weekdays(d$usage_date)
d$weekday <- ifelse(d$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Split data by the year
d_train <- d[(d$yr == 2013),]
d_test <- d[(d$yr == 2014),]

# ====== IDENTIFY THE TIMES OF DAY FOR GROUPING =========
d_time_day <- d_train %>% 
  group_by(weekday, sp_id, usage_hour) %>% 
  summarize(
    usage = median(usage)
  ) %>% mutate(
    usage = (usage - mean(usage)) / sd(usage) 
  )

# Plot the usage against time of day
ggplot(d_time_day, aes(usage_hour, usage)) + 
  geom_point(position = "jitter", alpha = 1/4) + 
  geom_smooth(size = 1.1, se = FALSE) + 
  annotate("rect", xmin = -0.5, xmax = 6, ymin = -2.5, ymax = 2.8, alpha = 0.2) +
  annotate("rect", xmin = 16, xmax = 23.5, ymin = -2.5, ymax = 2.8, alpha = 0.2) +
  facet_wrap(~weekday) + 
  ggtitle("Averaage Usage Pattern") + 
  xlab("Hour of the Day") + 
  ylab("Standardized Usage")

# Choosing breaks at 6am and 4pm
breaks <- c(6, 16)
d$hour_group <- 0
d$hour_group <- ifelse(d$usage_hour >= breaks[1], 1, d$hour_group)
d$hour_group <- ifelse(d$usage_hour >= breaks[2], 2, d$hour_group)

# ====== CREATE AND VISUALIZE DECISION TREES =========

# Create aggregate for each hourly group and user
da <- d %>% 
  group_by(yr, hour_group, sp_id) %>% 
  summarize(
    usage = mean(usage),
    temp = mean(mean_temp)
  )

# Read in the demographic data
fn <- "fresno_1_2_demographics_new.txt"
v <-read.csv(paste0(fl, fn), sep="\t")
da <- merge(da, v, by = "sp_id", all.x = TRUE, all.y = FALSE)
rm(v)

# Format variables (Warning messages are OK)
da$adlt_age <- as.numeric(as.character(da$adlt_age))
da$hm_bas_sq_ft_nbr <- as.integer(as.character(da$hm_bas_sq_ft_nbr))
da$segment <- 100

# Split data again by the year
d_train <- da[(da$yr == 2013),]
d_test <- da[(da$yr == 2014),]

# Select columns for inclusion
cols = c("usage", "estmtd_hsehd_incm", "stories_nbr", "bathrooms_nbr", "bedrooms_nbr", 
         "rooms_nbr", "swimming_pool_flg", "util_gas_heat_pct", "lp_gas_heat_pct", 
         "elec_heat_pct", "kerosene_oil_heat_pct", "coal_heat_pct", "wood_heat_pct", 
         "solar_heat_pct", "othr_heat_pct", "own_rnt_cd", "dwg_typ_cd", 
         "dwg_typ_dsc", "avg_scorex_plus_nbr", "avg_scorex_plus_dsc", 
         "homeowner_ind", "tot_enhnc_match_cd", "auto_match_cd", 
         "adlt_age_cd", "hm_htg_typ_dsc", "hm_ac_typ_cd", "hm_ac_typ_dsc", 
         "hybrid_car_nbr", "yr_built", "adlt_age", "hsehd_chld_cnt", "hsehd_adlt_cnt", 
         "hm_bas_sq_ft_nbr")

lab <- c("0-6", "6-16", "16-24")
ct_list <- list()
for (i in 0:2){
  
  # Set subsetting condition
  cond_train <- d_train$hour_group == i
  cond_test <- d_test$hour_group == i
  
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
for (i in 0:2){
  prp(ct_list[[i+1]], extra = 1, type = 2, left = FALSE, varlen = 0, nn=TRUE)
  title(i, cex=.3, line = 0, adj = 0)
  }

# ====== PLOT USAGE VS. TEMPERATURE FOR EACH GROUPING =========

# Create dataframe with sp_id to usage_hour-segment groupings
segments <- unique(d_train[,c("sp_id", "hour_group", "segment")])
# segments <- read.csv(paste0(fl, "tree_segments.csv"))
write.csv(segments , paste0(fl, "tree_segments.csv"), row.names = FALSE)

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
scores_rf <- data.frame()
for (y in 13:14){
  for (j in 0:2){
    cond <- (d_agg$hour_group == j) & (d_agg$yr == (2000 + y))
    rf <- randomForest(usage ~., d_agg[cond, cols], na.action = na.omit)
    tmp <- data.frame(imp = unname(rf$importance),
                      hour_group = j,
                      vars = row.names(rf$importance), 
                      year = y)
    scores_rf <- rbind(scores_rf, tmp)
  }  
}

# Format the hour group variable
scores_rf$hour_group <- factor(scores_rf$hour_group, 
                               levels = c(0, 1, 2),
                               labels = c("Morning", "Day", "Evening"))

# Visualize Importance Scores
ggplot(scores_rf, aes(imp, vars, color = hour_group)) +
  geom_point(size = 4) + 
  xlab("Importance Scores") + 
  ylab("Variables") + 
  geom_line(aes(group = hour_group), stat="vline", xintercept="mean", size = 0.9) + 
  facet_grid(~year) + 
  ggtitle("Variable Importance Scores for Random Forest")
