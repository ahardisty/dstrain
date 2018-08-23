# EDA


# visualize temp differences in weather stations --------------------------
# question: how does five year average daily temp compare to normal and actual for a given day

ggplot(data = weather_five %>% 
         filter(train_indicator == 'TEST' & climate_zone_cd == 'W' & month_name %in% c('Dec','Jan','Mar')) %>% 
         group_by(train_indicator, climate_zone_cd, month_day, month_name, normal_hourly_temperature, temp_type) %>%
         summarize(temp = mean(temp)), aes(x = temp_type, y = temp))+
         geom_boxplot(aes(fill = temp_type)) +
  geom_hline(aes(yintercept = normal_hourly_temperature, linetype = factor(train_indicator))) +
  scale_linetype_discrete("Baseline Forecast",labels = 'normal temp for test year') +
  facet_wrap(~month_name, ncol = 1) +
  labs(x = 'Temp Measure', y = "Average Daily Temp")+
  scale_fill_discrete("Lag Types",
                      labels = c("Mean temp: year-2:1"
                                 , 'Mean temp: year-5:3'
                                 , 'Max of temp: year-5:1'
                                 , 'Min of temp: year-5:1'
                                 , 'Median of temp: year-5:1'
                                 , 'Actual temp: year-0'))



# QUESTION: Are there material differences in the temperature data between weather stations
# in operating areas?
gc()
# find operating area codes with above average weather or high standard deviation
sample_opr <- weather[month_name == 'Dec'
                      & type == 'Temperature',.(sd = lapply(.SD, sd), .N)
                      , by = .(opr_area_cd, type)
                      , .SDcols = 'val'][sd > 10, opr_area_cd ] 

ggplot(weather[opr_area_cd %in% sample_opr
               # & month_name == 'Dec'
               # & year == '2013'
               & opr_area_cd == 'BR'
               & type == 'Temperature',lapply(.SD, mean)
               , by = .(year, type, hr), .SDcols = 'val']
       , aes(x = hr, y = val)) +
  geom_path(aes(color = factor(year))) +
  scale_color_discrete('Year')  +
  scale_x_continuous(breaks = seq(0,23, by = 1)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle('Hourly Temperature for a full year')

#---- hourly_trends ----

ggplot(usage_weather_hourly[climate_zone_cd == 'W'
               # & month_name == 'Dec'
               & season == 'winter'
               & year != '2015'
               & type == 'Temperature',lapply(.SD, mean)
               , by = .(year, month_name, type, hr), .SDcols = 'val']
       , aes(x = hr, y = val)) +
  geom_path(aes(color = factor(year))) +
  scale_color_discrete('Year')  +
  facet_wrap(~month_name, ncol = 2) +
  scale_x_continuous(breaks = seq(0,23, by = 3)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle('Comparing hourly temperature during winter months')

#---- test-b ----
  
# CONCLUSION: no visible material diffrences between weather stations within 
# operating codes and climate zone codes
# ; will use mean of temperature in operating areas,
# eliminate weather station and use climate zone code for aggregation;
# operating_area codes may be the best level of granularity

# Year over year differences from normal to actual ------------------------
# Are there periods of time year over year with differences between normal and 
# actual temperature

# QUESTION: what variations exist daily by month year over year?
setkey(weather_oac_wide, date, hr, climiate_zone_cd, c(key(weather_oac_wide),month_name))
ggplot(weather_oac_wide[month_name == 'Dec'
                        & climate_zone_cd == 'W'
                        # & opr_area_cd == 'TC'
                        & date == '2013-12-01'
                        & year == '2013',lapply(.SD, mean)
                        , by = .(date, year, month_name, below_normal
                                 , opr_area_cd)
                        , .SDcols = c('temp_normal', 'temp_actual')], aes(x = hr, y = temp_diff)) +
  geom_line(aes(color = opr_area_cd)) +
  facet_wrap(~climate_zone_cd) +
  scale_x_continuous(breaks = seq(min(weather_oac_wide$hr)
                                  , max(weather_oac_wide$hr), by = 1)) +
  scale_fill_discrete(guide = guide_legend(title = 'Actual Temp Indicator')
                      , labels = c('Above Normal', "Below Normal")) +
  coord_cartesian(xlim = c(0, 23), expand = FALSE) +
  ylab('Temperature Difference (F)') + 
  theme(axis.text.x = element_text(size = 8, angle = 30, hjust = 1)) +
  xlab('Hour of Day (0-23)') +
  ggtitle('Hourly variations between actual \nand normal temp by month')

ggplot(weather_oac_wide, aes(x = month_day, y = temp_diff
                             , fill = below_normal)) +
  geom_bar(stat = 'identity', position = 'identity') +
  facet_grid(year~month_name) +
  scale_x_continuous(breaks = seq(min(weather_oac_wide$month_day)
                                  , max(weather_oac_wide$month_day), by = 15)) +
  scale_fill_discrete(guide = guide_legend(title = 'Actual Temp Indicator')
                      , labels = c('Above Normal', "Below Normal")) +
  coord_cartesian(xlim = c(1, 31), expand = FALSE) +
  ylab('Temperature Difference (F)') + 
  theme(axis.text.x = element_text(size = 8, angle = 30, hjust = 1)) +
  xlab('Day of month (1-31)') +
  ggtitle('Daily variations between actual \nand normal temp by month')
# ANSWER: 2014 has the most days with above normal temperature; 
# The winter of 2014 experiences a large number of above normal temperatures
# ANSWER: these typically occur at the beginning and end of each month due to the 
# normal temp calculations

# what differences exist monthly, year over year, within each climate code zone
ggplot(weather_oac_wide, aes(x = as.factor(month), y = temp_diff, fill = below_normal)) +
  geom_bar(stat = 'identity', position = 'identity') +
  theme(axis.text.x = element_text(size = 5)) +
  facet_grid(year~climate_zone_cd) +
  scale_fill_discrete(guide = guide_legend(title = 'Actual Temp Indicator')
                      , labels = c('Above Forecast', "Below Forecast")) +
  ylab('Temp difference\nfrom normal') +
  xlab('Month') +
  ggtitle('Differences between actual \nand normal temp by month')
# ANSWER: On a monthly basis within each climate zone code 2014 was above normal temp

# what differences exist daily, year over year, within each climate code zone
ggplot(weather_oac_wide, aes(x = as.factor(month_day), y = temp_diff, fill = below_normal)) +
  geom_bar(stat = 'identity', position = 'identity') +
  theme(axis.text.x = element_text(size = 5)) +
  scale_x_discrete(breaks = seq(min(weather_oac_wide$month_day)
                                  , max(weather_oac_wide$month_day), by = 15)) +
  facet_grid(year~climate_zone_cd) +
  scale_fill_discrete(guide = guide_legend(title = 'Actual Temp Indicator')
                      , labels = c('Above Forecast', "Below Forecast")) +
  ylab('Temp difference\nfrom normal') +
  xlab('Day of month') +
  ggtitle('Differences between actual \nand normal temp by day')

# ANSWER: No significant difference from aggregated data. Discrepencies between
# temp occur at beginning and end of month in climate zones

# QUESTION; What are hourly differences

ggplot(weather_oac[climate_zone_cd == 'W'
                   & month_name == 'Dec', lapply(.SD, mean), by = .(season
, hr, type, year)
,.SDcols = 'val'], aes(x = hr, y = val)) +
geom_path(aes(color = type)) +
facet_grid(year~season) +
scale_color_discrete('Temperature Type') +
scale_x_continuous(breaks = seq(min(weather_oac$hr),max(weather_oac$hr))) +
theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
ggtitle('Hourly Temperature Averages \nacross year and season')

# ggplot(usage_weather_hourly, aes(x = val, y = usage)) +
#   geom_path(aes(color = type)) +
#   facet_grid(year~season) +
#   scale_color_discrete('Temperature Type') +
#   scale_x_continuous(breaks = seq(min(weather_oac$hr),max(weather_oac$hr))) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ggtitle('Hourly Temperature Averages \nacross year and season')
# 
# 

ggplot(weather_oac_wide[climate_zone_cd == 'W'
                        & month_name == 'Dec',]
       , aes(x = as.factor(hr), fill = below_normal)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 5)) +
  facet_grid(year~season) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle('Hourly Temperature Differences')

ggplot(weather_oac_wide[climate_zone_cd == 'W'
                        & month_name == 'Dec',]
       , aes(x = as.factor(hr), y = temp_diff)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(size = 5)) +
  facet_grid(year~season) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle('Hourly Temperature Differences')


# Distribution for weather and usage -----------------------------------------------------
# QUESTION: What is the relationship between weather and usage?
wea_agg <- interval_weather[prem_id %in% prem_sample
                            & year != '2015'
                            & type %in% c('Cloud Cover', 'Dewpoint', 'Relative Humidity',
                                          'Temperature','Wind Speed')
                            , lapply(.SD, mean, na.rm = TRUE)
                            , by = .(climate_zone_cd, opr_area_cd
                                     ,date, year, month_name, type)
                            , .SDcols = c('usage', 'avg_val')]

# QUESTION: How does weather vary over time?
ggplot(wea_agg[,lapply(.SD, mean, na.rm = TRUE), by = .(date, type)
               , .SDcols = c('usage', 'avg_val')]
       , aes(x = date, y = avg_val, color = type)) +
  geom_line() +
  facet_grid(type~., scales = 'free_y') +
  scale_x_date(date_breaks = 'month', date_labels = '%b') +
  coord_cartesian(xlim = c(min(wea_agg$date),max(wea_agg$date)), expand = FALSE) +
  ylab('Average weather value') + 
  xlab('Month') +
  guides(color = FALSE) +
  ggtitle('Weather patterns over time\nacross all Climate Zones')
# ANSWER: tempurature varies with season, windspeed, relative humidity and dewpoint
# show some seasonal patterns; 
# wind speed and cloud cover have slight seasonal patterns with extreme variations
# likely due to differences within climate code zones

# QUESTION: How does usage vary over time across climate zone codes
ggplot(wea_agg[,lapply(.SD, mean, na.rm = TRUE)
               , by = .(climate_zone_cd, opr_area_cd, date)
               , .SDcols = c('usage', 'avg_val')]
       , aes(x = date, y = usage, color = opr_area_cd)) +
  geom_line() +
  facet_grid(climate_zone_cd~., scales = 'free_y') +
  scale_x_date(date_breaks = 'month', date_labels = '%b-%y') +
  coord_cartesian(xlim = c(min(wea_agg$date),max(wea_agg$date)), expand = FALSE
                  , ylim = c(0,10))+
  ylab('Average daily gas usage') + 
  xlab('Month') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(color = FALSE) +
  ggtitle('Usage variations within Climate Zones')
# ANSWER: usage follows seasonal patterns with an expected increase between
# November and May; Zones P, R, W, V, Y  have consistent usage within terr codes
# Zones S, T, X have visible variations between territory codes

# QUESTION: How does temperature vary over time within climate codes
ggplot(wea_agg[type == 'Temperature', lapply(.SD, mean, na.rm = TRUE)
               , by = .(climate_zone_cd, opr_area_cd, date, type)
               , .SDcols = c('usage', 'avg_val')]
       , aes(x = date, y = avg_val, color = opr_area_cd)) +
  geom_line() +
  facet_grid(climate_zone_cd~., scales = 'free_y') +
  scale_x_date(date_breaks = 'month', date_labels = '%b-%y') +
  coord_cartesian(xlim = c(min(wea_agg$date),max(wea_agg$date)), expand = FALSE) +
  ylab('Average daily temperature') + 
  xlab('Month') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(color = FALSE) +
  ggtitle('Temperature variations within Climate Zones')
# ANSWER: temperature follows seasonal patterns with an expected increase between
# November and May;
# Zones S, T, X have visible variations between territory codes

# QUESTION: What is the relationship between weather and usage in winter months
ggplot(wea_agg[usage < 20,]
       , aes(x = avg_val, y = usage, color = type)) +
  # geom_jitter(alpha = .05) +
  geom_smooth() +
  facet_grid(type~season) +
  ylab('Gas Usage') + 
  xlab('Weather Value') +
  scale_color_discrete('Weather Type')

ggplot(wea_agg[usage < 20,]
       , aes(x = avg_val, y = usage, color = type)) +
  # geom_jitter(alpha = .05) +
  geom_smooth() +
  facet_grid(climate_zone_cd~season) +
  ylab('Gas Usage') + 
  xlab('Weather Value') +
  scale_color_discrete('Weather Type')

ggplot(wea_agg[usage < 20,]
       , aes(x = avg_val, y = usage, color = type)) +
  # geom_jitter(alpha = .05) +
  geom_smooth() +
  facet_wrap(~type, scales = 'free', ncol = 1) +
  ylab('Gas Usage') + 
  xlab('Weather Value') +
  scale_color_discrete('Weather Type')



# interval data -----------------------------------------------------------

ggplot(data = interval_weather) +
  geom_point(aes(x = date, y = avg_val, color = opr_area_cd)) +
  facet_grid(.~type, scales = 'free')

# QUESTION how does temperature differ by climate zone code
# year over year

box_temp <- ggplot(interval_weather_wide[season == 'winter'
                                         & year != 2015,]) +
  geom_boxplot(aes(x = month_name, y = temperature, fill = factor(year))) +
  facet_grid(.~climate_zone_cd, scales = 'free_y') +
  theme(axis.text.x = element_text(size = 8)) +
  ggtitle(paste('Year and month comparison for'
                ,format(x = NROW(unique(interval_weather_wide$prem_id))
                        , big.mark = ','), 'customers')) +
  labs(x = 'Month', y = 'Monthly Temperature', fill = 'Year')
box_temp

# ANSWER: for most climate zones 2014 had the largest range of temperatures
# and the highest median temperature

# QUESTION: how does usage differ by climate zone code
# year over year
box_usage <- ggplot(interval_weather_wide[season == 'winter' & year != 2015,]) +
  geom_boxplot(aes(x = month_name, y = usage, fill = factor(year))) +
  facet_grid(.~climate_zone_cd, scales = 'free_y') +
  coord_cartesian(ylim = c(0, 10), expand = FALSE) +
  theme(axis.text.x = element_text(size = 8)) +
  ggtitle(paste('Year and month comparison for'
                ,format(x = NROW(unique(interval_weather_wide$prem_id))
                        , big.mark = ','), 'customers')) +
  labs(x = 'Month', y = 'Monthly Therm Usage', fill = 'Year')
box_usage
# ANSWER: for most climate zones the mean usage for 2014 was below prior years

# QUESTION: what is the density distribution for usage by climate zone code
# year over year
violin_usage <- ggplot(interval_weather_wide[season == 'winter' 
                                             & year != 2015
                                             & usage <=10,]) +
  geom_violin(aes(x = month_name, y = usage, fill = factor(year))) +
  facet_wrap(~climate_zone_cd, scales = 'free', ncol = 2) +
  coord_cartesian(ylim = c(0, 10), expand = TRUE) +
  theme(axis.text.x = element_text(size = 8)) +
  ggtitle(paste('Year and month comparison for'
                ,format(x = NROW(unique(interval_weather_wide$prem_id))
                        , big.mark = ','), 'customers')) +
  labs(x = 'Month', y = 'Monthly Therm Usage', fill = 'Year')
violin_usage
# ANSWER: climate zone y showed the best pattern of higher distribution density
# at lower usage levels; P and V also showed shorter and wider violins indicating
# a higher distribution density at lower usage levels



# Fixed prem variables ----------------------------------------------------
# QUESTION: What is the distribution of home ages across all climate zone codes
# for full 86,922 customers
setkey(DT_Dem, prem_id)


ggplot(data = DT_Dem[!is.na(YEAR_age_house)]) +
  geom_boxplot(aes(x = climate_zone_cd, y = YEAR_age_house, fill = DWELL_dwg_typ_cd)) +
  scale_fill_discrete(guide = guide_legend(title = 'Dwelling Type')
                      , labels = c('Single Family','Apartment'))

# ANSWER: T has the oldest median age housing stock

ggplot(data = DT_Dem[,unique(DT_Dem)][!is.na(YEAR_age_house)]) +
  geom_histogram(aes(x = YEAR_age_house, fill = div_nm ), binwidth = 1) +
  facet_grid(climate_zone_cd~., scales = 'free_y')

ggplot(data = DT_Dem[,unique(DT_Dem)][!is.na(YEAR_age_house)]) +
  geom_freqpoly(aes(x = YEAR_age_house, color = div_nm ), binwidth = 1) +
  facet_grid(climate_zone_cd~., scales = 'free_y')

ggplot(data = DT_Dem[,unique(DT_Dem)][!is.na(YEAR_age_house)]) +
  geom_freqpoly(aes(x = YEAR_age_house, ..density.., color = div_nm ), binwidth = 1) +
  facet_grid(climate_zone_cd~., scales = 'free_y')

ggplot(data = DT_Dem[,unique(DT_Dem)][!is.na(YEAR_age_house) & climate_zone_cd == 'T']) +
  geom_histogram(aes(x = YEAR_age_house, ..density.., fill = div_nm ), binwidth = 1) +
  facet_grid(div_nm~., scales = 'free_y')
# nearly 10 percent of housing stock in San Francisco is 115 years old

ggplot(data = DT_Dem[,unique(DT_Dem)][!is.na(YEAR_age_house) & climate_zone_cd == 'T']) +
  geom_histogram(aes(x = YEAR_age_house, fill = div_nm ), binwidth = 1) +
  facet_grid(div_nm~., scales = 'free_y') +
  guides(fill = FALSE)
# ANSWER: climate zone T appears to have oldest overall housing stock
# with a large distribution at 90 years, ranging to low 100s
# Y and V have newest housing stock with most less than 60 years
# San Francisco and the East Bay heavily skew the age of housing in climate zone T

DT_Dem[!is.na(YEAR_age_house)
       ,.(median = sapply(.SD, median, na.rm = TRUE)
          , max = sapply(.SD, max, na.rm = TRUE)
          , .N)
       ,.SDcols = 'YEAR_age_house'
       , by = .(climate_zone_cd, opr_area_cd, div_nm, city)
       ][N >= 100
         ][order(-N, -median)][,head(.SD, 5), by = .(div_nm)]


# QUESTION: What is distribution of square footage?

ggplot(data = DT_Dem[!is.na(HOME_hm_bas_sq_ft_nbr)]) +
  geom_boxplot(aes(x = climate_zone_cd, y = HOME_hm_bas_sq_ft_nbr
                   , fill = DWELL_dwg_typ_cd )) +
  scale_y_continuous(breaks = seq(min(DT_Dem$HOME_hm_bas_sq_ft_nbr, na.rm = TRUE)
                                  , max(DT_Dem$HOME_hm_bas_sq_ft_nbr, na.rm = TRUE), by = 500))

ggplot(data = DT_Dem[HOME_hm_bas_sq_ft_nbr < 11000 & !is.na(HOME_hm_bas_sq_ft_nbr)]) +
  geom_boxplot(aes(x = climate_zone_cd, y = HOME_hm_bas_sq_ft_nbr
                   , fill = DWELL_dwg_typ_cd )) +
  scale_y_continuous(breaks = seq(min(DT_Dem$HOME_hm_bas_sq_ft_nbr, na.rm = TRUE)
                                  , max(DT_Dem$HOME_hm_bas_sq_ft_nbr, na.rm = TRUE), by = 500))

ggplot(data = DT_Dem[HOME_hm_bas_sq_ft_nbr < 11000 & !is.na(HOME_hm_bas_sq_ft_nbr)]) +
  geom_density(aes(x = HOME_hm_bas_sq_ft_nbr)) +
  facet_wrap(~climate_zone_cd,  ncol = 1, scales = 'free_y')

DT_Dem[HOME_hm_bas_sq_ft_nbr > 11000 & !is.na(HOME_hm_bas_sq_ft_nbr)
       ,.(prem_id, climate_zone_cd
          , HOME_hm_bas_sq_ft_nbr
          , HOME_rooms_nbr
          , HOME_bathrooms_nbr
          , DWELL_prem_typ_cd
          , NUMBE_hsehd_cnt
          , YEAR_age_house
          , HOME_bedrooms_nbr)
       ][order(-HOME_hm_bas_sq_ft_nbr)]
# only 11 homes, in S and T climate zone codes, have more than 11,000 square feet

DT_Dem[!is.na(HOME_hm_bas_sq_ft_nbr)
       ,.(median = as.double(median(HOME_hm_bas_sq_ft_nbr), na.rm = TRUE)
       ,max = as.double(max(HOME_hm_bas_sq_ft_nbr), na.rm = TRUE), .N)
       , by = .(climate_zone_cd, opr_area_cd, div_nm)
       ][order(-max)][,head(.SD, 11)]



# ANSWER: Most homes are below 2500 square feet. Outliers above 11,000 square feet were
# discarded due to data quality and poor representation of overall sample
# In climate zone codes with sq ftg > 11000 the median is 
# CONCLUSION: Will only analyze home with square footage below 11,000 square feet

# FURTHER RESEARCH: HOW TO IMPUTE HOUSE SIZE AND HOUSE AGE AVERAGES BASED ON 
# FIXED / COMMON VARIABLES




# How are house age and gas usage related ---------------------------------
DT_Dem[,.N, by = .(is.na(HOME_hm_bas_sq_ft_nbr))]
DT_Dem[,.N, by = .(is.na(YEAR_age_house))]



# Preliminary Modeling ----------------------------------------------------

interval_sm <- interval_weather_wide[prem_id == '1000447119' 
                                     & usage < 20 & season == 'winter'
                                     ,.(season, usage, normal_temperature
                                        , temperature
                                        , prem_id
                                        , cloud_cover
                                        , dewpoint
                                        , relative_humidity
                                        , climate_zone_cd
                                        , precipitation
                                        , wind_speed)]
gc()
act_temp_lm <- lm(usage ~ temperature, data = interval_sm[,.SD, .SDcols =! 'season'])
norm_temp_lm <- lm(usage ~ normal_temperature, data = interval_sm[,.SD, .SDcols =! 'season'])
summary(act_temp_lm)
plot(act_temp_lm)
summary(norm_temp_lm)


interval_lm_all <- lm(usage ~ ., data = interval_sm[,.SD, .SDcols =! 'season'])
summary(interval_lm_all)


# linear model with 1 year lag
# ggplot(data = usage_weather_daily_wide[year != 2012
#                                        & !is.na(usage)
#                                        & usage < 50
#                                        & prem_id %in% full_prem[1:1000]
#                                        & season == 'winter',]
#        , aes(x = temp_actual_avg, y = usage, color = factor(year))) +
#   facet_grid(month_name~climate_zone_cd) +
#   geom_smooth()

# ggplot(data = usage_weather_daily_wide[year != 2012
#                                        & !is.na(usage)
#                                        & usage < 50
#                                        & prem_id %in% full_prem[1:1000]
#                                        & season == 'winter',]
#        , aes(x = month_day, y = usage, color = factor(year))) +
#   facet_grid(month_name~climate_zone_cd) +
#   geom_smooth()

# ggplot() +
#   geom_smooth(data = weather[!is.na(avg_val)
#                              & season == 'winter'
#                              & year != 2015
#                              & type == 'Daily Average Temperature',]
#               , aes(x = month_day, y = avg_val, color = factor(year))) +
#   geom_smooth(data = weather[!is.na(avg_val)
#                              & season == 'winter'
#                              & year == 2015
#                              & type != 'Daily Average Temperature',]
#               , aes(x = month_day, y = avg_val, linetype = factor(year)),color = 'black'
#               , linetype = 'dotted'
#               , size = 1.5) +
#   facet_grid(month_name~climate_zone_cd)

# distribution of temperature in winter
ggplot(data = weather[season == 'winter' & type == "Temperature",], aes(x = avg_val)) +
  geom_histogram(binwidth = 1.5, aes(fill = train_indicator)) +
  facet_wrap(~month_name) +
  scale_fill_discrete("Sample Type") +
  labs(x = "Average Daily Temperature", y = NULL) +
  ggtitle(paste('Distribution of Average Daily temperature from', 
                min(weather$year),'to',max(weather$year)))

# comparing distributions of train and test temperature data
ggplot(data = weather[season == 'winter' & type == 'Temperature', ]
       , aes(x = avg_val, color = train_indicator, ..density.., linetype = type)) +
  geom_freqpoly(binwidth = 1.5) +
  facet_wrap(~month_name) +
  scale_linetype("Weather Type") +
  scale_color_discrete("Sample Type") +
  labs(x = "Average Daily Temperature", y = 'Density') +
  ggtitle(paste('Distribution of Average Daily temperature from', 
                min(weather$year),'to',max(weather$year)))

ggplot(data = weather[season == 'winter'
                      & type == 'Temperature',]
       , aes(x = avg_val,  ..density..
             , color = train_indicator, linetype = type)) +
  geom_freqpoly(binwidth = 1.5) +
  facet_wrap(~climate_zone_cd) +
  scale_linetype("Weather Type") +
  scale_color_discrete("Sample Type") +
  labs(x = "Average Daily Temperature", y = 'Density') +
  ggtitle(paste('Distribution of Average Daily temperature \nfrom', 
                min(weather$year),'to',max(weather$year),'for all territories'))

# distribution of daily usage in winter with density by month

interval_usage_sm = interval_usage_sm[season == 'winter' 
                                      & prem_id %in% prem_sample
                                      & usage < 20
                                      & !is.na(usage)]
rm(interval_usage)

ggplot(data = interval_usage_sm
       , aes(x = usage, color = train_indicator, ..density..,), linetype = 'usage') +
  geom_freqpoly(binwidth = 1.5) +
  facet_wrap(~month_name) +
  scale_linetype("Usage") +
  scale_color_discrete("Sample Type") +
  labs(x = "Actual Daily Gas Usage", y = 'Density') +
  ggtitle(paste('Distribution of Daily Gas Usage', 
                min(interval_usage$year),'to',max(interval_usage$year),'\nfor'
                , length(prem_sample),'customers in climate code W'))

ggplot(data = weather
       , aes(x = avg_val,  ..density..
             , color = train_indicator)) +
  geom_freqpoly(binwidth = 1.5) +
  facet_grid(climate_zone_cd~season)


