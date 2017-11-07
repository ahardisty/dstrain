# complex relationships
# library(tidyverse)
library(tidyverse)
load("data/TRAIN.rda")
load("data/TEST.rda")
load("data/CONS_USG.rda")



# Electricity Usage -------------------------------------------------------

ggplot(TRAIN, aes(x = X, y = Y)) +
  geom_jitter() +
  geom_smooth(method = 'lm',aes(color = season)) +
  facet_wrap(~week_day) +
  ggtitle(label = 'exploring relationship betwen temp and electricity usage',subtitle = "Southern Hemisphere") +
  labs(y = "electricty (kWh)", x = "Temp F")

example(earth)
churn_data$first_crm_mrr_band

# Churn Data --------------------------------------------------------------

churn_data <- fread('data/ah_chi_tenure_lag.csv')

ggplot(churn_data %>% filter(cum_gross_mrr_usd <= 200000), aes(x = total_customer_days, y  = cum_gross_mrr_usd)) +
  geom_jitter()+
  scale_y_continuous( labels = scales::comma) +
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2200, by = 90)) +
  ggtitle(label = 'exploring relationship betwen tenure and net mrr',subtitle = "SaaS customers with less than 200,000 monthly revenue")

ggplot(churn_data %>% filter(cum_gross_mrr_usd <= 200000), aes(x = total_customer_days, y  = cum_gross_mrr_usd)) +
  geom_jitter()+
  scale_y_continuous( labels = scales::comma) +
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2200, by = 90)) +
  # geom_smooth(aes(color = first_crm_mrr_band), method = 'loess') +
  geom_smooth(aes(color = first_crm_mrr_band), method = 'gam', se = F) +
  ggtitle(label = 'exploring relationship betwen tenure and net mrr',subtitle = "SaaS customers with less than 200,000 monthly revenue")

example(geom_smooth)
