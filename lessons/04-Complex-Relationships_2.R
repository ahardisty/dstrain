# complex relationships
# library(tidyverse)
library(tidyverse)
library(data.table)
library(rpart.plot)
library(caret)

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

head(churn_data)

churn_tree <- churn_data %>%
  select(crm_account_id
         , crm_owner_region_clean
         , crm_product_line_count
         , crm_product_line_count_prior
         , crm_product_line_paid_combo
         , crm_product_line_paid_combo_prior
         , crm_multi_product_ind
         , crm_multi_product_ind_prior
         , crm_multi_product_paid_ind
         , crm_multi_product_paid_ind_prior
         , cnt_mrr_date_qtr
         , cnt_reporting_quarters
         , current_state
         , prior_qtd_mrr_diff_type
         , first_crm_mrr_band
         , prior_crm_mrr_band
         , net_mrr = cum_net_mrr_usd
         , sfdc_industry
         , sfdc_employee_range) %>% as.tibble()


head(model.matrix(net_mrr ~ ., data = churn_tree))

make_binary <- dummyVars(net_mrr ~ ., data = churn_tree)

predict(make_binary, newdata = churn_tree)
cbind(head(predict(dummies, newdata = X201609QA[2:7])), head(X201609QA))

predict(dummies, newdata = Sacramento[Sacramento$city == 'RIO_LINDA',])


dummies <- dummyVars(survived ~ ., data = etitanic)




install.packages(c("AppliedPredictiveModeling", "caret", "Cubist",
                   "gbm", "party", "partykit", "randomForest",
                   "rpart", "RWeka"),
                 dependencies = c("Depends", "Suggests"))
library(rpart)
head(churn_tree$net_mrr)
single_tree <- rpart(net_mrr ~., data = churn_tree)
rpart.plot(single_tree)


fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
plot(fit)


rtree_fit <- rpart(survived ~ .,
                   .data$training)
rpart.plot(rtree_fit)
