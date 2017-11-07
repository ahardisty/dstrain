# # multi model practice --------------------------------------------------
# install.packages("gapminder")
library(gapminder)
data("gapminder")
library(tidyverse)
# library(devtools)
# library(purrr)
# library(ggplot2)
# devtools::install_github('hadley/modelr')
# install.packages('purrr')

gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# nested data
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

# Fit Models
country_model <- function(df) {
  lm(lifeExp ~ year1950, data = df)
}

models <- by_country %>% 
  mutate(
    mod_simple = data %>% map(country_model)
  )

models
models %>% filter(continent == 'Africa')


# Broom -------------------------------------------------------------------

# What can we do with a list of linear models
# turn models into tidy data frames

# model quality; model complexity; adjusted R2, AIC, BIC: glance
# model parameters and associated statistics: tidy
# predicted, actual, residuals: augment
# nest and unnest()

models <- models %>% 
  mutate(
    glance = mod_simple %>% map(broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    tidy = mod_simple %>% map(broom::tidy),
    augment = mod_simple %>% map(broom::augment)
  )

d <- d %>% mutate(
  # Fit the models
  # models = map(train, ~ lm(usg ~ temp, data = .))),
  models = map(train, usg_temp_lm),
  
  # extract training model elements
  tidy_train = map(models, broom::tidy), #broom
  glance_train = map(models, broom::glance), #broom
  augment_train = map(models, broom::augment),
  rsq_train = map_dbl(glance_train, 'r.squared'), # extract from model element to compare to below
  rsq_check = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), r_squared), #model level
  rmse_train = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), rmse), #model level
  
  #
  n = map(train, NROW),
  
  # Make predictions on test data
  pred = map2(models, test, predict),
  
  # Get the error
  rsq_test = map2(test %>% map("usg_actual"), pred, r_squared),
  rmse_test = map2(test %>% map("usg_actual"), pred, rmse),
  msd_test = map2(test %>% map("usg_actual"), pred, msd),
  # add rmse to both
  #
  # # Get the error
  d %>% mutate(rsq_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), r_squared))
  d %>% mutate(rsq_cons = test %>% map("usg_actual"))
  
  
  msd_val_1 = d %>% map2(test %>% map("usg_actual"), train %>% map('usg_actual'), msd),
  
  msd_cons = d %>% map2(d %>% map('test' %>% map("usg_actual")))
                        , train %>% map('usg_actual'), msd),
  rmse_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), rmse))
mutate(data = d, m = map('usg_actual'))
models %>% map(d$models, broom::glance)

models

models %>% arrange(desc(rsq))
models %>% 
  ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point(aes(colour = continent))


