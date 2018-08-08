# # multi model practice --------------------------------------------------
# http://r4ds.had.co.nz/many-models.html

install.packages("gapminder")
library(modelr)
library(gapminder)
library(tidyverse)
data("gapminder")
# library(tidyverse)
# library(devtools)
# library(purrr)
# library(ggplot2)
# devtools::install_github('hadley/modelr')
# install.packages('purrr')
install.packages("datapasta")
library
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)


ia <- filter(gapminder, country %in% c("India",'Canada','United States','New Zealand'))

ia %>%
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(color = country)) +
  ggtitle("Full data = ")

ia_mod <- lm(lifeExp ~ year, data = ia)

ia %>%
  add_predictions(ia_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  facet_wrap(~country)+
  ggtitle("Linear trend + with all countries")

ia %>%
  add_residuals(ia_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line() +
  facet_wrap(~country)+
  ggtitle("Remaining pattern")


# nested data
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

# Fit Models
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country


by_country %>% filter(country %in% c("India",'Canada','United States','New Zealand'))




# Broom -------------------------------------------------------------------

# What can we do with a list of linear models
# turn models into tidy data frames

# model quality; model complexity; adjusted R2, AIC, BIC: glance
# model parameters and associated statistics: tidy
# predicted, actual, residuals: augment
# nest and unnest()

by_country <- by_country %>%
  mutate(
    glance = model %>% map(broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    tidy = model %>% map(broom::tidy),
    augment = model %>% map(broom::augment)
  )

by_country %>% arrange(desc(rsq))
by_country %>%
  filter(country %in% c("India",'Canada','United States','New Zealand') | continent == 'Africa') %>%
  ggplot(aes(rsq, reorder(country, rsq))) +
  geom_point(aes(colour = continent))



# unnesting ---------------------------------------------------------------

by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )

resids <- unnest(by_country, resids)

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)


resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continent)




# looking in depth --------------------------------------------------------
# the r squared for single model on multiple countries
broom::glance(ia_mod)

# all countries
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)
glance

glance %>%
  arrange(r.squared)


glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)


bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

# what happened in Rwanda?
# https://en.wikipedia.org/wiki/Rwanda
# search for In 1990, the Rwandan Patriotic Front (RPF)

#> `geom_smooth()` using method = 'gam'

# d <- d %>% mutate(
#   # Fit the models
#   # models = map(train, ~ lm(usg ~ temp, data = .))),
#   models = map(train, usg_temp_lm),
#
#   # extract training model elements
#   tidy_train = map(models, broom::tidy), #broom
#   glance_train = map(models, broom::glance), #broom
#   augment_train = map(models, broom::augment),
#   rsq_train = map_dbl(glance_train, 'r.squared'), # extract from model element to compare to below
#   rsq_check = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), r_squared), #model level
#   rmse_train = map2(augment_train %>% map("usg"), augment_train %>% map(".fitted"), rmse), #model level
#
#   #
#   n = map(train, NROW),
#
#   # Make predictions on test data
#   pred = map2(models, test, predict),
#
#   # Get the error
#   rsq_test = map2(test %>% map("usg_actual"), pred, r_squared),
#   rmse_test = map2(test %>% map("usg_actual"), pred, rmse),
#   msd_test = map2(test %>% map("usg_actual"), pred, msd),
#   # add rmse to both
#   #
#   # # Get the error
#   d %>% mutate(rsq_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), r_squared))
#   d %>% mutate(rsq_cons = test %>% map("usg_actual"))
#
#
#   msd_val_1 = d %>% map2(test %>% map("usg_actual"), train %>% map('usg_actual'), msd),
#
#   msd_cons = d %>% map2(d %>% map('test' %>% map("usg_actual")))
#                         , train %>% map('usg_actual'), msd),
#   rmse_cons = map2(test %>% map("usg_actual"), train %>% map('usg_actual'), rmse))
# mutate(data = d, m = map('usg_actual'))
# models %>% map(d$models, broom::glance)
#
# models



