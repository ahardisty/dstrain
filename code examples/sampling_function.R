# full_inteval[,lapply(.SD, uniqueN)]
library(dplyr)
?sample_frac()
by_cyl <- mtcars %>% mutate(carname = rownames(mtcars)) %>% 
  group_by(carname, cyl) %>% 
  select(carname, mpg:carb)

sample_n(mtcars, 10, weight = disp)

sample_n(by_cyl, 3, replace = T)
sample_n(by_cyl, 10, replace = TRUE)
sample_n(by_cyl, 3, weight = mpg / mean(mpg))
sample_frac(mtcars, 0.1)
setkey(full_inteval, weather_station, sp_id, prem_id, acct_id)
unique_key <- full_inteval[,unique(full_inteval)][,.(weather_station, sp_id, prem_id, acct_id)
                                                  ][,(sp_id)]
unique_sum <- unique_key[,.N, by = .(weather_station)
                         ][,proportion := N/sum(N)]


u
sample_one <- sample_frac(unique_key, 0.10)
sample_one <- sample_n(unique_key, 1000)
list(unique_key) %>% map(sample_n,100)

# http://stackoverflow.com/questions/24685421/how-do-you-extract-a-few-random-rows-from-a-data-table-on-the-fly
random.length  <-  sample(x = 15:30, size = 1)
data.table(city = sample(c("Cape Town", "New York", "Pittsburgh", "Tel Aviv", "Amsterdam")
                         ,size=random.length, replace = TRUE)
           , score = sample(x=1:10, size = random.length, replace=TRUE))[sample(.N, 3)] 

data.table(city = sample(c("Cape Town", "New York", "Pittsburgh", "Tel Aviv", "Amsterdam")
                         ,size=random.length, replace = TRUE)
           , score = sample(x=1:3, size = random.length, replace=TRUE))[sample(.N, 5)] 


# http://stackoverflow.com/questions/16289182/how-do-you-sample-random-rows-within-each-group-in-a-data-table
unique_key[,.SD[sample(.N,3)], by = .(weather_station)]

sample_sum <- sample_one[,.N, by = .(weather_station)
                         ][,proportion := N/sum(N)]


sample_groups <- unique_key %>% group_by(weather_station) %>% 
  nest(.key = sample_100) %>% 
  mutate(sample_10 = map(sample_100, sample_frac, .10, replace = TRUE),
         sample_25 = map(sample_100, sample_frac, .25, replace = TRUE),
         sample_50 = map(sample_100, sample_frac, .50, replace = TRUE),
         sample_75 = map(sample_100, sample_frac, .75, replace = TRUE)) %>% 
  select(weather_station, sample_10:sample_75, sample_100)

sample_50 <- sample_groups %>% select(sample_50, weather_station) %>% 
  unnest() %>% filter(weather_station == 'LBFLT') %>% 
  select(sp_id)%>% 
  flatten_chr()

sample_groups <-  unique_key %>% group_by(weather_station) %>% 
  mutate(dist = n_distinct(acct_id)) %>% 
  summarize(dis = n_distinct(dist),
            mean = mean(dist))
sample_groups %>% filter(dist != 1)

  mutate(n = n()) %>% 
  nest(.key = sample_100) %>%
  mutate(n = map(sample_100, summarise(n = n())))
  map(summarize, n = n())
,  #date values
  
  small <- w_train[date %between% c("2014-08-01", "2014-08-31")]


sample_groups %>% select(sample_100) %>% 
  mutate(m = map(sample_100, 'blue'))
unique_key %>% mutate(n = 'blue')

  # spread(key = train_indicator, value = data)%>% 
  mutate(
    models = map(TRAIN, usg_temp_lm), # fit linear model
    augment_train = map(models, broom::augment),
    glance_train = map(models, broom::glance),
    tidy_train = map(models, broom::tidy)#model level
  )
