library(tidyverse)
library(stringr)
library(EDAWR)


# data sources from github ------------------------------------------------

weather_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/weather.csv'
billboard_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/billboard.csv'
tb_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/tb.csv'
pew_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/pew.csv'
preg_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/preg.csv'
preg2_source <- 'https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/preg2.csv'

# TIDY DATA INTRO ---------------------------------------------------------

# Tidy data is a standard way of mapping the meaning of a dataset to its structure.
# A dataset is messy or tidy depending on how rows, columns and
# tables are matched up with observations, variables and types.
# In tidy data:
#
  # Each variable forms a column.
  #
  # Each observation forms a row.
  #
  # Each type of observational unit forms a table.


# Column headers are values, not variable names.
#
# Multiple variables are stored in one column.
#
# Variables are stored in both rows and columns.
#
# Multiple types of observational units are stored in the same table.
#
# A single observational unit is stored in multiple tables.


# CREATE A DATA FRAME
messy2 <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  apple = c(67, 80, 64),
  banana = c(56, 90, 50),
  grape = c(24, 3, 4)
)

# VIEW WIDE DATA FRAME
head(messy2)
str(messy2)

# MAKE A WIDE DATA FRAME NARROW
messy2 %>%
  gather(key, value = value, -1)

# Column headers are values, not variable names ----------------------------------------------------------

browseVignettes(package = c("tidyr"))
#
pew <- tbl_df(read.csv("data/pew.csv", stringsAsFactors = FALSE, check.names = FALSE))
# view data
head(pew)
# is the data tidy? How would you plot this or describe it?

# make tidy
pew %>%
  gather(income, frequency, -religion)


# YOUR TURN: CHOOSE ANOTHER EXAMPLE FROM VIGNETTE
# HOW WOULD YOU WANT TO VISUALIZE OR EXPLAIN THE DATA?
# TYPE DO NOT COPY AND PASTE


# Multiple variables stored in one column ---------------------------------
tb <- read.csv(tb_source, stringsAsFactors = FALSE)
head(tb)

# YOUR TURN: FOLLOW THE VIGNETTE
# HOW WOULD YOU WANT TO VISUALIZE OR EXPLAIN THE DATA?
# TYPE DO NOT COPY AND PASTE


# Variables are stored in both rows and columns ---------------------------

weather <- read.csv(weather_source, stringsAsFactors = FALSE)

# YOUR TURN: FOLLOW THE VIGNETTE
# HOW WOULD YOU WANT TO VISUALIZE OR EXPLAIN THE DATA?
# TYPE DO NOT COPY AND PASTE



# Multiple types in one table ---------------------------------------------

billboard <- read.csv(billboard_source, stringsAsFactors = FALSE)
# YOUR TURN: FOLLOW THE VIGNETTE
# HOW WOULD YOU WANT TO VISUALIZE OR EXPLAIN THE DATA?
# TYPE DO NOT COPY AND PASTE

# Gather without vignette ------------------------------------------------------------------
library(EDAWR)
data(cases) #load cases data
head(cases) #view cases data

# tidy cases data with gather
tidyr::gather(cases, "year","n", 2:4)
tidyr::gather(cases, "year","n", -1)

# Spread without vignette ------------------------------------------------------------------

df <- data.frame(city = c("New York", "New York", "London", "London", "Beijing", "Beijing"),
                 size = c("large", "small", "large", "small", "large", "small"),
                 amount = c(23,14,22,16,121,56),
                 stringsAsFactors = FALSE)

# make wide
df_wide <- spread(df, size, amount)

# return back to long
df_long <- gather(df_wide, key = size, value = amount, -city) %>% arrange(amount)


# compare the original and coverted data frames
df %>% arrange(amount) == df_long

# A "clean" example without the vignette ------------------------------------
# load data
population_source <- "https://raw.githubusercontent.com/rstudio/EDAWR/master/data-raw/population.csv"
population <- read.csv("population.csv", header = TRUE, check.names = FALSE)
head(population)
dim(population)

# option 1: use colname(s)
tidyPop <- gather(population, year, population, - country)
head(tidyPop)

# option 2: use column numbers
tidyPop2 <- gather(population, year, population, 2:20)

# compare the two options
head(tidyPop) == head(tidyPop2)

# plot the tidy data
ggplot(tidyPop) +
  geom_line(aes(x = year, y = population, group = country))

# subset and plot countries beginning with A
a_countries <- tidyPop %>%
  filter(grepl("^[A]\\w+", country) == TRUE)

ggplot(a_countries) +
  geom_line(aes(x = year, y = population, group = country, color = country)) +
  scale_y_continuous(name="Values")

ggplot(data = a_countries, aes(x  = year, y = population, group = country)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  facet_wrap(~country, ncol = 1, scales = 'free')


# A MESSY EXAMPLE FROM YOUR SURVEY RESULTS TIDY DATA PRACTICE -----------------------------------------------------------

results <- read.csv('data/survey_results.csv', check.names = FALSE, blank.lines.skip = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
# ignore the worning
head(results)
summary(results)
dim(results)
str(results)

# spend 5 minutes to discuss what is wrong with this data



# usefull stuff
browseVignettes(package = c("stringr"))
browseVignettes(package = c("dplyr"))
# https://www.tidyverse.org/packages/
?select_helpers
?stringr::str_replace_all
# cheatsheets (see help menu)

# option 1: rename columns with str_replace_all (nuclear option)
new_names_1 <- colnames(results) %>%
  stringr::str_replace_all(pattern = ' ', "_")

# assign column names to data frame
names(results) <- new_names_2

# option 2: create character vector for renaming using "stringr"
new_names_2 <-results %>%
  colnames() %>%
  stringr::str_replace_all(pattern = "Please rate this food: ", replacement = "") %>%
  stringr::str_replace_all(pattern = "What is the name of your", replacement = "") %>%
  stringr::str_replace_all(pattern = "What is your", replacement = "") %>%
  stringr::str_replace_all(pattern = "\\?", replacement = "") %>%
  stringr::str_trim() %>%
  stringr::str_replace_all(pattern = " ", replacement = "_") %>%
  stringr::str_to_lower()

# change character vector one at a time
new_names_2[5] <- "r_skill"
new_names_2[6] <- "ds_skill"
new_names_2[7] <- "ds_topics"
new_names_2[8] <- "models"
new_names_2[9] <- "packages"
new_names_2[12] <- "star_rank"
new_names_2[25] <- "gender"

# assign column names to data frame
names(results) <- new_names_2


# proxy for number of expanding variables
results <- results %>%
  mutate(num_models = stringr::str_count(models, pattern = ";")  +1 ,
         num_packages = stringr::str_count(packages, pattern = ";") + 1,
         num_skills = stringr::str_count(ds_skill, pattern = ";") + 1,
         num_topics = stringr::str_count(ds_topics, pattern = ";") + 1 ,

         ds_skill_desc = stringr::str_extract_all(ds_skill, "(\\D.*)") %>% str_trim(),
         r_skill_desc = stringr::str_extract_all(r_skill, "(\\D).*") %>% str_trim(),

         ds_skill_level = stringr::str_extract_all(ds_skill, "(\\d)") %>% as.numeric(),
         r_skill_level = stringr::str_extract_all(r_skill, "(\\d)") %>% as.numeric())


results  <-  results %>%
  mutate(num_ds_skill = stringr::str_count(ds_skill_desc, pattern = ";") + 1,
         num_r_skill = stringr::str_count(r_skill_desc, pattern = ";") + 1)

results <- results %>%
  separate(col = packages, into = paste0("package_",1:results$num_packages), sep = ";") %>% # package names as columns
  separate(col = models, into = paste0("model_",1:results$num_models), sep = ";") %>% # model names as columns
  separate(col = ds_skill_desc, into = paste0("ds_skill_",1:results$num_skills), sep = ";") %>%  # ds description as column
  separate(col = ds_topics, into = paste0("ds_topic_",1:results$num_topics), sep = ";") %>%  # ds topic as column
  separate(col = r_skill_desc, into = paste0("r_skill_",1:results$num_r_skill), sep = ";")  %>% # r skills as column
  select(-c(start_time:name,r_skill, ds_skill, num_models:num_topics, num_ds_skill, num_r_skill)) %>%
  gather(key = variable, value = value, -c(start_date_at_slalom, start_date_at_zendesk
                                                , hometown, favorite_us_city))
