### 01-Transform.R
# library(dplyr)
# library(ggplot2)
# library(reshape)

## Baby names data

library(ggplot2)
options(stringsAsFactors = FALSE)
bnames <- read.csv("data/bnames.csv.bz2")
births <- read.csv("data/births.csv")

head(bnames)
tail(bnames)
getwd()

# Your Turn
# ----------------------------------------------
# create a data frame with only your name
Sarah <- bnames[bnames$name == 'Sarah', ]
qplot(year, prop, data = Sarah, geom = 'line')
# plot your name over time

# ----------------------------------------------

michael <- bnames[bnames$name == "Michael", ]
qplot(year, prop, data = michael, geom = "line")
qplot(year, prop, data = michael, geom = "point")
qplot(year, prop, data = michael, geom = "line",
  color = sex)

michaels <- bnames[bnames$name == "Michael" |
                   bnames$name == "Michelle", ]
qplot(year, prop, data = michaels, geom = "line",
  color = interaction(sex, name))


# Your Turn
# ----------------------------------------------
# create a data frame with analyst names
all_folks <- bnames[bnames$name%in% c('Aaron', 'Kathy'),]
qplot(year, prop, data = all_folks, geom = "line",
      color = interaction(sex, name))

# plot over time

# ----------------------------------------------



## dplyr

# install.packages("dplyr")
library(dplyr)

bnames <- tbl_df(bnames)
births <- tbl_df(births)
class(bnames)

tbl_df(diamonds)

## filter select mutate summarise arrange

df <- data.frame(color = c("blue", "black", "blue", "blue", "black"), value = 1:5)
tbl <- tbl_df(df)

filter(tbl, color == "blue")
filter(df, value %in% c(1, 4))

# Your Turn
# FILTER --------------------------------
# use dplyr verbs to create a dataframe with your name
filter(bnames, name == 'names')
# use dplyr to limit years from 1900 to 2000


# what are the dimensions

# SELECT --------------------------------

select(tbl, color)
select(df, -color)

# Your Turn
# --------------------------------
# use select helpers (below)
?dplyr::select_helpers
#select only soundex column by name
#select soundex column by starts_with
#select soundex column by ends_with


# ARRANGE --------------------------------

arrange(tbl, color)
arrange(tbl, desc(color))

# Your Turn
# --------------------------------

# arrange bnames descending prop

# arrange your name dataframe descending prop


# MUTATE AND SUMMARISE--------------------------------

mutate(tbl, double = 2 * value)
mutate(tbl, double = 2 * value, quadruple = 2 * double)

summarise(tbl, total = sum(value))
summarise(tbl, total = sum(value), avg = mean(value))

# Your Turn
# --------------------------------
 #create a new column called perc (prop * 100) in bnames
# create three summary variablss min(prop), mean(prop), max(prop) in bnames


# Joining data sets--------------------------------

head(bnames)
head(births)

x <- data.frame(
  name = c("John", "Paul", "George", "Ringo", "Stuart", "Pete"),
  instrument = c("guitar", "bass", "guitar", "drums", "bass",
     "drums"))

y <- data.frame(
  name = c("John", "Paul", "George", "Ringo", "Brian"),
  band = c("TRUE", "TRUE", "TRUE",  "TRUE", "FALSE"))

left_join(x, y, by = "name")
inner_join(x, y, by = "name")
semi_join(x, y, by = "name")
anti_join(x, y, by = "name")

# Your Turn
# ----------------------------------------------------
bnames2 <-
  #join bnames and births by year and sex

# create new variable n (prop * births; consider round() function)

# Groupwise operations ----------------------------------------------------

# Your Turn
# ----------------------------------------------------
garrett <- filter(bnames2, name == "Garrett")
sum(garrett$n)
summarise(garrett, total = sum(n))
# ----------------------------------------------------


## group_by

summarise(tbl, total = sum(value))
by_color <- group_by(tbl, color)
summarise(by_color, total = sum(value))

group_by(bnames2, name)
by_name <- group_by(bnames2, name)
totals <- summarise(by_name, total = sum(n))
totals

group_by(bnames2, name, sex)
by_name <- group_by(bnames2, name)
group_by(by_name, sex)

name_sex <- group_by(bnames2, name, sex)
totals2 <- summarise(name_sex, total = sum(n))
totals2

by_name_sex <- group_by(bnames2, name, sex)
ungroup(by_name_sex)

# WINDOW FUNCTIONS ------------------------------------------------------------
by_soundex <- group_by(bnames2, soundex)
stotals <- summarise(by_soundex, total = sum(n))
stotals
arrange(stotals, desc(total))
j500 <- filter(bnames, soundex == "J500")
unique(j500$name)

year_sex <- group_by(bnames2, year, sex)
ytotals <- summarise(year_sex, births = sum(n))
ytotals
qplot(year, total, data = ytotals, geom = "line", color = sex)

year_sex <- group_by(bnames2, year, sex)
ranks <- mutate(year_sex, rank = rank(desc(prop)))
ranks
garrett <- filter(ranks, name == "Garrett")
qplot(year, -rank, data = garrett, geom = "line")
ones <- filter(ranks, rank == 1)
ones <- select(ones, year, name, gender)
ones


# ------------------------------------------------------------

name_sex <- group_by(bnames2, name, sex)
name_sex
summary1 <- summarise(name_sex, total = sum(n))
summary1
summary2 <- summarise(summary1, total = sum(total))
summary2
summary3 <- summarise(summary2, total = sum(total))
summary3


# Where next?

browseVignettes(package = "dplyr")
