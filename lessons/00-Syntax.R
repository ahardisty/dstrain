## 02-Syntax.R
# Code from 02-subsetting
# library(ggplot2)
# crete a numeric vector
x <- c()

# create a new object with x as the values

# check if it is the same
# ==
# exact

# Subsetting

vec <- c(6, 1, 3, 6, 10, 5)
df <- data.frame(
  name = c("John", "Paul", "George", "Ringo"),
  birth = c(1940, 1942, 1943, 1940),
  instrument = c("guitar", "bass", "guitar", "drums")
)

df[2,3]
df[c(2,4),c(2,3)]
df[c(2,4),3]

1:4
df[1:4, 1:2]
df[c(1,1,1,2,2), 1:3]

vec[0]
df[1:2, 0]

vec[c(5, 6)]
vec[-c(5, 6)]

df[c(2:4), 2:3]
df[-c(2:4), 2:3]
df[-c(2:4),-(2:3)]

vec(1:4)
vec[-1:4]
df[c(1,2)]

vec[ ]
df[1, ]
df[ ,2]

names(vec) <- c("a","b","c","d","e","f")
vec[c("a","b","d")]
vec[c("a","c","f")]

df[ ,"birth"]
df[ ,c("name","birth")]

vec[c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)]

df[c(FALSE,TRUE,TRUE,FALSE), ]

# Your Turn
# ---------------------------------------
# subset
# ---------------------------------------

# Your Turn
# ---------------------------------------

# create lists
# ---------------------------------------

lst[c(1,2)]
lst[1]
lst[[1]]

lst[[1]][2]

names(lst) <- c("alpha", "beta", "gamma")
lst$alpha

df$birth

# Diamonds

# Your turn
# --------------------------
diamonds
library(tidyverse)
library(tidyverse)
# nrow of diamonds
diamonds[1:6]
diamonds[c(1:6)]
# summary of diamonds
nrow(diamonds)
str(diamonds)
dim(diamonds)
summary(diamonds)
# --------------------------

head(diamonds)
tail(diamonds)
View(diamonds)



qplot(x, y, data = diamonds)

# Logical tests

1 < 3
1 > 3
c(1, 2, 3, 4, 5) > 3

# Your Turn
# -------------------
x <- c(1, 2, 3, 4, 5)

x > 3
x >= 3
x < 3
x <= 3
x == 3
x != 3
x = 3
# -------------------

1 %in% c(1, 2, 3, 4)
1 %in% c(2, 3, 4)
c(3,4,5,6) %in% c(2, 3, 4)

1 %in% c(1, 2, 3, 4)
1 %in% c(2, 3, 4)
c(3,4,5,6) %in% c(2, 3, 4)

x > 2 & x < 9

TRUE & TRUE
TRUE & FALSE
FALSE & TRUE
FALSE & FALSE

TRUE | TRUE
TRUE | FALSE
FALSE | TRUE
FALSE | FALSE

xor(TRUE, TRUE)
xor( TRUE, FALSE)
xor( FALSE, TRUE)
xor( FALSE, FALSE)

!(TRUE)
!(FALSE)

any(c(TRUE, FALSE, FALSE))
any(c(FALSE, FALSE, FALSE))

all(c(TRUE, TRUE, TRUE))
all(c(TRUE, FALSE, TRUE))

w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

# Your Turn
# --------------------------------------------
w > 0
10 < x & x < 20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"))
# --------------------------------------------

# Logical subsetting

x_zeroes <- diamonds$x == 0
class(x_zeroes)
diamonds[x_zeroes, ]

# Your Turn
# create a dataframe from diamonds where x and y are equal ------------------------------------

# create logical vector
x_and_y <- diamonds$x == diamonds$y

# subset using logical vectora



# create dataframes using subset notation []  -------------

# diamond depth is between 55 and 70

# caret < mean(carat)

# price per carat < 10000

# all diamonds with cut better than "good"

# all diamond with cut in c("Very Good", "Premium", "Ideal")

# ------------------------------------

diamonds[diamonds$x > 10, ]
big <- diamonds[diamonds$x > 10, ]
diamonds <- diamonds[diamonds$x < 10,]

diamonds <- diamonds[1, 1]
diamonds
rm(diamonds)
str(diamonds)


