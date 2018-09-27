## 01-Structures.R
# Code from 01-structures

5 + 5
4 - 1
1 * 2
4 ^ 2

a <- 1
b <- 2
a + b
A <- 3
a + b - A

round(3.1415)
factorial(3)
sqrt(9)

factorial(round(2.0015) + 1)

# Your Turn
# ---------
# add some numbers
# ---------

# R objects
# create a numeric object
foo <-

# manipulate the numeric object

# remove the numeric object


# Data structures

# Warm up
# ---------
WorldPhones
# ---------

vec <- c(1, 2, 3, 10, 100)
vec

mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
mat

matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)

vec + 4
vec * 4
vec * vec # element-wise multiplication

vec %*% vec # matrix multiplication (inner)
vec %o% vec # matrix multiplication (outer)

mat
t(mat) # transpose

array(c(1, 2, 3, 4, 5, 6), dim = c(2, 2, 3))

# Data types

1 + 1
3000000
class(0.00001)

"hello"
class("hello")

"hello" + "world"
nchar("hello")
paste("hello", "world")

# Warm up
# ---------
1
"1"
"one"
# ---------

3 > 4
class(TRUE)
class(T)

fac <- factor(c("a", "b", "c"))
fac
class(fac)

# Quiz
# ---------------------
x <- c(1, 2, 3)
x
"x"
# ---------------------

# Your Turn
# ---------------------
vec <- c(1, "R", TRUE)
class(vec)
vec
# ---------------------

# Quiz
# ---------
c(5, "two")
c(TRUE, "a")
c(1, "TRUE")
TRUE + 5
# ---------

as.numeric("1")
as.character(TRUE)

lst <- list(1, "R", TRUE)
class(lst)

list(c(1, 2), TRUE, c("a", "b", "c"))

df <- data.frame(c(1, 2, 3),
 c("R","S","T"), c(TRUE, FALSE, TRUE))
class(df)

nvec <- c(one = 1, two = 2, three = 3)
nvec

nlst <- list(one = 1, two = 2, many = c(3, 4, 5))
nlst

ndf <- data.frame(numbers = c(1, 2, 3),
          letters = c("R","S","T"),
          logic = c(TRUE, FALSE, TRUE))
ndf

names(ndf)
names(nvec)
names(nvec) <- c("uno", "dos", "tres")
nvec

