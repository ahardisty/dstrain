# Prep environment and load functions
get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
  #   Returns loaded packages.
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  library(p, character.only = TRUE, verbose = TRUE)
}

PACKAGES <- c('tidyverse',
              'data.table',
              'tidyr',
              # 'plyr',
              'dplyr',
              'randomForest',
              'earth',
              'lubridate',
              'rattle',
              'RCurl',
              'rpart',
              'rpart.plot',
              'haven',
              'stringr',
              'broom',
              'purrr',
              'readxl',
              'readr')



rmse <- function(y, y_hat) {
  return(sqrt(sum((y - y_hat)^2) / length(y)))
}

r_squared <- function(y, y_hat){
  y_bar <- mean(y)
  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  rsq <- 1 - (SS_res / SS_tot)
  return(rsq)
}

# source function and data formatting scripts
# source('r/helper_functions.R')
# Load packages
sapply(PACKAGES, get_packages)

install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
