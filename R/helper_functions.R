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
    install.packages(p, dep = TRUE, lib = 'C:/Program Files/R/R-3.3.0/library' )
  library(p, character.only = TRUE, verbose = TRUE)
}

PACKAGES <- c('ggplot2',
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
              'readxl')


# source function and data formatting scripts
source('r/helper_functions.R')
# Load packages
sapply(PACKAGES, get_packages)
