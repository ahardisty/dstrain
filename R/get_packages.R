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

PACKAGES <- c('data.table',
              # 'tidyr',
              # 'plyr',
              # 'dplyr',
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
              'readr',
              'tidyverse')
