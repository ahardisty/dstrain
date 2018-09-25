install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")
library(roxygen2)
library(testthat)
devtools::session_info()
library()



PACKAGES <- c(
  'data.table',
  'caret',
  'earth',
  'tidyverse',
  'stats',
  'gbm',
  'stringr',
  'ggthemes',
  'wesanderson',
  'scales',
  'binr',
  'odbc',
  'DBI',
  'RMySQL',
  "devtools", "roxygen2", "testthat", "knitr"
  )


get_packages <- function(p){
  # Check for, install, and load required packages
  #
  # Args:
  #   p: individual package name as a character string.
  #
  # Returns:
  #   Returns loaded packages.
  if (is.element(p, installed.packages()[,1])){
  library(p, character.only = TRUE, verbose = TRUE)
    }else{
  install.packages(p, dep = TRUE)
  library(p, character.only = TRUE, verbose = TRUE)
    }
}

# Load packages
sapply(PACKAGES, get_packages)

