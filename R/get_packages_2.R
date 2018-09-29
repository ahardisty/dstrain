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
