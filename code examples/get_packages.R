#' Install and load packages
#'
#' @param pkgs_install: a character vector of R packages with one or more packages
#'
#' @return
#' side effect of installing and / or loading pkgs_install elements
#' @export
#'
#' @examples
#' With only one package
#' pkgs_install <- c('data.table')
#' sapply(pkgs_install, get_packages)
#'
#' With more than one package
#' pkgs_install_two <- c('tidyverse','data.table')
#' sapply(pkgs_install_two, get_packages)
#'
get_packages <- function(pkgs_install){
  if (is.element(pkgs_install, installed.packages()[,1])){
    library(pkgs_install, character.only = TRUE, verbose = TRUE)
    } else {
    install.packages(pkgs_install, dep = TRUE)
      library(pkgs_install, character.only = TRUE, verbose = FALSE)
    }
  }
