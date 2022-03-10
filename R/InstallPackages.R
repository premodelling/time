# Title     : InstallPackages.R
# Objective : Install and activate packages
# Created by: greyhypotheses
# Created on: 14/02/2022


InstallPackages <- function (){

  packages <- c('tidyverse', 'ggplot2', 'moments', 'rmarkdown', 'stringr', 'latex2exp', 'mapview', 'tseries',
                'healthcareai', 'equatiomatic', 'rstatix', 'matrixStats', 'patchwork', 'geoR', 'PrevMap')

  # Install
  .install <- function(x){
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      if (x == 'rmarkdown') {tinytex::install_tinytex()}
    }
  }
  lapply(packages, .install)

  # Activate
  .activate <- function (x){
    library(x, character.only = TRUE)
    if (x == 'rmarkdown') {library(tinytex)}
  }
  lapply(packages[!(packages %in% c('tidyverse', 'healthcareai', 'equatiomatic', 'tseries'))], .activate)

  # Special Case
  if ('tidyverse' %in% packages) {
    lapply(X = c('magrittr', 'dplyr', 'tibble', 'ggplot2', 'stringr', 'lubridate'), .activate)
  }

  # Active libraries
  sessionInfo()

}