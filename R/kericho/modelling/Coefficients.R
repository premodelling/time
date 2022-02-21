# Title     : Coefficients.R
# Objective : Coefficients
# Created by: greyhypotheses
# Created on: 21/02/2022

Coefficients <- function (estimates, modelname) {

  # as a data frame
  T <- data.frame(estimates)

  # focus on coefficients only
  T <- T[!(row.names(T) %in% c('ar1', 'phisqr', 'sigmasqr')), 1, drop = FALSE]
  names(T) <- modelname
  T <- data.frame(t(T))

  # coefficient names
  labels <- paste0( 'beta_', seq(from = 0, to = -1 + length(T)) )
  names(T) <- labels


  return(T)

}