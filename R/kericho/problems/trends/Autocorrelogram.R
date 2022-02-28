# Title     : Autocorrelogram.R
# Objective : Autocorrelogram
# Created by: greyhypotheses
# Created on: 20/02/2022


#'
#' @param time: time points
#' @param residues: a model's residual's
#'
Autocorrelogram <- function(time, residues) {

  par(mfrow = c(1, 2))
  plot(time, residues, type = "l", frame.plot = FALSE)
  acf(residues, frame.plot = FALSE)

}