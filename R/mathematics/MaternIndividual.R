# Title     : MaternIndividual.R
# Objective : Matern Individual
# Created by: greyhypotheses
# Created on: 22/02/2022


source('R/mathematics/SimulateMatern.R')
source('R/mathematics/VariogramEnvelop.R')


#' @param limit:
#' @param by: granularity ...
#' @param kappa: the smoothness parameter κ
#' @param mu:
#' @param sigmasqr:
#' @param tausqr:
#' @param phi: a set of range parameters φ
#'
MaternIndividual <- function (limit, by, kappa, mu, sigmasqr, tausqr, phi) {

  # a data set
  data <- SimulateMatern(start = 1, end = limit, mu = mu, sigmasqr = sigmasqr,
                         phi = phi, kappa = kappa, tausqr = tausqr)
  acf(x = data, frame.plot = FALSE)


  # its abscissae
  abscissae <- seq(from = 1, to = limit, by = by)


  # the variogram objects of the data set
  geodata <- list(coords = cbind(abscissae, 1), data = data)
  variogram <- variog(coords = geodata$coords, data = geodata$data,
                      uvec = seq(0, round(2 * limit / 3, digits = 0), by = by))


  # the variogram graph
  envelop <- VariogramEnvelop(geodata = geodata, variogram = variogram, nenvelops = 5, nsim = 1000)
  matplot(envelop$distance, cbind(envelop$lower.mean, variogram$v, envelop$upper.mean), type='l',
          col = 1, lty = c('dashed', 'solid', 'dashed'),
          xlab = 'distance',
          ylab = 'variogram', frame.plot = FALSE)


  # the data curve
  plot(x = data, xlab = 'abscissae', ylab = 'data', type = 'l', frame.plot = FALSE)

}