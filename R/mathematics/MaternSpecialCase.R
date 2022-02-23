# Title     : MaternSpecialCase.R
# Objective : Matern Special Case ρ(distance) = exp(- distance / φ )
# Created by: greyhypotheses
# Created on: 22/02/2022


source('R/mathematics/SimulateMatern.R')
source('R/mathematics/VariogramEnvelop.R')


#' @param limit:
#' @param by: granularity ...
#' @param mu:
#' @param sigmasqr:
#' @param tausqr:
#' @param RHO: a set of Matern correlation values
#'
MaternSpecialCase <- function (limit, by, mu, sigmasqr, tausqr, RHO) {

  kappa <- 0.5
  starting <- 0
  abscissae <- seq(from = starting, to = limit, by = by)

  T <- function (rho) {

    phi <- -limit/log(rho)

    # data
    series <- SimulateMatern(start = starting, end = limit, by = by, mu = mu, sigmasqr = sigmasqr,
                             phi = phi, kappa = kappa, tausqr = tausqr)
    geodata <- list(coords = cbind(abscissae, 1), data = series)
    data <- data.frame(abscissa = abscissae, series = series) %>%
      dplyr::mutate(rho = rho, phi = round(phi, digits = 3))

    # variogram
    variogram <- variog(coords = geodata$coords, data = geodata$data,
                        uvec = seq(starting, round(2 * limit / 3, digits = 0), by = by))
    envelop <- VariogramEnvelop(geodata = geodata, variogram = variogram, nenvelops = 5, nsim = 2000)

    # summary
    estimates <- data.frame(distance = variogram$u, estimate = variogram$v) %>%
      dplyr::mutate(estimate.lower = envelop$lower.mean, estimate.upper = envelop$upper.mean) %>%
      dplyr::mutate(rho = rho, phi = round(phi, digits = 3))

    return(list(estimates = estimates, data = data))

  }

  frame <- lapply(X = RHO, FUN = T)
  estimates <- dplyr::bind_rows(lapply(X = seq(1, length(frame)), FUN = function (x) frame[[x]][1]))
  data <- dplyr::bind_rows(lapply(X = seq(1, length(frame)), FUN = function (x) frame[[x]][2]))

  return(list(estimates =  estimates, data = data))

}