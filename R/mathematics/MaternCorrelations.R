# Title     : MaternCorrelations.R
# Objective : Explore Matern via ρ(distances | φ, κ)
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
#' @param PHI: a set of range parameters φ
#'
MaternCorrelations <- function (limit, by, kappa, mu, sigmasqr, tausqr, PHI) {

  starting <- 0
  abscissae <- seq(from = starting, to = limit, by = by)

  T <- function (phi) {

    # the series is ρ(distances | φ, κ)
    series <- SimulateMatern(start = starting, end = limit, by = by, mu = mu, sigmasqr = sigmasqr,
                             phi = phi, kappa = kappa, tausqr = tausqr)
    geodata <- list(coords = cbind(abscissae, 1), data = series)
    data <- data.frame(abscissa = abscissae, series = series) %>%
      dplyr::mutate(phi = phi)

    # variogram
    variogram <- variog(coords = geodata$coords, data = geodata$data,
                        uvec = seq(starting, round(2 * limit / 3, digits = 1), by = by))
    envelop <- VariogramEnvelop(geodata = geodata, variogram = variogram, nenvelops = 5, nsim = 2000)

    # summary
    estimates <- data.frame(distance = variogram$u, estimate = variogram$v) %>%
      dplyr::mutate(estimate.lower = envelop$lower.mean, estimate.upper = envelop$upper.mean) %>%
      dplyr::mutate(phi = phi)

    return(list(estimates = estimates, data = data))

  }

  frame <- lapply(X = PHI, FUN = T)
  estimates <- dplyr::bind_rows(lapply(X = seq(1, length(frame)), FUN = function (x) frame[[x]][1]))
  data <- dplyr::bind_rows(lapply(X = seq(1, length(frame)), FUN = function (x) frame[[x]][2]))

  return(list(estimates =  estimates, data = data))

}