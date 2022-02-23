# Title     : SimulateMatern.R
# Objective : Simulate a matern based process
# Created by: greyhypotheses
# Created on: 22/02/2022

#' @param start: below
#' @param end: start & end -> for creating a distance matrix between pairs of data locations
#' @param by: granularity ...
#' @param mu:
#' @param sigmasqr:
#' @param phi: the range parameter φ
#' @param kappa: the smoothness parameter κ
#' @param tausqr
SimulateMatern <- function(start, end, by, mu, sigmasqr, phi, kappa, tausqr) {

  # a sequence of point
  points <- seq(from = start, to = end, by = by)
  n <- length(x = points)

  # matern() evaluates the Matern correlation values w.r.t. distances matrix u
  # note that u ≡ h
  Sigma <- sigmasqr * geoR::matern(u = as.matrix(dist(x = points)), phi = phi, kappa = kappa)

  # add sqr(tau)
  diag(Sigma) <- diag(Sigma) + tausqr

  # Sigma: n x n
  # chol(Sigma): n x n
  # rnorm(n): n x 1
  # mu: scalar
  # tensor: n x 1
  tensor <- as.numeric(mu + t(chol(Sigma)) %*% rnorm(n))

  return(tensor)
}
