# Title     : MLEU.R
# Objective : Maximum Likelihood
# Created by: greyhypotheses
# Created on: 19/02/2022


#'
#' In progress ...
#'
#' @param par: c(beta_0, ..., beta_, phisqr, sigmasqr)
#' @param df: a data frame of design variables & y
#'
MLEU <- function (par, df) {

  # the coefficients being estimated
  parameters <- par[!(names(par) %in% c('phi', 'sigmasqr'))]
  beta <- as.matrix(x = parameters, ncol = 1)


  # The constant and independent variables of the design matrix
  design <- df %>%
    select(!(y))
  design <- as.matrix(x = design)


  # Hence
  mu <- mean(design %*% beta)


  # the other parameters being estimated
  phi <- as.numeric(par['phi'])
  sigmasqr <- as.numeric(par['sigmasqr'])


  # number of observations
  N <- nrow(df)


  # processes
  Y <- df['y']
  fore <- Y[1, 'y']
  tailing <- Y[2:N, 'y']
  leading <- Y[1:(N-1), 'y']


  # Unconditional sum of squares
  F <- (tailing - mu) - phi*(leading - mu)
  unconditional <- (1 - phi^2)*((fore - mu)^2) + (F %*% F)

  
  # terms
  terms <- -(0.5 * N * log(2 * pi)) - ( 0.5 * N * log(sigmasqr) ) + ( 0.5 * log(1 - phi^2) )


  # natural log likelihood
  series <- terms - (0.5 * unconditional / sigmasqr)

  # Cost
  C <- -series

  return(C)
}