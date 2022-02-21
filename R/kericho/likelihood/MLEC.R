# Title     : MLEC.R
# Objective : Maximum Likelihood
# Created by: greyhypotheses
# Created on: 20/02/2022

MLEC <- function (par, df) {

  # the coefficients being estimated
  parameters <- par[!(names(par) %in% c('phisqr', 'sigmasqr'))]
  beta <- as.matrix(x = parameters, ncol = 1)


  # The constant and independent variables of the design matrix
  design <- df %>%
    select(!(y))
  design <- as.matrix(x = design)


  # Hence
  mu <- mean(design %*% beta)


  # the other parameters being estimated
  phisqr <- as.numeric(par['phisqr'])
  sigmasqr <- as.numeric(par['sigmasqr'])


  # number of observations
  N <- nrow(df)


  # processes
  Y <- df['y']
  fore <- Y[1, 'y']
  tailing <- Y[2:N, 'y']
  leading <- Y[1:(N-1), 'y']


  # Unconditional sum of squares
  F <- (tailing - mu) - sqrt(phisqr)*(leading - mu)
  unconditional <- (1 - phisqr)*((fore - mu)^2) + (F %*% F)


  # terms
  terms <- -(0.5 * N * log(2 * pi)) - ( 0.5 * N * log(sigmasqr) ) + ( 0.5 * log(1 - phisqr) )


  # natural log likelihood
  series <- terms - (0.5 * unconditional / sigmasqr)

  # Cost
  C <- -series

  return(C)




}