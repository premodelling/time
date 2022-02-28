# Title     : InterfaceMLEC.R
# Objective : Optimisation
# Created by: greyhypotheses
# Created on: 20/02/2022


source(file = 'R/kericho/problems/trends/likelihood/MLEC.R')


#' for estimating phi, mu, sigmasqr via MLEU()
#'
#' @param model: a lm() model from whence external regressors, i.e., its design matrix variables, are extracted
#' @param data: the modelling data of <model>
#' @param variable: the variable being modelled
#'
InterfaceMLEC <- function (model, data, variable) {

  # Preliminaries
  estimate <- summary(object = model)
  design <- model$x %>% data.frame()
  names(design) <- c( 'constant', paste0('x_', seq(from = 1, to = ncol(design) - 1)) )
  Y <- data[, variable, drop = FALSE]
  names(Y) <- 'y'


  # Initialising the parameters to be determined ...
  phisqr_ <- 0.5
  beta_ <- as.numeric(estimate$coefficients[,'Estimate'])
  sigmasqr_ <- 0.4

  par <- c(beta_, phisqr_, sigmasqr_)
  names(par) <- c(paste0('beta_', seq(from = 0, to = -1 + length(beta_))), 'phisqr', 'sigmasqr')

  lower <- c( rep(x = -Inf, length(beta_)), 0, 0.0001 )
  upper <- c( rep(x = Inf, length(beta_)), 0.9999, Inf )


  # Determine the parameters of ... via MLE
  frame <- cbind(Y, design)
  mle <- optim(par = par, fn = MLEC, df = frame, method = 'L-BFGS-B',
               lower = lower, upper = upper, control = list(maxit = 5000))

  return(mle)

}