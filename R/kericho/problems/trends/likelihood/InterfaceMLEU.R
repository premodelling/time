# Title     : InterfaceMLEU.R
# Objective : Optimisation
# Created by: greyhypotheses
# Created on: 19/02/2022


source(file = 'R/kericho/problems/trends/likelihood/MLEU.R')


#'
#' In progress ... switch from optim() to nlminb()
#'
#' for estimating phi, mu, sigmasqr via MLEU()
#'
#' @param model: a lm() model from whence external regressors, i.e., its design matrix variables, are extracted
#' @param data: the modelling data of <model>
#' @param variable: the variable being modelled
#'
InterfaceMLEU <- function (model, data, variable) {

  # Preliminaries
  estimate <- summary(object = model)
  design <- model$x %>% data.frame()
  names(design) <- c( 'constant', paste0('x_', seq(from = 1, to = ncol(design) - 1)) )
  Y <- data.frame(y = data[[variable]])


  # Initialising the parameters to be determined ...
  phi_ <- 0.5
  beta_ <- as.numeric(estimate$coefficients[,'Estimate'])
  sigmasqr_ <- 0.4

  par <- c(beta_, phi_, sigmasqr_)
  names(par) <- c(paste0('beta_', seq(from = 0, to = -1 + length(beta_))), 'phi', 'sigmasqr')


  # Determine the parameters of ... via MLE
  frame <- cbind(Y, design)
  mle <- optim(par = par, fn = MLEU, df = frame, method = 'BFGS', control = list(maxit = 5000))

  return(mle)

}

