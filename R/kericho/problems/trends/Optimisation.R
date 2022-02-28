# Title     : Optimisation.R
# Objective : Optimisation
# Created by: greyhypotheses
# Created on: 28/02/2022


source(file = 'R/kericho/problems/trends/likelihood/InterfaceMLEC.R')


Optimisation <- function (lrdetails, ardetails, data, variable) {

  # ... optimisation alternative
  mlec <- InterfaceMLEC(model = lrdetails$model, data = data, variable = variable)

  # ... coefficients & phi
  x <- rbind(Coefficients(est = lrdetails$model$coefficients, modelname = 'linear regression'),
             Coefficients(est = ardetails$model$coef, modelname = 'autoregressive'),
             Coefficients(est = mlec$par, modelname = 'custom mle'))
  x['autoregressive', 'phi'] <- ardetails$model$coef['ar1']
  x['custom mle', 'phi'] <- sqrt(mlec$par['phisqr'])

  x

}