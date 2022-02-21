# Title     : interface.R
# Objective : interface
# Created by: greyhypotheses
# Created on: 20/02/2022


# external functions
source(file = 'R/kericho/modelling/Modelling.R')
source(file = 'R/kericho/modelling/CurvesOfPredictions.R')
source(file = 'R/kericho/modelling/Autocorrelogram.R')
source(file = 'R/kericho/likelihood/InterfaceMLEU.R')
source(file = 'R/kericho/likelihood/InterfaceMLEC.R')
source(file = 'R/kericho/modelling/Coefficients.R')


# for a variable in focus, e.g., minT, maxT, and Rain
variable <- 'maxT'


# ... the modelling data set, such that observations wherein variable = NaN are excluded
data <- ModellingData(variable = variable)
ModellingDataGraph(variable = variable)


# ... a model expresion that excludes a linear trend
expr <- 'sin(2*pi*time/12) + cos(2*pi*time/12) + sin(2*pi*time/6) + cos(2*pi*time/6)'
lrdetails <- ModellingLinear(variable = variable, expr = expr)
ardetails <- ModellingAutoregressive(variable = variable, expr = expr)


# ... originals & predictions vs. time
CurvesOfPredictions(estimates = lrdetails$estimates)
CurvesOfPredictions(estimates = ardetails$estimates)


# ... autocorrelogram
Autocorrelogram(time = data$time, residues = lrdetails$model$residuals)
Autocorrelogram(time = data$time, residues = ardetails$model$residuals)


# ... likelihood ratio test
lrlikelihood <- as.numeric(logLik(object = lrdetails$model))
arlikelihood <- ardetails$model$loglik
1 - pchisq( -2*(lrlikelihood - arlikelihood), df = 1 )


# ... optimisation alternative
mleu <- InterfaceMLEU(model = lrdetails$model, data = data, variable = variable)
mlec <- InterfaceMLEC(model = lrdetails$model, data = data, variable = variable)


# ... coefficients & phi
x <- rbind(Coefficients(est = lrdetails$model$coefficients, modelname = 'linear regression'),
           Coefficients(est = ardetails$model$coef, modelname = 'autoregressive'),
           Coefficients(est = mlec$par, modelname = 'custom mle'))
x['autoregressive', 'phi'] <- ardetails$model$coef['ar1']
x['custom mle', 'phi'] <- sqrt(mlec$par['phisqr'])

x




