# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 28/02/2022


# external functions
source(file = 'R/kericho/problems/trends/Modelling.R')
source(file = 'R/kericho/problems/trends/CurvesOfPredictions.R')
source(file = 'R/kericho/problems/trends/Autocorrelogram.R')
source(file = 'R/kericho/problems/trends/Coefficients.R')


# for a variable in focus, e.g., minT, maxT, and Rain
variable <- 'minT'


# ... the modelling data set, such that observations wherein variable = NaN are excluded
data <- ModellingData(variable = variable)
ModellingDataGraph(variable = variable)


# ... a model expresion that excludes a linear trend
expr <- 'sin(2*pi*time/12) + cos(2*pi*time/12) + sin(2*pi*time/6) + cos(2*pi*time/6)'
lrdetails <- ModellingLinear(variable = variable, expr = expr)

predictions <- cbind(data[, c(variable, 'time')], prediction = lrdetails$model$fitted.values)
predictions %>%
  gather(key = 'data', value = 'value', -time) %>%
  ggplot(mapping = aes(x = time, y = value, colour = factor(data))) +
  geom_line(alpha = 0.65) +
  scale_colour_manual(values = c('black', 'orange')) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05))



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


# ... custom optimisation
