# Title     : kericho.R
# Objective : Tutorial
# Created by: greyhypotheses
# Created on: 14/02/2022


# external functions
source(file = 'R/01/triple/Modelling.R')
source(file = 'R/01/triple/Curves.R')



#' Part 2
#'

# exercise 2 & 3: for variables minT, maxT, and Rain
variable <- 'minT'

free <- ModellingData(variable = variable)

ModellingDataGraph(variable = variable)

single <- ModellingSeasonSingle(variable = variable)
Curves(estimates = single$estimates)

double <- ModellingSeasonDouble(variable = variable, estimates = single$estimates )
Curves(estimates = double$estimates)

ModellingAutocorrelogram(estimates = double$estimates, models = list(single = single$model, double = double$model))



# exercise 4: For each model of each variable of exercise 2 & 3 ... example
lrmodel <- double$model
X <- lrmodel$x[, -1]
armodel <- arima(x = free[variable], order = c(1, 0, 0), xreg = X, method = 'ML')

# exercise 4: ... likelihood ratio test
lrlikelihood <- as.numeric(logLik(object = lrmodel))
arlikelihood <- armodel$loglik
1 - pchisq( -2*(lrlikelihood - arlikelihood), df = 1 )

# exercise 4: ... autocorrelogram
acf(x = armodel$residuals)

# exercise 4: ... ARIMA predictions via original - residuals
predictions <- data.frame(free[, variable] - armodel$residuals)
names(predictions) <- paste0(variable, '_predictions')
cbind(free, predictions)



#' Part 3
#'

source(file = 'R/01/likelihood/OptimisationAlgorithms.R')
mle <- TripleOptimisation(model = lrmodel, data = free, variable = variable)


