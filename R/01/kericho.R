# Title     : kericho.R
# Objective : Tutorial
# Created by: greyhypotheses
# Created on: 14/02/2022


source(file = 'R/01/StudyData.R')
source(file = 'R/01/ExplorationGraph.R')
source(file = 'R/functions/TimeDependentLag.R')
source(file = 'R/01/rain/Rainfall.R')
source(file = 'R/01/rain/Regression.R')

source(file = 'R/01/triple/Modelling.R')
source(file = 'R/01/triple/Curves.R')



#' Data
#'

instances <- StudyData()




#' Part 1
#'

# excercise 1.1: relationships
ExplorationGraphNL(instances = instances)


# excercise 1.2a: lagged rainfall series
temporary <- TimeDependentLag(frame = instances, frame.date = 'date',
                 frame.date.granularity = 'month', frame.focus = 'Rain', lags = seq(from = 0, to = 4) )
head(temporary$frame[temporary$lagfields])


# excercise 1.2b: graphs of ln(cases) vs. lagged rainfall series
Rainfall(data = temporary$frame, lagfields = temporary$lagfields)


# exercise 1.2c: correlation between ln(cases) and each lagged rainfall series
correlation <- function (variable) {
  y <- cor(x = temporary$frame$CasesLN, y = temporary$frame[variable], use = 'complete.obs', method = 'pearson') %>%
    data.frame()
}
correlations <- dplyr::bind_cols(lapply(X = temporary$lagfields, FUN = correlation))
row.names(correlations) <- 'correlation'
correlations


# exercise 1.3
regression <- Regression(data = temporary$frame, lagfields = temporary$lagfields)
merge(x = regression, y = t(correlations), by = 0)



#' Part 2
#'

# exercise 2: for variables minT, maxT, and Rain
variable <- 'maxT'

free <- ModellingData(instances = instances, variable = variable)

ModellingDataGraph(instances = instances, variable = variable)

single <- ModellingSeasonSingle(instances = instances, variable = variable)
Curves(estimates = single$estimates)

double <- ModellingSeasonDouble(instances = instances, variable = variable, estimates = single$estimates )
Curves(estimates = double$estimates)

ModellingAutocorrelogram(estimates = double$estimates, models = list(single = single$model, double = double$model))


# For each model of a variable ...

# exercise 2: ... likelihood ratio test
lrmodel <- double$model
covariance <- lrmodel$x[, -1]
armodel <- arima(x = free[variable], order = c(1, 0, 0), xreg = covariance)

lrlikelihood <- as.numeric(logLik(object = lrmodel))
arlikelihood <- armodel$loglik
1 - pchisq( -2*(lrlikelihood - arlikelihood), df = 1 )

# exercise 2: ... autocorrelogram
acf(x = armodel$residuals)

# exercise 2: ... ARIMA predictions via original - residuals
predictions <- data.frame(free[, variable] - armodel$residuals)
names(predictions) <- paste0(variable, '_predictions')
cbind(free, predictions)