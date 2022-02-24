# Title     : main.R
# Objective : Tutorial
# Created by: greyhypotheses
# Created on: 14/02/2022


#'
#' The analysis scripts are
#'      R/kericho/explore/interface.R
#'      R/kericho/modelling/interface.R
#'      R/kericho/rain/interface.R
#'



# external functions
source(file = 'R/kericho/StudyData.R')
source(file = 'R/kericho/functions/TimeDependentLag.R')
source(file = 'docs/programme/mathematics/auxiliary_function.R')
source(file = 'R/kericho/problems/fourth/PredictionsGraph.R')


# data
instances <- StudyData()
str(instances)


# lagged series
extra <- function (variable) {
  temporary <- TimeDependentLag(frame = instances,
                                frame.date = 'date',
                                frame.date.granularity = 'month',
                                frame.focus = variable,
                                lags = seq(from = 2, to = 2) )
  series <- temporary$frame[temporary$lagfields]

  return(series)
}
lagged.variables <- lapply(X = c('minT', 'maxT', 'Rain'), FUN = extra)
lagged.variables <- dplyr::bind_cols(lagged.variables)
instances <- cbind(instances, lagged.variables)


# fit
condition <- !is.na(instances$rain_lag_2) | !is.na(instances$mint_lag_2) | !is.na(instances$maxt_lag_2)
excerpt <- instances[condition, ]
fit2.5 <- fit.matern(form =
                       as.formula(log(Cases) ~ time + I(pmax(time - 50, 0)) + I(time > 225)
                         + mint_lag_2 + maxt_lag_2 + rain_lag_2),
                     time = 'time',
                     start.cov.pars = c(1,5),
                     kappa = 2.5,
                     data = excerpt,
                     method = 'nlminb')


# model estimates
estimates <- summary(fit2.5, log.cov.pars = TRUE)


# the natural logarithm scale parameters
parameters <- data.frame(estimates$cov.pars)
parameters$interval <- qnorm(p = 0.975, lower.tail = TRUE) * parameters$StdErr
parameters[, c('ln_lower_ci', 'ln_upper_ci')] <- parameters$Estimate +
  matrix(data = parameters$interval) %*%  matrix(data = c(-1, 1), nrow = 1, ncol = 2)


# exponentials of ...
parameters[, c('lower_ci', 'upper_ci')] <- as.matrix(exp(parameters[, c('ln_lower_ci', 'ln_upper_ci')]))


# predictions
predictor <- time.predict(
  fitted.model = fit2.5,
  predictors = excerpt[, c('time', 'mint_lag_2', 'maxt_lag_2', 'rain_lag_2')],
  time.pred = excerpt$time,
  scale.pred = 'exponential')

PredictionsGraph(predictor = predictor)