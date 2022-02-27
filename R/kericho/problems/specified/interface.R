# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 24/02/2022



# external functions
source(file = 'R/kericho/StudyData.R')
source(file = 'R/functions/TimeDependentLag.R')
source(file = 'R/functions/ConfidenceInterval.R')
source(file = 'docs/programme/mathematics/auxiliary_function.R')
source(file = 'R/kericho/problems/specified/PredictionsGraph.R')


# data
instances <- StudyData()
str(instances)


# lagged series
variables <- c('minT', 'maxT', 'Rain')
lags <- seq(from = 2, to = 2)
T <- TimeDependentLag(
  frame = instances, frame.date = 'date', frame.date.granularity = 'month',
  variables = variables, lags = lags)
instances <- T$frame

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
intervals <- ConfidenceInterval(estimates = estimates, log.cov.pars = TRUE)
intervals



# predictions
predictor <- time.predict(
  fitted.model = fit2.5,
  predictors = excerpt[, c('time', 'mint_lag_2', 'maxt_lag_2', 'rain_lag_2')],
  time.pred = excerpt$time,
  scale.pred = 'exponential')

PredictionsGraphNaturalLog(predictor = predictor, original = excerpt$CasesLN)
