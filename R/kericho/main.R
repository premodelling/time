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
source(file = 'R/kericho/problems/fourth/PredictionsGraph.R')

source(file = 'docs/programme/mathematics/auxiliary_function.R')



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



# NaN
condition <- !is.na(instances$rain_lag_2) | !is.na(instances$mint_lag_2) | !is.na(instances$maxt_lag_2)
excerpt <- instances[condition, ]

# Splitting
quantity <- 12
splitdate <- max(excerpt$date) %m-% months(x = quantity)
training <- excerpt[ excerpt$date <= splitdate, ]
testing <- excerpt[ excerpt$date > splitdate, ]

# fit
fit2.5 <- fit.matern(form =
                       as.formula(log(Cases) ~ time + I(pmax(time - 50, 0)) + I(time > 225)
                         + mint_lag_2 + maxt_lag_2 + rain_lag_2),
                     time = 'time',
                     start.cov.pars = c(1,5),
                     kappa = 2.5,
                     data = training,
                     method = 'nlminb')


# model estimates
estimates <- summary(fit2.5, log.cov.pars = TRUE)


# confidence intervals


# predictions
predictor <- time.predict(
  fitted.model = fit2.5,
  predictors = testing[, c('time', 'mint_lag_2', 'maxt_lag_2', 'rain_lag_2')],
  time.pred = testing$time,
  scale.pred = 'linear')

PredictionsGraphLinear(predictor = predictor, original = testing$CasesLN)


# bias, error
bias <- mean(predictor$predictions -  testing$CasesLN)
rmse <- sqrt( sum((predictor$predictions -  testing$CasesLN)^2) / nrow(testing) )



