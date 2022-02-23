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
summary(fit2.5, log.cov.pars = TRUE)







