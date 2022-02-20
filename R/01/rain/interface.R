# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 19/02/2022

#' Part 1
#'


# external functions
source(file = 'R/01/StudyData.R')
source(file = 'R/01/ExplorationGraph.R')
source(file = 'R/functions/TimeDependentLag.R')
source(file = 'R/01/rain/Rainfall.R')
source(file = 'R/01/rain/Regression.R')


# data
instances <- StudyData()


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