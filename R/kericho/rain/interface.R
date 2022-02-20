# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 19/02/2022


# external functions
source(file = 'R/kericho/StudyData.R')
source(file = 'R/kericho/functions/TimeDependentLag.R')
source(file = 'R/kericho/rain/Graphs.R')
source(file = 'R/kericho/rain/Regression.R')


# data
instances <- StudyData()


# lagged rainfall series
temporary <- TimeDependentLag(frame = instances, frame.date = 'date',
                              frame.date.granularity = 'month', frame.focus = 'Rain', lags = seq(from = 0, to = 4) )
head(temporary$frame[temporary$lagfields])


# graphs of ln(cases) vs. lagged rainfall series
RainSeriesGraphs(data = temporary$frame, lagfields = temporary$lagfields)


# correlation between ln(cases) and each lagged rainfall series
correlation <- function (variable) {
  y <- cor(x = temporary$frame$CasesLN, y = temporary$frame[variable], use = 'complete.obs', method = 'pearson') %>%
    data.frame()
}
correlations <- dplyr::bind_cols(lapply(X = temporary$lagfields, FUN = correlation))
row.names(correlations) <- 'correlation'
correlations


# regression
regression <- Regression(data = temporary$frame, lagfields = temporary$lagfields)
merge(x = regression, y = t(correlations), by = 0)
