# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 19/02/2022


# external functions
source(file = 'R/kericho/StudyData.R')
source(file = 'R/functions/TimeDependentLag.R')
source(file = 'R/kericho/problems/rainfall/Graphs.R')
source(file = 'R/kericho/problems/rainfall/Regression.R')


# data
instances <- StudyData()


# lagged rainfall series
dataset <- TimeDependentLag(frame = instances, frame.date = 'date',
                              frame.date.granularity = 'month', variables = 'Rain', lags = seq(from = 0, to = 4) )


# graphs of ln(cases) vs. lagged rainfall series
RainSeriesGraphs(data = dataset$frame, lagfields = dataset$laggedfields)


# correlation between ln(cases) and each lagged rainfall series
correlation <- function (variable) {
  y <- cor(x = dataset$frame$CasesLN, y = dataset$frame[variable], use = 'complete.obs', method = 'pearson') %>%
    data.frame()
}
correlations <- dplyr::bind_cols(lapply(X = dataset$laggedfields, FUN = correlation))
row.names(correlations) <- 'correlation'
correlations


# regression
regression <- Regression(data = dataset$frame, lagfields = dataset$laggedfields)
merge(x = regression, y = t(correlations), by = 0)
