# Title     : Modelling.R
# Objective : Model Selection
# Created by: greyhypotheses
# Created on: 16/02/2022


source(file = 'R/kericho/StudyData.R')


#'
#' @param variable: the variable being modellied
#'
ModellingData <- function (variable) {

  instances <- StudyData()
  data <- instances[!is.na(instances[variable]),]

  return(data)
}


#'
#' @param variable: the variable being modellied
#'
ModellingDataGraph <- function (variable) {

  data <- ModellingData(variable = variable)

  # time vs variable
  ggplot(data = data, mapping = aes(x = time, y = !!sym(variable))) +
    geom_point(alpha = 0.25) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', col = 'green') +
    theme_minimal()
}


#'
#' @param variable: the variable being modelled
#' @param expr: the right hand side of the lm() formula, ic
#'
ModellingLinear <- function(variable, expr) {

  data <- ModellingData(variable = variable)
  model <- lm( as.formula(paste0(variable, ' ~ ', expr)), data = data, x = TRUE)
  estimates <- cbind(data[,c(variable, 'time')], prediction = model$fitted.values)
  names(estimates)[names(estimates) == 'prediction'] <- paste0(variable, '_predicted')

  return(list(model = model, estimates = estimates))
}


#'
#' @param variable: the variable being modelled
#' @param expr: the right hand side of the lm() formula, ic
#'
ModellingAutoregressive <- function(variable, expr) {

  data <- ModellingData(variable = variable)


  # External regressors for arima() via linear regression model
  linear <- ModellingLinear(variable = variable, expr = expr)
  lrmodel <- linear$model
  X <- lrmodel$x[, -1]


  # ARIMA
  armodel <- arima(x = data[variable], order = c(1, 0, 0), xreg = X, method = 'ML')


  # ARIMA predictions via original - residuals
  # predictions <- data.frame(data[variable], residual = as.numeric(armodel$residuals))

  predictions <- data.frame(predicted = data[[variable]] - as.numeric(armodel$residuals))
  print(predictions)
  names(predictions) <- paste0(variable, '_predicted')
  estimates <- cbind(data[,c(variable, 'time')], predictions)

  return(list(model = armodel, estimates = estimates))

}














