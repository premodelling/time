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

  # excercise 2.1: time vs variable
  ggplot(data = data, mapping = aes(x = time, y = !!sym(variable))) +
    geom_point(alpha = 0.25) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', col = 'green') +
    theme_minimal()
}


#'
#' for exercise 2.3a & exercise 2.3b
#'
#' @param variable: the variable being modelled
#' @param expr: the right hand side of the lm() formula, ic
#'
ModellingAlgorithm <- function(variable, expr) {

  data <- ModellingData(variable = variable)
  model <- lm( as.formula(paste0(variable, ' ~ ', expr)), data = data, x = TRUE)
  estimates <- cbind(data[,c(variable, 'time')], prediction = model$fitted.values)

  return(list(model = model, estimates = estimates))
}
