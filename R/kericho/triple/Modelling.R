# Title     : Modelling.R
# Objective : Model Selection
# Created by: greyhypotheses
# Created on: 16/02/2022


source(file = 'R/01/StudyData.R')


ModellingData <- function (variable) {

  instances <- StudyData()
  data <- instances[!is.na(instances[variable]),]

  return(data)
}

ModellingDataGraph <- function (variable) {

  data <- ModellingData(variable = variable)

  # excercise 2.1: time vs variable
  ggplot(data = data, mapping = aes(x = time, y = !!sym(variable))) +
    geom_point(alpha = 0.25) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', col = 'green') +
    theme_minimal()
}


ModellingSeasonSingle <- function (variable) {

  data <- ModellingData(variable = variable)

  # excercise 2.2a
  # $Y_{i} = \beta_{0} + \beta_1 x_{i} + \beta_2 sin(2 pi x_{i}/12) + \beta_3 cos(2 pi x_{i}/12) + Z_{i}$

  # exercise 2.2b: the linear regression model that accounts for seasonal trend, for a season period of 1 year
  expr <- ' ~ sin(2*pi*time/12) + cos(2*pi*time/12)'
  model <- lm(as.formula( paste0(variable, expr ) ), data = data, x = TRUE)

  # exercise 2.2c: original & predictions
  estimates <- cbind(data[,c(variable, 'time')], annual = model$fitted.values)

  return(list(model = model, estimates = estimates))

}


ModellingSeasonDouble <- function (variable, estimates) {

  data <- ModellingData(variable = variable)

  # exercise 2.3a
  expr <- ' ~ sin(2*pi*time/12) + cos(2*pi*time/12) + sin(2*pi*time/6) + cos(2*pi*time/6)'
  model <- lm( as.formula(paste0(variable, expr)), data = data, x = TRUE)

  # exercise 2.3b
  estimates <- cbind(estimates, biannual = model$fitted.values)

  return(list(model = model, estimates = estimates))

}


ModellingAutocorrelogram <- function (estimates, models) {

  # exercise 2.3c
  residues <- list(single = residuals(models$single),
                   double = residuals(models$double))

  par(mfrow = c(1, 2))
  plot(estimates$time, residues$single, type = "l", frame.plot = FALSE)
  acf(residues$single, frame.plot = FALSE)

  par(mfrow = c(1, 2))
  plot(estimates$time, residues$double, type = "l", frame.plot = FALSE)
  acf(residues$double, frame.plot = FALSE)

}