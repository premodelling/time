# Title     : Regression.R
# Objective : Univariate linear regression analysis
# Created by: greyhypotheses
# Created on: 17/02/2022


#' Univariate linear regression analysis of the lagged rain fields w.r.t. ln(cases)
#'
#' @param data: a Kericho data set that includes cases, and lagged rain fields
#' @param lagfields: the lagged rain fields
#'
Regression <- function (data, lagfields) {

  univariate <- function (lagfield) {

    fit <- lm(as.formula(object = paste0('log(Cases) ~ ', lagfield)), data = data, na.action = na.omit)
    outline <- summary(object = fit)

    data.frame(coefficient = outline$coefficients[lagfield, 'Estimate'],
               std_error = outline$coefficients[lagfield, 'Std. Error'],
               p_value = outline$coefficients[lagfield, 'Pr(>|t|)'], row.names = lagfield)
  }
  
  estimates <- lapply(X = lagfields, FUN = univariate)
  estimates <- dplyr::bind_rows(estimates)

  return(estimates)
  
}