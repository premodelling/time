# Title     : ConfidenceInterval.R
# Objective : Confidence Interval
# Created by: greyhypotheses
# Created on: 24/02/2022


#'
#' @param estimates: summary( linear.model.MLE() object, log.cov.pars = ... )
#' @param log.cov.pars: are the estimates ln(estimates)?
#'
ConfidenceInterval <- function (estimates, log.cov.pars = TRUE) {


  # the σ*σ, ϕ, and τ*τ estimaates
  parameters <- data.frame(estimates$cov.pars)
  names(parameters) <- c('estimate', 'StdErr')


  # their confidence intervals
  parameters$interval <- qnorm(p = 0.975, lower.tail = TRUE) * parameters$StdErr
  parameters[, c('lower_ci', 'upper_ci')] <- parameters$estimate +
    matrix(data = parameters$interval) %*%  matrix(data = c(-1, 1), nrow = 1, ncol = 2)


  # if the estimates are natural logarithm estimates, calculate their normal forms
  if (log.cov.pars) {
    parameters[, c('exp(estimate)', 'exp(lower_ci)', 'exp(upper_ci)')] <-
      as.matrix(exp(parameters[, c('estimate', 'lower_ci', 'upper_ci')]))
  }


  # exclude irrelevant fields
  parameters <- parameters %>%
    select(!c('StdErr', 'interval'))

  return(parameters)


}
