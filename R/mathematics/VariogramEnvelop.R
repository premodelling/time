# Title     : VariogramEnvelop.R
# Objective : TODO
# Created by: greyhypotheses
# Created on: 22/02/2022


VariogramEnvelop <- function (geodata, variogram, nenvelops, nsim) {


  # envelops
  upper <- list()
  lower <- list()
  for (j in seq(from = 1, to = nenvelops)) {
    envelops <- variog.mc.env(geodata = geodata, obj.variog = variogram, nsim = nsim)
    upper[[j]] <- envelops$v.upper
    lower[[j]] <- envelops$v.lower
  }
  upper <- do.call(cbind, upper)
  lower <- do.call(cbind, lower)


  # statistics
  frame <- data.frame(distance = envelops$u)

  frame$upper.mean <- rowMeans(x = upper)
  frame$upper.min <- rowMins(x = upper)
  frame$upper.max <- rowMaxs(x = upper)

  frame$lower.mean <- rowMeans(x = lower)
  frame$lower.min <- rowMins(x = lower)
  frame$lower.max <- rowMaxs(x = lower)

  return(frame)

}