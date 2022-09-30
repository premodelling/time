# Title     : main.R
# Objective : Main
# Created by: greyhypotheses
# Created on: 03/03/2022



wine <- read.csv(file = 'data/aus_wine_sales.csv', header = FALSE)
wine.ts <- ts(wine[,2], start = c(1985, 1), frequency = 4)

par(mfrow = c(3, 1), mar = c(4, 4, 4, 4))
plot(wine.ts, type = 'l', xlab = 'Year', ylab = 'Sales in thousand litres', frame.plot = FALSE)

acf(wine[,2], lag.max = 25, xlab = 'Lag', ylab = 'ACF', main = '', frame.plot = FALSE)
acf(wine[,2], lag.max = 25, type = 'partial', xlab = 'Lag', ylab = 'Partial ACF', main = '', frame.plot = FALSE)



dwine <- diff(wine[,2], lag = 4)

par(mfrow = c(3, 1), mar = c(4, 4, 4, 4))
plot(diff(wine.ts, lag = 4), type = 'l', xlab = 'Year', ylab = 'Differenced Series', frame.plot = FALSE)

acf(dwine, lag.max = 25, xlab = 'Lag', ylab = 'ACF', main = '', frame.plot = FALSE)
acf(dwine, lag.max = 25, type = 'partial', xlab = 'Lag', ylab = 'Partial ACF', main = '', frame.plot = FALSE)