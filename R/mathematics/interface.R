# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 26/02/2022


# part of Exercise 3, cf. main.R, etc.

phi <- uniroot(function (x) matern(u = 1, phi = x, kappa = 1.5) - 0.5,
               lower = 0.1, upper = 10)$root
matern(u = 1, phi = phi, kappa = 0.5)

phi <- -1/log(0.5)
exp(-1/phi)

y_0.5 <- simulate.matern(start=1,end=100,
                         mu=0,
                         sigma2=1,
                         phi=phi,
                         tau2=0,
                         kappa=0.5)

acf(y_0.5)
vari_0.5 <- vari.time(time = 1:100, data = y_0.5, uvec = seq(1, 20, length = 15))
plot(vari_0.5, type = 'l')