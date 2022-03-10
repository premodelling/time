# Title     : main.R
# Objective : Main
# Created by: greyhypotheses
# Created on: 16/02/2022


# the challenge question


# external functions
source(file = 'R/mauna/StudyData.R')
source(file = 'R/mathematics/SimulateMatern.R')
source(file = 'docs/programme/mathematics/auxiliary_function.R')



# the data
mauna <- StudyData()

ggplot(data = mauna, mapping = aes(x = time, y = CO2)) +
  geom_point(alpha = 0.65, size = 1) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05)) +
  xlab(label = '\ntime\n') +
  ylab(label = TeX(string = '$CO_{2}$'))



# foremost, the regression coefficiets via lm()
# note that the model's estimated y values can be obtained via
#   model$fitted.values
#   predict.lm(object = model, se.fit = FALSE)
#   model$x %*% matrix(data = model$coefficients, ncol = 1)
expr <- 'time + sin(2*pi*time/12) + cos(2*pi*time/12) + sin(2*pi*time/6) + cos(2*pi*time/6)'
model <- lm(formula = as.formula(object = paste0('CO2 ~ ', expr)), data = mauna, x = TRUE)



# next simulate a carbon dioxide series via Matern, and using the estimated
# y values of the lm() model
temporary <- function(scale, variance) {

  T <- SimulateMatern(start = 1, end = nrow(mauna), by = 1, mu = model$fitted.values, sigmasqr = 1,
                      phi = scale, kappa = 0.5, tausqr = variance)
  data <- mauna %>%
    mutate(simulated = T, phi = scale, tausqr = variance)

  return(list(data = data))
}
tensors <- mapply(FUN = temporary, scale = c(3.0, 3.0, 1.0, 1.0), variance = c(0.5, 1.0, 0.5, 1.0))
tensors <- dplyr::bind_rows(tensors)
tensors$phi <- as.factor(tensors$phi)
tensors$tausqr <- as.factor(tensors$tausqr)



# graphs of the simulated carbon dioxide series
caption <- "The light grey background curve is the observed $co_{2}$ series"

ggplot(data = tensors, mapping = aes(x = time, y = simulated, colour = phi)) +
  geom_line(data = mauna, mapping = aes(x = time, y = CO2), size = 1.25,
            colour = 'darkgrey', alpha = 0.60, linetype = 'solid') +
  geom_line(size = 0.5, alpha = 0.65) +
  geom_point(size = 0.5, alpha = 0.65) +
  facet_wrap(~tausqr, labeller = as_labeller(c(`0.5` = 'tausqr = 0.5', `1` = 'tausqr = 1'))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10, colour = 'grey'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05)) +
  xlab(label = '\ntime\n') +
  ylab(label = TeX(string = 'simulated $\\; co_{2}$ series')) +
  labs(caption = TeX(str_wrap(caption, width = 100) ))

ggplot(data = tensors, mapping = aes(x = time, y = simulated, colour = tausqr)) +
  geom_line(data = mauna, mapping = aes(x = time, y = CO2), size = 1.25,
            colour = 'darkgrey', alpha = 0.60, linetype = 'solid') +
  geom_line(size = 0.5, alpha = 0.65) +
  geom_point(size = 0.5, alpha = 0.65) +
  facet_wrap(~phi, labeller = as_labeller(c(`1` = 'phi = 1', `3` = 'phi = 3'))) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 10, colour = 'grey'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05)) +
  xlab(label = '\ntime\n') +
  ylab(label = TeX(string = 'simulated $\\; co_{2}$ series')) +
  labs(caption = TeX(str_wrap(caption, width = 100) ))



# modelling a simulated curve
modelling <- function (phi, tausqr) {

  sigmasqr <- 1
  kappa <- 0.5

  # append ...
  simulation <- tensors[ tensors$phi == phi & tensors$tausqr == tausqr,  c('date', 'simulated')]
  excerpt <- dplyr::left_join(x = mauna, y = simulation, by = 'date')

  # start.cov.pars = c(phi, tau2/sigma2), noting that Z(t) ~ N(0, tau2)
  T <- fit.matern(form = as.formula(object = paste0('simulated ~ ', expr)),
                  time = 'time',
                  start.cov.pars = c(phi, tausqr/sigmasqr),
                  kappa = kappa,
                  data = excerpt,
                  method = 'nlminb')

  return(T)
}
unique(tensors[, c('phi', 'tausqr')])
sample <- modelling(phi = 3, tausqr = 0.5)
coef(object = sample)
coef(object = model)




