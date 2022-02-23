# Title     : main.R
# Objective : main
# Created by: greyhypotheses
# Created on: 21/02/2022


source('R/mathematics/MaternCorrelations.R')
source('R/mathematics/MaternSpecialCase.R')


RHO <- c(0.25, 0.5, 0.75, 0.98)
PHI <- c(0.1, 0.3, 0.5, 0.75)

mu <- 0
sigmasqr <- 1
kappa <- 0.5
tausqr <- 0
limit <- 1



X <- MaternCorrelations(limit = limit, by = 0.01, kappa = kappa, mu = mu, sigmasqr = sigmasqr, tausqr = tausqr, PHI = PHI)
estimates <- X$estimates$estimates
data <- X$data$data
estimates$phi <- factor(estimates$phi)
data$phi <- factor(data$phi)

ggplot(data = estimates, mapping = aes(x = distance, y = estimate, group = phi, colour = phi, fill = phi)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
  geom_point(alpha = 0.65, size = 1) +
  facet_wrap(~ phi) +
  theme_minimal() +
  theme(panel.spacing = unit(x = 2, units = 'lines'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
        strip.text.x = element_text(size = 11, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
        axis.title.x = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13, face = 'bold'), axis.text.y = element_text(size = 11) ) +
  xlab(label = '\ndistance\n') +
  ylab(label = '\nvariogram\n') +
  labs(title = TeX(paste('$\\kappa \\;$', ' =', kappa))) +
  xlim(0, 0.75)

ggplot(data = data, mapping = aes(x = abscissa, y = series, colour = phi)) +
  geom_line(size = 0.2) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
        axis.title.x = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13, face = 'bold'), axis.text.y = element_text(size = 11) ) +
  labs(title = TeX(paste('$\\kappa \\;$', ' =', kappa)))





Y <- MaternSpecialCase(limit = limit, by = 0.01, mu = mu, sigmasqr = sigmasqr, tausqr = tausqr, RHO = RHO)
estimates <- Y$estimates$estimates
data <- Y$data$data
estimates$rho <- factor(estimates$rho)
data$rho <- factor(data$rho)

ggplot(data = estimates, mapping = aes(x = distance, y = estimate, group = rho, colour = rho, fill = rho)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = `estimate.lower`, ymax = `estimate.upper`), alpha = 0.3, linetype = 0) +
  geom_point(alpha = 0.65, size = 2) +
  facet_wrap(~ rho) +
  theme_minimal() +
  theme(panel.spacing = unit(x = 2, units = 'lines'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
        strip.text.x = element_text(size = 11, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
        axis.title.x = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13, face = 'bold'), axis.text.y = element_text(size = 11) ) +
  xlab(label = '\ndistance\n') +
  ylab(label = '\nvariogram\n') +
  labs(title = TeX(paste('$\\kappa \\;$', ' =', kappa)))


ggplot(data = data, mapping = aes(x = abscissa, y = series, colour = rho)) +
  geom_line(size = 0.2) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.05),
        plot.title = element_text(hjust = 0.25, size = 13, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
        axis.title.x = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13, face = 'bold'), axis.text.y = element_text(size = 11) ) +
  labs(title = TeX(paste('$\\kappa \\;$', ' =', kappa)))







