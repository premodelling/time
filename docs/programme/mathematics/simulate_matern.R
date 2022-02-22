simulate.matern <- function(start,end,mu,sigma2,phi,kappa,tau2) {
  t.set <- seq(start,end,by=1)
  n <- length(t.set)
  library(geoR)
  Sigma <- sigma2*matern(as.matrix(dist(t.set)),phi=phi,kappa=kappa)
  diag(Sigma) <- diag(Sigma)+tau2
  out <- as.numeric(mu+t(chol(Sigma))%*%rnorm(n))
  out
}