
# y: data of the outcome to model
# D: matrix of covariates (use x=TRUE to obtain this from the linear model)
mle <- function(y,D) {
  p <- ncol(D) # number of covariates (including intercept)
  n <- nrow(D) # number of observations
 
  llik <- function(par) {
    beta <- par[1:p]
    phi <- 2*exp(par[p+1])/(1+exp(par[p+1]))-1
    sigma2 <- exp(par[p+2])
    
    llik <- rep(NA,n)
    
    # Write the log-density function of Y1
    llik[1] <- 
      
    for(i in 2:n) {
      
      # Write the log-density function of Yt given Yt-1
      # llik[i] <-
        
    }
    sum(llik)
  }
  # nlimnb(rep(0,p+2), function(x) -llik(x))
}