rm(list=ls())

source('docs/programme/mathematics/auxiliary_function.R')
mal <- read.csv('docs/programme/kericho/kericho.csv')

mal$Month <- factor(mal$Month, 
                        levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                 "Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
mal$t <- as.numeric(mal$Month) + 12*(mal$Year-1979)

lm1 <- lm(log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6), data=mal)


vari.lm1 <- vari.time(time=mal$t,data=residuals(lm1),
       uvec=seq(1,12,length=15))
range(dist(mal$t))

plot(vari.lm1,type="l")

env <- vari.env(vari.lm1,time=mal$t,data=residuals(lm1),
                nsim=10000)

eyefit(vari.lm1)

matplot(env$u,cbind(env$v.lower,vari.lm1$v,env$v.upper),type="l",
        col=1,lty=c("dashed","solid","dashed"),
        ylim=c(0,0.3),
        xlab="Time separation (months)",
        ylab="Variogram")
plot(residuals(lm1),xlab = "time (months, 1=January 1979)",
     ylab="residuals",type="l")

# Two options: include or NOT include the noise Z(t)
fit0.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                   time="t",
                   # start.cov.pars=c(phi,tau2/sigma2)
                   start.cov.pars = 0.65,
                   # Set tau^2=0, meaning remove Z(t) from the model
                   fixed.rel.nugget=0,
                   kappa=0.5,
                   data=mal,
                   method="nlminb")
summary(fit0.5,log.cov.pars=FALSE)

# exp(-h.star/phi.hat)=0.05 
phi.hat <- coef(fit0.5)["phi"]
h.star <- -phi.hat*log(0.05)

fit1.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                     time="t",
                     start.cov.pars = c(2.59,0.05/0.16),
                     kappa=1.5,
                     data=mal,
                     method="nlminb")
summary(fit1.5,log.cov.pars=TRUE)

phi.hat <- coef(fit1.5)["phi"]
# x is the time separation
h.star <- 
uniroot(function(x) matern(x,phi=phi.hat,kappa=1.5)-0.05,
         lower=0.1,upper=12)$root
  


fit2.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                     time="t",
                     start.cov.pars = c(1,5),
                     kappa=2.5,
                     data=mal,
                     method="nlminb")
summary(fit2.5,log.cov.pars=FALSE)

c(fit0.5$log.lik,
  fit1.5$log.lik,
  fit2.5$log.lik)

pred2.5 <- time.predict(fitted.model=fit2.5,
                        predictors = data.frame(t=seq(1,310,1)),
                        time.pred = seq(1,310,1),
                        scale.pred = "exponential")

matplot(pred2.5$time.pred,log(cbind(pred2.5$predictions,pred2.5$quantiles)),
        lty=c("solid","dashed","dashed"),type="l",col=1)
