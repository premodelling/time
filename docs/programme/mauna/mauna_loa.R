rm(list=ls())

co2 <- read.csv("mauna_loa.csv")

str(co2)

co2$t <- co2$Month+12*(co2$Year-1965)

plot(co2$t,co2$CO2,type="l")

lm.fit1 <- lm(CO2 ~ t + 
                    sin(2*pi*t/12)+cos(2*pi*t/12),
              data=co2)

summary(lm.fit1)

beta.hat.lm1 <- coef(lm.fit1)

year.t <- 1:12
seasonality.lm1 <- beta.hat.lm1[3]*sin(2*pi*year.t/12)+
               beta.hat.lm1[4]*cos(2*pi*year.t/12)

plot(year.t,seasonality.lm1,type="l")
which.max(seasonality.lm1)
which.min(seasonality.lm1)

resid.lm1 <- residuals(lm.fit1)
plot(co2$t,resid.lm1,type="l")

acf(resid.lm1,lag.max = 40)


lm.fit2 <- lm(CO2 ~ t + 
              sin(2*pi*t/12)+cos(2*pi*t/12)+
              sin(2*pi*t/6)+cos(2*pi*t/6),
              data=co2)

beta.hat.lm2 <- coef(lm.fit2)

seasonality.lm2 <- beta.hat.lm2[3]*sin(2*pi*year.t /12)+
                   beta.hat.lm2[4]*cos(2*pi*year.t /12)+
                   beta.hat.lm2[5]*sin(2*pi*year.t /6)+
                   beta.hat.lm2[6]*cos(2*pi*year.t /6)

matplot(year.t,cbind(seasonality.lm1,seasonality.lm2),
        type="l",col=1:2,lwd=2)  

resid.lm2 <- residuals(lm.fit2)

plot(co2$t,resid.lm2,type="l")
acf(resid.lm2,lag.max = 40)

lm.fit3 <- lm(CO2 ~ t + 
                sin(2*pi*t/12)+cos(2*pi*t/12)+
                sin(2*pi*t/6)+cos(2*pi*t/6)+
                sin(2*pi*t/3)+cos(2*pi*t/3),
              data=co2)

beta.hat.lm3 <- coef(lm.fit3)

seasonality.lm3 <- beta.hat.lm2[3]*sin(2*pi*year.t /12)+
  beta.hat.lm3[4]*cos(2*pi*year.t /12)+
  beta.hat.lm3[5]*sin(2*pi*year.t /6)+
  beta.hat.lm3[6]*cos(2*pi*year.t /6)+
  beta.hat.lm3[7]*sin(2*pi*year.t /3)+
  beta.hat.lm3[8]*cos(2*pi*year.t /3)

matplot(year.t,cbind(seasonality.lm2,seasonality.lm3),
        type="l",col=1:2,lwd=2)  

summary(lm.fit3)

###
lm.fit0 <- lm(CO2 ~ t ,
              data=co2)
anova(lm.fit0,lm.fit1)


### Piecewise constant

co2$season <- rep(NA)
co2$season[co2$Month==12 | (co2$Month>=1 & co2$Month<=2)] <- "Winter"
co2$season[co2$Month>=3 & co2$Month<=5] <- "Spring"
co2$season[co2$Month>=6 & co2$Month<=9] <- "Summer"
co2$season[co2$Month==10 | co2$Month==11] <- "Autumn"

lm.fit4 <- lm(CO2 ~ t + 
                season,
              data=co2)

summary(lm.fit4)

plot(co2$t,co2$CO2,type="l")
lines(co2$t,predict(lm.fit4),col=2)

seasonality.lm4 <- rep(NA,12)
beta.hat.lm4 <- coef(lm.fit4)
seasonality.lm4[c(1:2,12)] <- beta.hat.lm4[5]
seasonality.lm4[3:5] <- beta.hat.lm4[3]
seasonality.lm4[6:9] <- beta.hat.lm4[4]
seasonality.lm4[10:11] <- 0

matplot(year.t,cbind(seasonality.lm2,seasonality.lm4),
        type="l",col=1:2,lwd=2)  

acf(residuals(lm.fit4),lag.max = 40)

#### Fitting and AR1 for lm.fit2
# For an AR1, order=c(1,0,0)

?arima0
lm.fit2 <- lm(CO2 ~ t + 
                sin(2*pi*t/12)+cos(2*pi*t/12)+
                sin(2*pi*t/6)+cos(2*pi*t/6),
              data=co2,x=TRUE)

# Extract covariates and remove intercept
mat.cov <- lm.fit2$x[,-1]

ar1.fit <- 
arima0(x=co2$CO2,
       order=c(1,0,0),
       xreg = mat.cov)

# Likelihood ratio test
# To test the significance of temporal correlation
# Null hypothesis: phi=0
1-pchisq(-2*(as.numeric(logLik(lm.fit2))-ar1.fit$loglik),df=1)

acf(ar1.fit$residuals,lag.max = 40)

ar1.pred <- co2$CO2-ar1.fit$residuals

plot(co2$t,co2$CO2,type="l")
lines(co2$t,ar1.pred,col=2)


