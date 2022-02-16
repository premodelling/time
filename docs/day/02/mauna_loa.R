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
