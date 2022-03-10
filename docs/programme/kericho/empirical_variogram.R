rm(list=ls())

source("auxiliary_function.R")
mal <- read.csv("Kericho.csv")

mal$Month <- factor(mal$Month, 
                    levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                             "Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
mal$t <- as.numeric(mal$Month) + 12*(mal$Year-1979)

lm1 <- lm(log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
            sin(2*pi*t/12)+cos(2*pi*t/12)+
            sin(2*pi*t/6)+cos(2*pi*t/6), data=mal)


vari.lm1 <- vari.time(time=mal$t,data=residuals(lm1),
                      uvec=seq(1,20,length=15))
range(dist(mal$t))

plot(vari.lm1,type="l")

env <- vari.env(vari.lm1,time=mal$t,data=residuals(lm1),
                nsim=10000)

matplot(env$u,cbind(env$v.lower,vari.lm1$v,env$v.upper),type="l",
        col=1,lty=c("dashed","solid","dashed"),
        ylim=c(0,0.3),
        xlab="Time separation (months)",
        ylab="Variogram")
plot(residuals(lm1),xlab = "time (months, 1=January 1979)",
     ylab="residuals",type="l")