rm(list=ls())
kericho <- read.csv("Kericho_data.csv")
kericho$Month <- factor(kericho$Month, 
                        levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                 "Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
str(kericho)
kericho$time <- as.numeric(kericho$Month) + 12*(kericho$Year-1979)

lm1 <- lm(log(Cases) ~ sin(2*pi*time/12)+cos(2*pi*time/12)+
            sin(2*pi*time/6)+cos(2*pi*time/6)+
            time+I((time-50)*(time>50))+
            I(time>229), data=kericho)
summary(lm1)

#pdf("Malcases_vcap.pdf",width = 8, height = 8)
par(mfrow=c(2,1))
plot(kericho$time,log(kericho$Cases),type="l",
     xlab = "time (months, 1=January 1979",
     ylab = "log(cases)")
for(i in 1:max(kericho$time)) if(i%%12==0) abline(v=i,lty="dotted")
lines(predict(lm1),col=2)

plot(fitted(lm1),residuals(lm1),
     ylab="Residuals",xlab="Fitted")
#dev.off()

unique(kericho[,c("Year","Month")])



library(geoR)
v1 <- variog(coords=cbind(kericho$t,1),data=residuals(lm1),
             uvec=seq(0,20,by=1))
plot(v1)

env <- 
  variog.mc.env(coords=cbind(kericho$time,1),data=residuals(lm1),
                obj.variog = v1,nsim=1000)

#pdf("Kreicho_variog.pdf",width=8,height=8)
par(mfrow=c(2,1))
matplot(env$u,cbind(env$v.lower,v1$v,env$v.upper),type="l",
        col=1,lty=c("dashed","solid","dashed"),
        ylim=c(0,0.3),
        xlab="Time separation (months)",
        ylab="Variogram")
plot(residuals(lm1),xlab = "time (months, 1=January 1979)",
     ylab="residuals",type="l")
#dev.off()