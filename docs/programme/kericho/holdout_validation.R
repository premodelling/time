rm(list=ls())

source("auxiliary_function.R")
mal <- read.csv("Kericho.csv")


mal$Month <- factor(mal$Month, 
                    levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                             "Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)
mal$t <- as.numeric(mal$Month) + 12*(mal$Year-1979)

# year.holdout: is the number of years that are part of the holdout data-set
year.holdout <- 1
time.split <- max(mal$t)-year.holdout*12

plot(mal$t, log(mal$Cases),type="l")
abline(v=time.split,col=2)

# Identify the rows in the data-set that correspond to the training data-set
train <- which(mal$t<=time.split)
# Identify the rows in the data-set that correspond to the holdout data-set
holdout <- which(mal$t>time.split)


fit0.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                     time="t",
                     start.cov.pars = 1,
                     kappa=0.5,
                     fixed.rel.nugget = 0,
                     data=mal[train,], # NOTE: selecting the training data
                     method="nlminb")

fit1.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                     time="t",
                     start.cov.pars = c(1,5),
                     kappa=1.5,
                     data=mal[train,], # NOTE: selecting the training data
                     method="nlminb")

fit2.5 <- fit.matern(form=
                       log(Cases) ~ t+I((t-50)*(t>50))+I(t>229)+
                       sin(2*pi*t/12)+cos(2*pi*t/12)+
                       sin(2*pi*t/6)+cos(2*pi*t/6),
                     time="t",
                     start.cov.pars = c(1,5),
                     kappa=2.5,
                     data=mal[train,], # NOTE: selecting the training data
                     method="nlminb")


t.pred.holdout <- (time.split+1):max(mal$t)
pred0.5.holdout <- time.predict(fitted.model=fit0.5,
                                predictors = data.frame(t=t.pred.holdout),
                                time.pred = t.pred.holdout,
                                scale.pred = "linear")
pred1.5.holdout <- time.predict(fitted.model=fit1.5,
                            predictors = data.frame(t=t.pred.holdout),
                            time.pred = t.pred.holdout,
                            scale.pred = "linear")
pred2.5.holdout <- time.predict(fitted.model=fit2.5,
                                predictors = data.frame(t=t.pred.holdout),
                                time.pred = t.pred.holdout,
                                scale.pred = "linear")


matplot(pred0.5.holdout$time.pred,
        cbind(pred0.5.holdout$predictions,pred0.5.holdout$quantiles),
        lty=c("solid","dashed","dashed"),type="l",col=1,
        ylab="log(Cases)",xlab="Time")
points(mal[holdout,]$t,log(mal[holdout,]$Cases),pch=20)

matplot(pred1.5.holdout$time.pred,
        cbind(pred1.5.holdout$predictions,pred1.5.holdout$quantiles),
        lty=c("solid","dashed","dashed"),type="l",col=1,
        ylab="log(Cases)",xlab="Time")
points(mal[holdout,]$t,log(mal[holdout,]$Cases),pch=20)

matplot(pred2.5.holdout$time.pred,
        cbind(pred2.5.holdout$predictions,pred2.5.holdout$quantiles),
        lty=c("solid","dashed","dashed"),type="l",col=1,
        ylab="log(Cases)",xlab="Time")
points(mal[holdout,]$t,log(mal[holdout,]$Cases),pch=20)

# Bias

# kappa=0.5
mean((pred0.5.holdout$predictions-log(mal[holdout,]$Cases)))

# kappa=1.5
mean((pred1.5.holdout$predictions-log(mal[holdout,]$Cases)))

# kappa=2.5
mean((pred2.5.holdout$predictions-log(mal[holdout,]$Cases)))

# Root-mean-square error

# kappa=0.5
mean((pred0.5.holdout$predictions-log(mal[holdout,]$Cases))^2)

# kappa=1.5
mean((pred1.5.holdout$predictions-log(mal[holdout,]$Cases))^2)

# kappa=2.5
mean((pred2.5.holdout$predictions-log(mal[holdout,]$Cases))^2)
