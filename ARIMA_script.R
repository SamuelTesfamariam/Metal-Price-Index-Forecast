library(quantmod)
library(car)
library(tseries)
library(forecast)
library(urca)
library(lmtest)


start <- as.Date("1992-01-01")
end <- as.Date("2022-02-01")
getSymbols("PMETAINDEXM", src = "FRED", from = start, to = end)

PMETAINDEXM <- PMETAINDEXM[1:362]
PMETAINDEXM <- ts(PMETAINDEXM[,"PMETAINDEXM"],start=c(1992,1), frequency=12)
plot.ts(PMETAINDEXM)

adf <- ur.df(log(PMETAINDEXM), type="trend", lags=12) # type: "none", "drift", "trend"
summary(adf)

diff.adf <- ur.df(diff(log(PMETAINDEXM)), type="trend", lags=2) # type: "none", "drift", "trend"
summary(diff.adf)

plot.ts(diff(log(PMETAINDEXM)))

#######################Series transformation##################
Metal1<-PMETAINDEXM
Metal2<-log(PMETAINDEXM)
Metal <- diff(log(PMETAINDEXM))
plot.ts(Metal)
acf(Metal)
pacf(Metal)


########################ARIMA(1,1,0)################
result1 <- arima(Metal2[1:350], order=c(1,1,0))
summary(result1)
coeftest1<- coeftest(result1)
summary(coeftest1)
Metal.fitted.1 <- fitted(result1) #result1$fitted.values
Metal.resid.1 <- result1$residuals

Metal_1 <- ts(Metal2[5:350],start=c(1992,1),frequency=12)
Metal.fitted.1 <- ts(Metal.fitted.1[5:350],start=c(1992,1),frequency=12)
Metal.resid.1 <- ts(Metal.resid.1[5:350],start=c(1992,1),frequency=12)

plot(Metal_1,type="l", yaxt='n', ylim=c(3,6), xlab="",ylab="Log(PMETAINDEXM)",col="black", main="ARIMA(1,1,0)")
lines(Metal.fitted.1,col="red",lty=2)
axis(side=2,at=seq(3,6, by=1),lab=seq(3,6, by=1),cex.axis=0.8)
par(new=T)
plot(Metal.resid.1,col="blue", axes=FALSE, ylim=c(-1.5,1.5), xlab="",ylab="")
abline(h=0,col="black")
abline(h=0.1,col="black",lty="dashed")
abline(h=-0.1,col="black",lty="dashed")
axis(side=4,at=seq(-0.3,0.3, by=0.1),lab=seq(-0.3,0.3, by=0.1) ,cex.axis=0.6)
legend("topleft",c("Residual","Actual","Fitted"),lty=c(1,1),col=c("blue","black","red"),cex=0.6)

acf(Metal.resid.1)
pacf(Metal.resid.1)


####################Forecasting using model##########################
AR1.predict <- predict(arima(Metal2[1:350]), order=c(1,1,1), n.ahead = 12)
AR1.forecast <- AR1.predict$pred

AR1.Forecast.lower <- NULL
AR1.Forecast.upper <- NULL

for (i in seq_along(AR1.forecast)) {
  AR1.Forecast.lower[36+i] <- AR1.forecast[i]-(AR1.predict$se[i]*1.96)
  AR1.Forecast.upper[36+i] <- AR1.forecast[i]+(AR1.predict$se[i]*1.96)
}

Metal.history <- Metal2[314:362]
Metal.history <- as.ts(Metal.history, frequency=12)

Metal.forecast <- NULL
for (j in seq_along(AR1.forecast)) {
  Metal.forecast[36+j] <- AR1.forecast[j]
}
Metal.forecast <- as.ts(Metal.forecast, frequency=12)

par(mfrow=c(1,1))
plot(Metal.history, type="l",xlim=c(0,48),ylim=c(0,10), ylab="Unemployment Rate",xlab="Time",axes=F, main="Unemployment Rate History and Forecast ARMA(2,1) Model")
lines(Metal.forecast, col="blue")
lines(AR1.Forecast.lower,col="red",lty=2)
lines(AR1.Forecast.upper,col="red",lty=2)
box()
axis(side=1, at=seq(0,48, by=4), lab=seq(0,48, by=4)) #,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37))
axis(side=2, at=seq(0,10,by=2))
abline(v=37,lty="dashed")













