#ARCH
library(quantmod)
library(rugarch)
library(forecast)
library(car)
library(FinTS)
library(PerformanceAnalytics)
library(ggplot2)

# download data
getSymbols("SPY")

spy <- na.approx(na.trim(CalculateReturns(Ad(SPY), method = "log")))

autoplot(spy) + theme_bw()


acfpl <- acf(spy, plot=FALSE, lag.max = 24)

par(mfrow=c(2,1))
plot(acfpl, xlab="Lag", main="", col="red",pch=1,ylab="")

pacfpl <- pacf(spy, plot=FALSE, lag.max = 24)

plot(pacfpl, xlab="Lag", main="", col="red",pch=1,ylab="")


garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(1,0)))

spy.garch11.fit <- ugarchfit(spec=garch11.spec, data=spy)

spy.garch11.fit

# news impact
plot(spy.garch11.fit, which=12)

signbias(spy.garch11.fit)

# Nelson's egarch model
egarch11.spec = ugarchspec(variance.model=list(model="eGARCH",
                                               garchOrder=c(1,1)),
                           mean.model=list(armaOrder=c(0,0)))
spy.egarch11.fit = ugarchfit(egarch11.spec, spy)
spy.egarch11.fit

# GJR garch model
gjrgarch11.spec = ugarchspec(variance.model=list(model="gjrGARCH",
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0)))
spy.gjrgarch11.fit = ugarchfit(gjrgarch11.spec, spy)
spy.gjrgarch11.fit

# aparch models
aparch11.1.spec = ugarchspec(variance.model=list(model="apARCH",
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(0,0)),
                             fixed.pars=list(delta=1))

spy.aparch11.1.fit = ugarchfit(aparch11.1.spec, spy)
spy.aparch11.1.fit

# garch-m
garchm1.1.spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(0,0), archm=TRUE,archpow=1),
                             fixed.pars=list(delta=1))

spy.garchm1.1.fit = ugarchfit(garchm1.1.spec, spy)
spy.garchm1.1.fit

nic.garch11 = newsimpact(spy.garch11.fit)
nic.egarch11 = newsimpact(spy.egarch11.fit)
nic.gjrgarch11 = newsimpact(spy.gjrgarch11.fit)
nic.aparch11.1 = newsimpact(spy.aparch11.1.fit)
nic.garchm1.1. = newsimpact(spy.garchm1.1.fit)

# compare information criteria
model.list = list(garch11 = spy.garch11.fit,
                  egarch11 = spy.egarch11.fit,
                  gjrgarch11 = spy.gjrgarch11.fit,
                  aparch11.1 = spy.aparch11.1.fit)
info.mat = sapply(model.list, infocriteria)
rownames(info.mat) = rownames(infocriteria(spy.garch11.fit))
info.mat