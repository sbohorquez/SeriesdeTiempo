library(tidyverse)
library(readxl)
library(fUnitRoots)
library(tseries)
library(MASS)
library(forecast)
library(ggplot2)

rm(list = ls())

setwd("D:/OneDrive - Universidad EAFIT/EAFIT/Docencia/Series de Tiempo/2021 - 1")

USMacro <- read_excel("us_macro_quarterly.xlsx")

GDP <- ts(USMacro$GDPC96,
          start = c(1957,1),
          end = c(2013,4),
          frequency = 4)

plot(GDP)

kpss.test(GDP) #H0 es estacionario
pp.test(GDP) #H0 es NO estacionario
unitrootTest(GDP, lags = 4) #H0 es No estacionario

GDPDiff <- diff(GDP)
plot(GDPDiff)

bc <- boxcox(GDP ~ 1)
lambda <- bc$x[which.max(bc$y)]

GDPGrowth <- diff(log(GDP))

plot(GDPGrowth)

kpss.test(GDPGrowth) #H0 es estacionario
pp.test(GDPGrowth) #H0 es NO estacionario
unitrootTest(GDPGrowth, lags = 4) #H0 es No estacionario

face <- acf(GDPGrowth, plot = FALSE, lag.max = 20)
facpe <- pacf(GDPGrowth, plot = FALSE, lag.max = 20)

plot(face, xlab="Lag", col="red")
plot(facpe, xlab="Lag", col="red")

arma12 <- arima(GDPGrowth, order = c(1,0,2))
arma12
# 
# arma12MCO <- arima(GDPGrowth, order = c(1,0,2), method = "CSS")
# arma12MCO

ar1 <- arima(GDPGrowth, order = c(1,0,0))
ar1

AIC(arma12)
AIC(ar1)

BIC(arma12)
BIC(ar1)

#Escojo el AR1 por ahora.

#Prueba residuales

white.test(ar1$residuals)

plot(ar1$residuals, type = 'p')

face <- acf(ar1$residuals, plot = FALSE, lag.max = 20)
facpe <- pacf(ar1$residuals, plot = FALSE, lag.max = 20)

plot(face, xlab="Lag", col="red")
plot(facpe, xlab="Lag", col="red")



#Prediccion

ventana <- floor(0.85*length(GDPGrowth))
j<- length(GDPGrowth) - ventana

ar1pred <- arima(GDPGrowth[1:ventana], order = c(1,0,0))
ar1pred

Predar1 <- forecast(ar1pred,j)

df1 <- GDPGrowth[(ventana+1):227]
df1 <- as.data.frame(df1)
names(df1) <- "GDPGrowth"

df1 <- df1 %>%
       mutate(Pred = as.numeric(Predar1$mean),
              low = as.numeric(Predar1$lower[,2]),
              high = as.numeric(Predar1$upper[,2]),
              t = seq(1:35))

ggplot(df1) +
  geom_line(aes(x=t, y=GDPGrowth, colour = "GDP")) +
  geom_line(aes(x=t, y=Pred, colour = "Pred")) +
  geom_line(aes(x=t, y=low, colour = "IC")) +
  geom_line(aes(x=t, y=high, colour = "IC")) +
  scale_color_manual("",
                     breaks = c("GDP","Pred","IC"),
                     values = c("black","blue","red"))







