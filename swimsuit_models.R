setwd("D:/NTU G suite (NTU Space)/01 ½å/ISS/BAFT/Final Project")

library(forecast)
library(ggplot2)

# read data
swim.data <- read.csv("BAFT_daily_paidorder.csv")

# transform date
as.Date(as.character(swim.data[ ,1]),"%Y-%m-%d") 

# convert to ts objects
swim.ts <- ts(swim.data[ ,-1], start = c(1,6), frequency = 7) # 7-28 is Sat

nSeries <- ncol(swim.ts)
sIndex <- 16 ## choose a series to analyze

data.ts <- swim.ts[ ,sIndex]

autoplot(data.ts)

### partition the series into train.ts and valid.ts
valid.ts <- subset(data.ts, start = length(data.ts)-7+1)
train.ts <- subset(data.ts, end = length(data.ts) - length(valid.ts))
nValid = length(valid.ts)

### fit train.ts to different models

## naive forecast
train.naive <- naive(train.ts, h = 7)

## snaive forecast
train.snaive <- snaive(train.ts, h = 7 * 1)

## ets
train.ets.auto <- ets(train.ts)
train.ets.auto.pred <- forecast(train.ets.auto, h = nValid, level = 0)
# summary(train.ets.auto)

## regression
# # trend
# train.lm.trend <- tslm(train.ts ~ trend)
# train.lm.trend.pred <- forecast(train.lm.trend, h = nValid, level = 0)
# summary(train.lm.trend)
# # seasonality
# train.lm.season <- tslm(train.ts ~ season)
# train.lm.season.pred <- forecast(train.lm.season, h = nValid, level = 0)
# summary(train.lm.season)
# trend and seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
# summary(train.lm.trend.season)

## auto arima
train.arima <- auto.arima(train.ts)
train.arima.pred <- forecast(train.arima, h= length(valid.ts))
# summary(train.arima)



## Performance Evaluation

accuracy.naive <- accuracy(train.naive, valid.ts)
accuracy.snaive <- accuracy(train.snaive, valid.ts)
accuracy.ets <- accuracy(train.ets.auto.pred, valid.ts)
accuracy.lmts <- accuracy(train.lm.trend.season.pred, valid.ts)
accuracy.arima <- accuracy(train.arima.pred, valid.ts)

accuracy.matrix <- rbind("naive", accuracy.naive, "snaive", accuracy.snaive,"ets",accuracy.ets, "lmts", accuracy.lmts,
                         "arima", accuracy.arima)

### ploting forecasts and actual

title1 <- paste(sIndex," ",colnames(swim.ts)[sIndex]," Performance")
title2 <- paste(sIndex," ",colnames(swim.ts)[sIndex]," Forecast")

autoplot(data.ts, series = "Data")+
  autolayer(train.naive$mean, linetype = "dashed", series ="naive")+
  autolayer(train.snaive$mean, linetype = "dashed", series ="snaive")+
  autolayer(train.ets.auto.pred$mean, linetype = "dashed", series = "ets")+
  autolayer(train.ets.auto.pred$fitted, series = "ets")+
  # autolayer(train.lm.trend.pred$mean, linetype = "dashed", series = "reg.t")+
  # autolayer(train.lm.trend.pred$fitted, series = "reg.t")+
  # autolayer(train.lm.season.pred$mean, linetype = "dashed", series = "reg.s")+
  # autolayer(train.lm.season.pred$fitted, series = "reg.s")+
  autolayer(train.lm.trend.season.pred$mean, linetype = "dashed", series = "reg.t+s")+
  autolayer(train.lm.trend.season.pred$fitted, series = "reg.t+s")+
  autolayer(train.arima.pred$mean, linetype = "dashed", series = "arima")+
  autolayer(train.arima.pred$fitted, series = "arima")+
  ggtitle(title1)
