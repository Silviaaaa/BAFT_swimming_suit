setwd("D:/NTU G suite (NTU Space)/01 ½å/ISS/BAFT/Final Project")

library(forecast)
library(ggplot2)
library(caret)
library(e1071)

# read data
swim.data <- read.csv("monthlyProduct.csv")

# transform date
as.Date(as.character(swim.data[ ,2]),"%Y-%m-%d") 
swim.data <- swim.data[ ,-1]
swim.data[is.na(swim.data)] <- 0

## transform data into binary forecast
nSeries <- ncol(swim.data)-1
binary.data <- matrix(NA, nrow = nrow(swim.data)-1,ncol=nSeries)

# rules: yt-(yt-1) / yt > 0.2 [20% growth]
for (col in 1:nSeries){
  for (row in 1:(nrow(swim.data)-1)){
    dif <- swim.data[row,(col+1)] - swim.data[row+1,(col+1)]
    if (dif == 0) { # no difference
      binary.data[row, col] <- 0
    }
    else {
      if (swim.data[row,(col+1)]== 0){ # previous day 0
      binary.data[row, col] <- 1
      }
      else{
        if (binary.data[row, col] <- dif/swim.data[row,(col+1)] >= 0.2000000){
          binary.data[row, col] <- 1
        }
      }
    }
  }
}

colnames(binary.data) <- colnames(swim.data)[2:17]

# convert to ts objects
# binary.ts <- ts(binary.data, start = c(1,6), frequency = 7) # 7-28 is Sat
# sIndex <- 1 ## choose a series to analyze
# data.ts <- binary.ts[ ,sIndex]
# autoplot(data.ts)

### set 1 month as valid
nFull <- nrow(binary.data)
nValid <- 3

valid <- window(binary.data, start = nFull-nValid+1, end = nFull, frequency = 12)
train <- window(binary.data, start = 1, end = nFull-nValid, frequency = 12)

valid <- data.frame(cbind(c(1:nValid),valid[ ,10])) # choose series 1
train <- data.frame(cbind(c(1:(nFull-nValid)),train[ ,10]))

colnames(valid) <- c("t","y")
colnames(train) <- c("t","y")
# run logistic regression
data.lr <- glm(y ~ t, data = train, family = "binomial")
summary(data.lr)
data.lr.pred <- predict(data.lr, valid, type = "response")

# get confusion matrix
confusionMatrix(factor(unname(ifelse(data.lr$fitted > 0.5, 1, 0))), factor(train$y))
confusionMatrix(factor(unname(ifelse(data.lr.pred > 0.5, 1, 0))), factor(valid$y))

