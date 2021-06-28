##############################
# Lec03 Exponential smoothing
#############################
library(ggplot2)
library(fpp2)
library(forecast)

############################################################
## Review
############################################################

# vector of demand data
demand <- c(1212, 1321, 1278, 1341, 
            1257, 1341, 1257, 1287, 
            1189, 1111, 1145, 1150,
            1298, 1331) 

# create time series object
y <- ts(demand, start=c(2010,3), frequency=4) 
y


# partition into train and test data
ytrain <- window(y, start=c(2010,3), end=c(2012,4)) # train
ytest <- window(y, start=c(2013,1)) # test 

autoplot(y) +
  autolayer(ytrain, series="Training") +
  autolayer(ytest, series="Test") +
  ylab("Demand")


############################################################
## Naive and average methods
############################################################

# naive method
yN <- naive(ytrain, h=4)
yN


# average method
yM <- meanf(ytrain, h=4)
yM


############################################################
## Simple Exponential Smoothing
############################################################


ySES <- ses(ytrain, h=4)
ySES
summary(ySES)
accuracy(ySES, ytest)
autoplot(ySES) + 
  autolayer(fitted(ySES), series="SES Fitted") +
  ylab("Demand")



############################################################
## Holt (Double Exponential Smoothing)
############################################################

yHolt <- holt(ytrain, h=4) # h forecast horizon = 4 periods
yHolt
summary(yHolt)

autoplot(y) +
  autolayer(yHolt, series="Holt", PI=FALSE) +
  ylab("Demand")


yHoltDamp <- holt(ytrain, h=4, damped=TRUE)
summary(yHoltDamp)

accuracy(ySES, ytest)
accuracy(yHolt, ytest)



############################################################
## Holt-Winters' (Triple Exponential Smoothing)
############################################################

yHWA <- hw(ytrain, h=4, seasonal="additive")
yHWM <- hw(ytrain, h=4, seasonal="multiplicative")


summary(yHWA)
summary(yHWM)

accuracy(yHWA, ytest)
accuracy(yHWM, ytest)

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

summary(fit1)
summary(fit2)

## In-class
accuracy(yN, ytest)
accuracy(yM, ytest)
accuracy(ySES, ytest)
accuracy(yHolt, ytest)
accuracy(yHoltDamp, ytest)
accuracy(yHWA, ytest)

autoplot(y) +
  autolayer(yN, series="Naive", PI=FALSE) +
  autolayer(yM, series="Average", PI=FALSE) +
  autolayer(ySES, series="SES", PI=FALSE) +
  autolayer(yHolt, series="Holt", PI=FALSE) +
  autolayer(yHoltDamp, series="Holt Damped", PI=FALSE) +
  autolayer(yHoltDamp, series="HW Additive", PI=FALSE) +
  xlab("Year") +
  ylab("Demand") +
  guides(colour=guide_legend(title="Forecast"))




## Homework 3
y_hw3 <- ts(c(21,19, 31, 27, 29), frequency=12, start=c(2000,3))
y_hw3
y <- ses(y_hw3, h=1, alpha=.1)
summary(y)

