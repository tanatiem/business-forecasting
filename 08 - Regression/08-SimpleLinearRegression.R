################################################
# Lec05 Simple Linear Regression
################################################
library(ggplot2)
library(fpp2)
library(forecast)

#setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")

d <- read.table("c4t6.txt", header=T)
head(d)  
d$DPI  # data frame column selection

dts <- ts(data= d, frequency = 4, start=c(2010, 1), end=c(2015, 4))
head(dts)
dts[,"DPI"] # time series column selection

# Time plot
autoplot(dts[,c("JS", "DPI")]) +
  ylab("")

# Scatter plot
qplot(DPI, JS, data=as.data.frame(dts))
qplot(d$DPI, d$JS)  

# Seasonally adjusted
yJS <- ts(data=d$JS, frequency = 4, start=c(2010, 1), end=c(2015, 4))
modDecomJS <- decompose(yJS, type="multiplicative")
autoplot(modDecomJS)
si <- tail(seasonal(modDecomJS),n=4) 
si
yJSSA <- seasadj(modDecomJS)
autoplot(yJS, series="JS Data") +
  autolayer(yJSSA, series="Deasonalized series")

dJSSA <- as.numeric(yJSSA)
class(yJSSA)
class(dJSSA)
qplot(d$DPI, dJSSA, xlab="DPI", ylab="JS Seasonally Adjusted")


# Simple regression
yDPI <- ts(data=d$DPI, frequency = 4, start=c(2010, 1), end=c(2015, 4))
myReg1 <- tslm(yJSSA~yDPI)
summary(myReg1)

dts2 <- cbind(DPI=dts[,"DPI"], JSSA=yJSSA)
head(dts2)
myReg2 <- tslm(JSSA~DPI, data=dts2)  # same thing as above
summary(myReg2)  

autoplot(yJS, series="JS") +
  autolayer(yJSSA, series ="JSSA") +
  autolayer(fitted(myReg1), series="JSSA.Fitted") 



# Forecasts for 2016Q1-Q4
DPIFC <- holt(yDPI, h=4)
mynewdata <- data.frame(yDPI=as.numeric(DPIFC$mean))
mynewdata

yJSSAFC <- forecast(myReg1, newdata = mynewdata)
yJSSAFC

siAllM <- rep(si, times=1)
yJSFC <- yJSSAFC$mean*siAllM
yJSFC



autoplot(yJS, series="JS") +
  autolayer(yJSSA, series ="JSSA") +
  autolayer(fitted(myReg1), series="JSSA.Fitted") +
  autolayer(yJSSAFC, series="JSSA.Forecast") +
  autolayer(yJSFC, series = "JS.Forecast")

# evalute forecast accuracy on holdout sample
yJStest <- ts(c(6851, 7648, 6735, 11684), frequency = 4, start=c(2016,1))
yJSFC
mean(abs(yJSFC-yJStest)/yJStest)*100

# residual analysis
checkresiduals(myReg1)

library(lmtest)
dwtest(myReg1, alternative="two.sided")

######################
# in-class exercise
#####################

d <- read.table("c4t6_2.csv", sep=',', header=T)
head(d)  
d$DPI  # data frame column selection

dts <- ts(data= d, frequency = 4, start=c(2010, 1), end=c(2015, 4))
head(dts)
dts[,"DPI"] # time series column selection


# Seasonally adjusted
yJS <- ts(data=d$JS, frequency = 4, start=c(2010, 1), end=c(2015, 4))
modDecomJS <- decompose(yJS, type="multiplicative")
autoplot(modDecomJS)
si <- tail(seasonal(modDecomJS),n=4) 
si
yJSSA <- seasadj(modDecomJS)


dJSSA <- as.numeric(yJSSA)
class(yJSSA)
class(dJSSA)
qplot(d$DPI, dJSSA, xlab="DPI", ylab="JS Seasonally Adjusted")


# Simple regression
yDPI <- ts(data=d$DPI, frequency = 4, start=c(2010, 1), end=c(2015, 4))
myReg1 <- tslm(yJSSA~yDPI)
summary(myReg1)

dts2 <- cbind(DPI=dts[,"DPI"], JSSA=yJSSA)
head(dts2)
myReg2 <- tslm(JSSA~DPI, data=dts2)  # same thing as above
summary(myReg2)  

autoplot(yJS, series="JS") +
  autolayer(yJSSA, series ="JSSA") +
  autolayer(fitted(myReg1), series="JSSA.Fitted") 

dts3 <- cbind(T=dts[,"Time.period"], DPI=dts[,"DPI"])
dts3
myReg3 <- tslm(DPI~T, data=dts3)
summary(myReg3)

# Forecasts for 2016Q1-Q4
#DPIFC <- holt(yDPI, h=4)
#mynewdata <- data.frame(yDPI=as.numeric(DPIFC$mean))
#mynewdata

# Forecasts DPI using T for 2016Q1-Q4
newT = data.frame(T=c(25,26,27,28))
DPIFC = forecast(myReg3, newdata=newT)
mynewdata <- data.frame(yDPI=as.numeric(DPIFC$mean))
mynewdata

yJSSAFC <- forecast(myReg1, newdata = mynewdata)
yJSSAFC

siAllM <- rep(si, times=1)
yJSFC <- yJSSAFC$mean*siAllM
yJSFC

autoplot(yJS, series="JS") +
  autolayer(yJSSA, series ="JSSA") +
  autolayer(fitted(myReg1), series="JSSA.Fitted") +
  autolayer(yJSSAFC, series="JSSA.Forecast") +
  autolayer(yJSFC, series = "JS.Forecast")


# evalute forecast accuracy on holdout sample
yJStest <- ts(c(6851, 7648, 6735, 11684), frequency = 4, start=c(2016,1))
yJSFC
mean(abs(yJSFC-yJStest)/yJStest)*100

# residual analysis
checkresiduals(myReg1)

