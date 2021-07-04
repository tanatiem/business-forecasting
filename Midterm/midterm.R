library(ggplot2)
library(fpp2)
library(forecast)
library(seasonal)

# f/c error measures -> 02


filepath = 'D:/projects/business-forecasting/04 - ETS/'
filename = 'c3GapSalesData.txt'



src <- read.table('LM7204Midterm2020.txt', header=TRUE)
head(src)

dts <- ts(src$NS, frequency=4, start=c(2005,1))
dts
autoplot(dts)

# train/test split
ytrain <- window(dts, start=c(2005,1), end=c(2007,4))
ytest <- window(dts, start=c(2008,1))
ytest

autoplot(dts) +
  autolayer(ytrain, series="Train") +
  autolayer(ytest, series="Test") +
  ylab('Net Sales (Million USD)') +
  ggtitle('LM Goods - Quarterly Net Sales (Million USD)')
  


# error metrics
res <- residuals()
mean(res^2) # mse

sqrt(mean(res^2)) # RMSE
mean(abs(res)) # MAE
mean(res/y)*100 # MPE (%)
mean(abs(res/y))*100 # MAPE (%)

ytrain

yNaive <- naive(ytrain, h=4)
yNaive

ySNaive <- snaive(ytrain, h=4)
ySNaive

accuracy(ySNaive, ytest)
# snaive()

# forecast() ETS
# ses

ySES <- ses(ytrain, h=4)
ySES
summary(ySES)


dts
# cv - holt
multieHolt <- tsCV(dts, forecastfunction = holt, h=4)
sqrt(colMeans(multieHolt^2, na.rm=TRUE))

# HW additive by default
multieHWA <- tsCV(dts, forecastfunction = hw, h=4)
sqrt(colMeans(multieHWA^2, na.rm=TRUE))

help("hw")

# classical multiplicative decomposition
ytrain
mDM <- decompose(ytrain, type="multiplicative")
tail(seasonal(mDM),4)
sum(tail(seasonal(mDM),4))

# STL
mSTL <- mstl(ytrain)
tail(seasonal(mSTL),4)
sum(tail(seasonal(mSTL),4))


# 5
# decompose
mDM <- decompose(ytrain, type="multiplicative")
siAll <- rep(tail(seasonal(mDM),n=4),times=1)
siAll

# get Seasonaly adjusted data
sa <- seasadj(mDM)

# forecast seasonally adjusted data
modelETS <- ets(sa)
summary(modelETS)

checkresiduals(modelETS)

# get seasonal index (latest from train)
siAll <- rep(tail(seasonal(mDM),n=4),times=1)
siAll
# forecast
fETS <- forecast(modelETS, h=4)
# calculate actual forecast by mutiply SI
actualfc <- fETS$mean * siAll
actualfc

ytest
