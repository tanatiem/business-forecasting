##############################
# Lec06 ARIMA
#############################
library(ggplot2)
library(fpp2)
library(forecast)



################################################
# Review
################################################

setwd("D:/gdrive/BADS/7204-LM forecasting/work/06")
d <- read.table("c3GapSalesData.txt", header=TRUE)
head(d)
head(d$GapSales)
dts <- ts(data=d$GapSales, frequency = 4, start=c(2006,2))
dts
ytrain <- window(dts, start=c(2006,2), end=c(2014,4))
ytest <- window(dts, start=c(2015,1))
autoplot(dts) +
  autolayer(ytrain, series="Training") +
  autolayer(ytest, series="Test") +
  ylab("Gap Sales")

# Holt-Winters
fhw <- hw(ytrain, h= 9, level=c(90, 95))
fhw 

# ETS
mod2 <- ets(ytrain)
summary(mod2)
mod2Forecast <- forecast(mod2, h=9, level=95)
mod2Forecast

# ARIMA
mod3 <- auto.arima(ytrain)
summary(mod3)
mod3Forecast <- forecast(mod3, h=9, level=95)
mod3Forecast


autoplot(dts) + 
  autolayer(fhw, series="Holt-Winters", PI=FALSE) + 
  autolayer(mod2Forecast, series="ETS", PI=FALSE) + 
  autolayer(mod3Forecast, series="ARIMA", PI=FALSE) +
  ylab("Gap Sales")

accuracy(fhw, ytest)
accuracy(mod2Forecast, ytest)
accuracy(mod3Forecast, ytest)


################################################
# Time series cross-validation
################################################

# Compute CV errors for Holt-Winters as e1
e1 <- tsCV(dts, forecastfunction = hw)
e1
mean(e1^2, na.rm=TRUE)

# Need to define function to return forecasts of ETS
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

# Compute CV errors for ETS as e2
e2 <- tsCV(dts, forecastfunction = fets, h=1)
# Compute CV errors for ARIMA as e3
e3 <- tsCV(dts, forecastfunction = farima, h=1)

# Find MSE of each model class
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)

# multi-step errors
multie1 <- tsCV(dts, hw, h=4)
multie1
colMeans(multie1^2, na.rm=TRUE)

# Try multi step 8
multie1 <- tsCV(dts, forecastfunction = hw, h=8)
sqrt(colMeans(multie1^2, na.rm=TRUE))

multie2 <- tsCV(dts, forecastfunction = fets, h=8)
sqrt(colMeans(multie2^2, na.rm=TRUE))

multie3 <- tsCV(dts, forecastfunction = farima, h=8)
sqrt(colMeans(multie3^2, na.rm=TRUE))

errors <- data.frame(rbind(colMeans(multie1^2, na.rm=TRUE),
      colMeans(multie2^2, na.rm=TRUE),
      colMeans(multie3^2, na.rm=TRUE)),
      row.names=c('Holt-Winters','ETS','ARIMA')
)
colnames(errors) <- 1:8

library(reshape2)

temp <- melt(as.matrix(errors))
colnames(temp) <- c('model','h','MSE')
ggplot(data=temp) +
  geom_line(
    mapping=aes(x=h, y=MSE, group=model, color=model)
  ) + 
  geom_point(
    mapping=aes(x=h, y=MSE, group=model, color=model)
  )
################################################
# White Noise
################################################
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)

checkresiduals(mod3)


################################################
# Autocorrelation
################################################
y <- c(15, 10, 12, 16, 9, 12, 10)
sum(y)
ggAcf(y)
ggAcf(y, lag.max=4)
ggAcf(y, plot=FALSE)




################################################
# Non-seasonal ARIMA
################################################
#c7t2 <- read.table("c7t2.txt", header=TRUE)
c7t2 <- read.table('c7t2 NEW.csv', sep=',', header=TRUE)

y3 <- ts(c7t2$AR1, freq=1)
autoplot(y3)
ggAcf(y3)  
ggPacf(y3)    

# select model order yourself
modAR1 <- Arima(y3, order=c(1,0,0))
summary(modAR1)
checkresiduals(modAR1)
y3f <- forecast(modAR1, h=20)
autoplot(y3f)

# auto selection with approximation
y3amod <- auto.arima(y3)
summary(y3amod)
autoplot(forecast(y3amod, h=20))

# auto selction without approximation
y3aa <- auto.arima(y3, stepwise = FALSE, approximation = FALSE)
summary(y3aa)

#######################
