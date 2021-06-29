##############################
# Lec04 Exponential smoothing
#############################
library(ggplot2)
library(fpp2)
library(forecast)

setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")
d <- read.table("04/c3GapSalesData.txt", header=TRUE)
head(d)
head(d$GapSales)
dts <- ts(data=d$GapSales, frequency = 4, start=c(2006,2))
dts
ytrain <- window(dts, start=c(2006,2), end=c(2016,1))
ytest <- window(dts, start=c(2016,2), end=c(2017,1))
autoplot(dts) +
  autolayer(ytrain, series="Training") +
  autolayer(ytest, series="Test") +
  ylab("Gap Sales")

mod1 <- ets(ytrain, model = "AAA", alpha=0.5, beta=0.001, gamma=0.499)
summary(mod1)

mod1Forecast <- forecast(mod1, h=4, level=95)  # default level 80, 95
mod1Forecast

autoplot(mod1Forecast) +
  ylab("Gap Sales")

accuracy(mod1Forecast, ytest)

# Working with ETS object
autoplot(mod1)
res1 <- residuals(mod1)
checkresiduals(res1)


# automatic selection
mod2 <- ets(ytrain, "ZZZ")
summary(mod2)
mod2Forecast <- forecast(mod2, h=4, level=95)
mod2Forecast
autoplot(mod2)
res2 <- residuals(mod2)
checkresiduals(res2)

autoplot(forecast(mod2, h= 12))

# linking forecasts to inventory control
myMod <- ets(dts, model="ZZZ")
summary(myMod)
myModFor <- forecast(myMod, h=3)
myModFor
accuracy(myMod)
qnorm(0.95, sum(myModFor$mean[1:3]), accuracy(myMod)[2]*sqrt(3)) # normal approximation


nsim <- 10000
h <- 3
sim <- numeric(nsim)
for (i in 1:nsim){
  sim[i] <- sum(simulate(myMod, future=TRUE, nsim=h))  # simulation
}
quantile(sim, prob=0.95)

help("simulate.ets")


