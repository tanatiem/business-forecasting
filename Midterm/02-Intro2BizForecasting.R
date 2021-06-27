library(forecast)
library(ggplot2)
library(fpp2)

# P. Dalgaard. (2002) Introductory Statistics with R
# overgrown calculator
2+2
2*exp(-2)
qnorm(0.90) # cumulative dist
help("qnorm")
qnorm(0.9, 0, 5.724)
pnorm(0)
pnorm(108, mean=100, sd=10)

# assignment
x <- 2
x + x

# vectorized arithmetic
weight <- c(60, 72, 57, 90, 95, 72)
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2
bmi
sum(weight)
sum(weight)/length(weight)
mean(weight)

# calculate sample standard deviation (the denominator is n-1)
1/(length(weight)-1) * sum((weight - sum(weight)/length(weight))^2)
var(weight)
sd(weight)

# weight & height correlation
plot(height, weight)
cor(height, weight)

# time-series
y <- ts(c(123,39,78,52,110), start=2012)
plot(y)
#y <- ts(z, start=2003, frequency=12)


# indexing & selection 
d <- data.frame(weight=weight, height=height)
d
d$weight[2:4]
d$weight[c(1,3,4)]
d$weight[d$height > 1.8]
d$weight[d$height > 1.8 & d$weight > 75]

d2 <- subset(d, d$height > 1.8)
d2

c1f8TNHS <- read.table('c1f8TNHS.txt', header=TRUE)
c1f8TNHS
# HA (2018)
# ts objects
y <- ts(c1f8TNHS$TNHS, frequency = 12, start=c(2010, 01))
head(y)
# ts(c1f8TNHS$TNHS, frequency=4, start=c(2013,3)) another example
autoplot(y) + 
  ggtitle("New Houses Sold (000)") +
  xlab("Year") +
  ylab("Thousand")

ggseasonplot(y, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Thousand") +
  ggtitle("Seasonal plot: New Houses Sold")

ytrain <- window(y, start=c(2011,1), end=c(2015,6))
ytest <- window(y, start=c(2015,7), end=c(2016,12))
autoplot(y) +
  autolayer(ytrain, series="Training") +
  autolayer(ytest, series="Test")

yfitN <- naive(ytrain, h=18)
autoplot(yfitN, PI=TRUE)
autoplot(yfitN, PI=FALSE)

yfitSN <- snaive(ytrain, h=18)
autoplot(yfitSN, PI=FALSE)
autoplot(yfitSN, PI=TRUE)

yfitETS <- forecast(ytrain, h=18)

summary(yfitETS)

autoplot(y) +
  autolayer(yfitN, series="Naive", PI=FALSE) + 
  autolayer(yfitSN, series="Seasonal Naive", PI=FALSE) +
  autolayer(yfitETS, series="ETS", PI=FALSE) +
  xlab("Year") + ylab("Thousand") +
  ggtitle("Forecasts for New Houses Sold (000) ") +
  guides(colour=guide_legend(title="Forecast"))

accuracy(yfitN, ytest)
accuracy(yfitSN, ytest)
accuracy(yfitETS, ytest)

# linking inventory and forecasting
forecast(y, h=12)
yhat2017 <- forecast(y, h=12)
effDemandMean <- sum(yhat2017$mean[1:4])
accuracy(yhat2017)
errorMesure <- accuracy(yhat2017)
RMSE <- errorMesure[2]

ReviewPeiod <- 3
LeadTime <- 1
protectionTime <- ReviewPeiod + LeadTime
effDemandStd <- RMSE*sqrt(protectionTime)
CSL <- 0.9
SS <- qnorm(CSL, 0, effDemandStd )
OUTL1 <- ceiling(effDemandMean + SS) 
OUTL1
OUTL2 <- ceiling(qnorm(CSL, effDemandMean, effDemandStd))
OUTL2


res <- residuals(yhat2017)
autoplot(res) + xlab("Year") + ylab("") + ggtitle("Residual from ETS model")
gghistogram(res)
ggAcf(res)
checkresiduals(yhat2017)

# in-class homework 02
# service level = 0.95
forecast(y, h=12)
yhat2017 <- forecast(y, h=12)
effDemandMean <- sum(yhat2017$mean[4:7])
accuracy(yhat2017)
errorMesure <- accuracy(yhat2017)
RMSE <- errorMesure[2]

effDemandMean <- sum(yhat2017$mean[4:7])
ReviewPeiod <- 3
LeadTime <- 1
protectionTime <- ReviewPeiod + LeadTime
effDemandStd <- RMSE*sqrt(protectionTime)
CSL <- 0.95
SS <- qnorm(CSL, 0, effDemandStd )
OUTL1 <- ceiling(effDemandMean + SS) 
OUTL2 <- ceiling(qnorm(CSL, effDemandMean, effDemandStd))

res <- residuals(yhat2017)
autoplot(res) + xlab("Year") + ylab("") + ggtitle("Residual from ETS model")
gghistogram(res)
ggAcf(res)
ggAcf(res, 12)
checkresiduals(yhat2017)

#  Interactive case: the gap (KW Chapter 2)
res <- residuals(yhat2017)
mean(res)
cor(res[1:83],res[2:84])
