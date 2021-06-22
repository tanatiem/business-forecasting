##############################
# Lec05 TS Decomposition
#############################
library(ggplot2)
library(fpp2)
library(forecast)

library(seasonal)


y <- c(6809, 6465, 6569, 8266, 
       7257, 7064, 7784, 8724,
       6992, 6822, 7949, 9650)
dts <- ts(y, frequency = 4)
dts


################################################
# Classical Decomposition
# Seasonal indices
################################################

mDM <- decompose(dts, type="multiplicative")
mDM$seasonal
autoplot(mDM) 

mDA <- decompose(dts, type="additive")
mDA$seasonal
autoplot(mDA) 

# components
trendcycle(mDM)
seasonal(mDM)
remainder(mDM)

# forecasting with decomposition

sa <- seasadj(mDM)
autoplot(dts, series="Data") +
  autolayer(sa, series="Seasonally Adjusted") 

siLast <- tail(mDM$seasonal,n=4) # SI last year
siLast
saNaive2 <- naive(sa, h=8)
saNaive2$mean
siAll <- rep(siLast[1:4], times=2)
siAll
actualForecastN2 <- siAll*saNaive2$mean
actualForecastN2

autoplot(dts, series="Data") +
  autolayer(sa, series="Seasonally Adjusted") +
  autolayer(actualForecastN2, series='Actual Forecast')

################################################
# STL decomposition
################################################
mSTL <- mstl(dts)
mSTL
autoplot(mSTL)

# manual naive
siAllA <- rep(tail(seasonal(mSTL),n=4), times=2)
siAllA
mSTL1 <- naive(seasadj(mSTL),h=8)$mean + siAllA
mSTL1

# use forecast function
mSTL2 <- forecast(mSTL, method="naive", h=8)
mSTL2$mean

# use short-cut function: stlf()
mSTL3 <- stlf(dts, method="naive", h=8)
mSTL3$mean

# manual holt
siAllA <- rep(tail(seasonal(mSTL),n=4), times=2)
mSTL4 <- holt(seasadj(mSTL),h=8)$mean + siAllA
mSTL4



################################################
# SEATS decomposition 
################################################

d <- read.table("c6t2.csv", sep=',', header=TRUE)  # private housing starts
head(d)

dtsPHS <- ts(d$PHS, frequency = 4, start=c(1959, 1))
autoplot(dtsPHS) + ylab("PHS (000)")
head(dtsPHS)
tail(dtsPHS)


mSEATS <- seas(dtsPHS)
autoplot(mSEATS)

siAllM <- rep(tail(seasonal(mSEATS),n=4), times=2)
mSEATSf <- naive(seasadj(mSEATS),h=8)$mean*siAllM
mSEATSf

autoplot(dtsPHS, series="Data") +
  autolayer(seasadj(mSEATS), series="Seasonally Adjusted") +
  autolayer(mSEATSf, series = "Forecast (SEATS + Naive)") +
  ylab("PHS (000)")


###############################
