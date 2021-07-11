##############################
# Lec09 Multiple Regression
#############################
library(ggplot2)
library(fpp2)
library(forecast)

#setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")

# time plot
#d <- read.delim("c5f2.txt")
d <- read.delim('c5f2.csv', sep=',')
head(d)

str(d)

dts <- ts(data= d, frequency = 4, start=c(2002, 1), end=c(2016, 4))
head(dts)

colnames(dts)

dts[1:3, "NCS"]
dts[1:3, 2]  
dts[1:3, c("NCS", "DPIPC")]
dts[1:3, 2:3]
dts[1:3, c("NCS", "DPIPC", "UMICS")]
dts[1:3, c(2:3,6)]

autoplot(dts[,2:6])
autoplot(dts[,2:6], facets = TRUE) +
  ylab(" ")

# regression equation
myReg <- tslm(NCS ~ DPIPC + UR + PR + UMICS, data=dts)
summary(myReg)
autoplot(dts[,"NCS"], series="Data") +
  autolayer(fitted(myReg), series="Fitted") +
  ylab("")

checkresiduals(myReg)

# coefficient t-statistics
# DPIPC
# t = 3.2275/0.3916
pt(3.2275/0.3916,55) # left tail
1-pt(3.2275/0.3916,55) # right tail
2*(1-pt(3.2275/0.3916,55)) # two tail

# PR
t <- -3219.4991/1412.5314
t
2*(1-pt(abs(t),55))


# Adj R squared
ybar <- mean(d$NCS)
SSE <- sum((myReg$residuals)^2)
SSE
SST <- sum((d$NCS-ybar)^2) 
SST
Rsq <- 1- SSE/SST
Rsq
n <- 60
k <- 4
AdjRSq <- 1-(1-Rsq)*(n-1)/(n-k-1)
AdjRSq

# f test
SSR <- sum((myReg$fitted.values -ybar)^2)
MSR <- SSR/k
MSE <- SSE/(n-k-1)
Fval <- MSR/MSE
Fval
1-pf(Fval, 4, 55)

# t test
1-pt(2.277, 55)  # right tail
2*(1-pt(2.277, 55))  # two tail


# forecasting with multiple regression
md <- data.frame(DPIPC=42807, UR=4.93, PR=3.50, UMICS=91.57)
forecast(myReg, newdata = md)

snaive(dts[,"DPIPC"], h=1)$mean
snaive(dts[,"UR"], h=1)$mean
snaive(dts[,"PR"], h=1)$mean
snaive(dts[,"UMICS"], h=1)$mean

snaive(dts[,"DPIPC"], h=4)$mean
snaive(dts[,"UR"], h=4)$mean
snaive(dts[,"PR"], h=4)$mean
snaive(dts[,"UMICS"], h=4)$mean

fitted(myReg)
myH <- 4
myBaseline <- data.frame(DPIPC = snaive(dts[,"DPIPC"], h=myH)$mean,
                        UR = snaive(dts[,"UR"],h=myH)$mean,
                        PR = snaive(dts[,"PR"],h=myH)$mean,
                        UMICS = snaive(dts[,"UMICS"],h=myH)$mean)
fcast.baseline <- forecast(myReg, newdata=myBaseline)
fcast.baseline

myNewScenario <- data.frame(DPIPC=1.05*myBaseline$DPIPC,
                            UR = 0.98*myBaseline$UR,
                            PR = myBaseline$PR,
                            UMICS = 1.03*myBaseline$UMICS)  # optimistic
fcast.newscenario <- forecast(myReg, newdata=myNewScenario)
fcast.newscenario

autoplot(dts[,"NCS"], series="Data") + 
  autolayer(fcast.baseline, PI=FALSE, series="baseline") + 
  autolayer(fcast.newscenario, PI=FALSE, series="new scenario (optimistic)") +
  ylab("NCS")

CV(myReg)

x# prediction interval
autoplot(fcast.baseline)
fcast.baseline
forecast(myReg, newdata = myBaseline, level=c(90, 95, 99))

sqrt(sum(resid(myReg)^2)/(55))
yHat <- 217301.2 
SEE <- 11690
yHat + 1.96*SEE
yHat - 1.96*SEE


# multicollinearity
x <- data.frame(DPIPC=d$DPIPC, PR=d$PR, UR=d$UR, UMICS=d$UMICS)
cor(x)
library(GGally)
ggpairs(x)

# vif
library(car)
vif(myReg)

CV(myReg)
myReg2 <- tslm(NCS~DPIPC, data=dts)
CV(myReg2)

# model selection using genetic algorithm
# Scrucca 2013, GA: a package for genetic algorithm in R. 
# Journal of Statistical Software. Vol 53(4)
# Examples: subset selection

data("fat", package = "UsingR")
mod <- lm(body.fat.siri ~ age + weight + height 
          + neck + chest + abdomen + hip
          + thigh + knee + ankle + bicep + forerm + wrist, data=fat)
