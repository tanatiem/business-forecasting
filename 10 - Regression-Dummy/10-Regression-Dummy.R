############################################
# Lec10 Multiple Regression with Dummy
###########################################

library(fpp2)
library(forecast)

#setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")

d <- read.table("c3t13.txt", header=TRUE)
str(d)
dts <- ts(data= d, frequency = 12, start=c(2015, 4))
dts
autoplot(dts[,"Mustard"]) +
  ylab("Mustard")


# This does NOT describe the event
myRegBAD <- tslm(Mustard ~ trend + season + Event, data=dts)
summary(myRegBAD)

# Need to make event as factors
fevent <- factor(dts[,"Event"], levels=1:3)
fevent
levels(fevent)

# Regression with categorical data
myReg2 <- tslm(Mustard ~ trend + season + fevent, data=dts)
summary(myReg2)
autoplot(dts[,"Mustard"], series = "Data") +
  autolayer(fitted(myReg2), series="Fitted") +
  ylab("Mustard")
checkresiduals(myReg2)

# forecasting 07/2017-12/2017 
mynewdata <- data.frame(trend=28:33, 
                        season=factor(7:12, levels=1:12),
                        fevent=factor(c(1,1,1,2,1,3), levels=1:3))
mynewdata
forecast(myReg2, newdata=mynewdata, level=c(90, 95))

myFC <-forecast(myReg2, newdata=mynewdata, level=c(90, 95))$mean
autoplot(dts[,"Mustard"], series = "Data") +
  autolayer(fitted(myReg2), series="Fitted") +
  autolayer(myFC, series="Forecast") +
  ylab("Mustard")

###############################
# in-class exercise 01
# forecasting 07/2017-12/2017 
mynewdata2 <- data.frame(trend=28:33, 
                        season=factor(7:12, levels=1:12),
                        fevent=factor(rep(1,6), levels=1:3))
mynewdata2
forecast(myReg2, newdata=mynewdata2, level=c(90, 95))

myFC2 <- forecast(myReg2, newdata=mynewdata2, level=c(90, 95))
autoplot(dts[,"Mustard"], series = "Data") +
  autolayer(fitted(myReg2), series="Fitted") +
  autolayer(myFC2, series="Forecast") +
  ylab("Mustard")

###############################
# in-class exercise 02
# Based on AIC, which model should be selected?
# Model 1: Consider only the trend
dts
exreg1 <- tslm(Mustard ~ trend, data=dts)
AIC(exreg1)
AIC(myReg2)

mynewdata2 <- data.frame(trend=28:33, 
                         season=factor(7:12, levels=1:12),
                         fevent=factor(c(2,2,2,3,3,3), levels=1:3))
myFC2 <- forecast(myReg2, newdata=mynewdata2, level=c(90,95))
autoplot(dts[,"Mustard"], series="Data") +
  autolayer(fitted(myReg2), series="Fitted") +
  autolayer(myFC2, series="Forecast") +
  ylab("Mustard")


# Alternative representation 
levels(fevent) <- c("nothing", "promotion", "media")
fevent
myReg3 <- tslm(Mustard ~ trend + season + fevent, data=dts)
summary(myReg3)
mynewdata <- data.frame(trend=28:33, 
                        season=factor(7:12, levels=1:12),
                        fevent=factor(c("nothing", "nothing", "nothing", 
                                        "promotion","nothing","media")))
mynewdata
forecast(myReg3, newdata=mynewdata, level=c(90, 95))

AIC(myReg2)
AIC(myReg3)
#################################
# interaction variable
#################################

dbs <- read.table("banksalary.txt", header=TRUE)
str(dbs)
dbs <- read.table("banksalary.txt", header=TRUE, 
                  colClasses = c("integer",	"factor",	"factor",	
                                 "integer",	"integer",	"factor",	
                                 "factor",	"integer",	"numeric"
))
str(dbs)
mBS1 <- lm(Salary ~ YrsExper + Gender, data = dbs)  
mBS1

mSB2 <- lm(Salary ~ YrsExper*Gender, data = dbs)
mSB2

anova(mSB2)
anova(mBS1)


regbs <- lm(Salary ~ YrsExper + Gender + JobGrade + PCJob, data=dbs)
summary(regbs)

CV(regbs)
#################################
# full rank assumption
################################# 
X <- matrix(c(1	,	1	,	0	,	0	,	1	,
              1	,	2	,	0	,	0	,	1	,
              1	,	3	,	0	,	1	,	0	,
              1	,	4	,	1	,	0	,	0	,
              1	,	5	,	1	,	0	,	0	,
              1	,	6	,	1	,	0	,	0	,
              1	,	7	,	1	,	0	,	0	,
              1	,	8	,	1	,	0	,	0	,
              1	,	9	,	0	,	1	,	0	,
              1	,	10	,	1	,	0	,	0	,
              1	,	11	,	1	,	0	,	0	,
              1	,	12	,	1	,	0	,	0	,
              1	,	13	,	0	,	1	,	0	,
              1	,	14	,	1	,	0	,	0	,
              1	,	15	,	0	,	1	,	0	,
              1	,	16	,	1	,	0	,	0	,
              1	,	17	,	1	,	0	,	0	,
              1	,	18	,	1	,	0	,	0	,
              1	,	19	,	1	,	0	,	0	,
              1	,	20	,	1	,	0	,	0	,
              1	,	21	,	0	,	1	,	0	,
              1	,	22	,	1	,	0	,	0	,
              1	,	23	,	1	,	0	,	0	,
              1	,	24	,	1	,	0	,	0	,
              1	,	25	,	0	,	1	,	0	,
              1	,	26	,	1	,	0	,	0	,
              1	,	27	,	0	,	1	,	0	
), ncol=5, byrow = TRUE)
head(X)
qr(X)$rank  # rank of matrix

t(X)%*%X
det(t(X)%*%X)  # determinant of matrix
solve(t(X)%*%X)  # inverse of matrix


