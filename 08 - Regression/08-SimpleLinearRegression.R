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
modDecomJS
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
str(dts2)
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


d <- read.table("c4t6.txt", header=T)
head(d)

dts <- ts(d, frequency=4, start=c(2010,1), end=c(2015,4))
dts

autoplot(dts[,2:3])

ggseasonplot(dts[,"JS"],year.labels = TRUE, year.labels.left = TRUE) 

yJS <- ts(d$JS, frequency=4, start=c(2010,1), end=c(2015,4))

#mdcJS <- decompose(yJS, type='multiplicative')
#mdcJS <- decompose(yJS, type='additive')
#mdcJS <- seas(yJS)
mdcJS <- mstl(yJS)
autoplot(mdcJS)

# extract SI
seasonal(mdcJS)
si <- rep(tail(seasonal(mdcJS), n=4),1)
si

# seasonally adjusted
sa <- seasadj(mdcJS)
qplot(d$DPI, as.numeric(sa), xlab='DPI', ylab='JS SS.Adjusted')


# Simple regression
X <- cbind(DPI=dts[,'DPI'],JSSA=sa)
reg <- tslm(JSSA~DPI, data=X)
summary(reg)

# plot
autoplot(yJS, series="JS") +
  autolayer(sa, series ="JSSA") +
  autolayer(fitted(reg), series="JSSA.Fitted") 

#===== Forecast DPI =====
regDPI <- tslm(dts[,'DPI']~trend)
summary(regDPI)

newDPI <- forecast(regDPI, h=4)

autoplot(dts[,'DPI'], series='DPI') + 
  autolayer(regDPI$fitted, series='DPI Fitted') +
  autolayer(newDPI, series='DPI Predicted')

# predict
newX <- data.frame(DPI=newDPI$mean)
yJSSAFC <- forecast(reg, newdata=newX)

yJSFC <- yJSSAFC$mean + si

autoplot(dts[,'JS'], series="JS") +
  autolayer(sa, series ="JS SA") +
  autolayer(fitted(reg), series="JSSA.Fitted") +
  autolayer(yJSSAFC, series="JSSA.Forecast") +
  autolayer(yJSFC, series = "JS.Forecast")


# evalute forecast accuracy on holdout sample
yJStest <- ts(c(6851, 7648, 6735, 11684), frequency = 4, start=c(2016,1))
yJSFC
mean(abs(yJSFC-yJStest)/yJStest)*100
accuracy(yJSFC, yJStest)

# residual analysis
checkresiduals(reg)

