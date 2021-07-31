library(ggplot2)
library(forecast)
library(fpp2)
library(dplyr)
library(caret)
library(e1071)


d <- read.table('datap1.txt', header=TRUE,
                colClasses=c('numeric','numeric','factor','factor','numeric'))
str(d)

qplot(d$CarAge, d$FuelConsumption, xlab='Car Age', ylab='Fuel Consumption')

# Problem 2

reg <- lm(FuelConsumption ~ CarAge, data=d)
summary(reg)

checkresiduals(reg)

newdata <- data.frame(CarAge=3.2)
fc <- forecast(reg, newdata=newdata, level=90)
fc


cor(d[c(1,2,5)])
cor(d$CarAge, d$Distance)

regA <- lm(FuelConsumption ~ CarAge, data=d)
regB <- lm(FuelConsumption ~ CarAge + Distance, data=d)
summary(regB)

qplot(d$Distance, d$FuelConsumption)


regA <- lm(FuelConsumption ~ CarAge, data=d)
regB <- lm(FuelConsumption ~ CarAge + Distance, data=d)

summary(regA)$r.squared
summary(regB)$r.squared

CV(regA)
CV(regB)

# Problem 4

reg <- lm(FuelConsumption ~ CarAge + FemaleDriver + CarModel,data=d)
summary(reg)
d

newdata <- data.frame(CarAge=c(3.2,3.2,3.2,3.2),
                      FemaleDriver=factor(c(1,1,0,0), levels=0:1),
                      CarModel=factor(c('A','P','A','P'), levels=c('A','P')))
forecast(reg, newdata=newdata)



# Problem 6
d <- read.table('datap6.txt',header=TRUE,
                colClasses=c('integer','numeric','numeric','factor'))
d
str(d)

logreg <- glm(MowerOwner ~ Income + LotSize, data=d, family="binomial")
summary(logreg)

newdata <- data.frame(Income=c(68.7,49.5),
                      LotSize=c(22.2,18))
predict(logreg, newdata = newdata, type="response")
