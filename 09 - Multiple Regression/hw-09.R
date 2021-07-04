library(ggplot2)
library(fpp2)
library(forecast)
library(GGally)

data <- read.table('c5p9.csv',sep=',',header=TRUE)
data

str(data)

ggpairs(data[,2:4])

reg <- lm(AS~INC+POP, data=data)

summary(reg)
checkresiduals(reg)

newX <- data.frame(INC=23175, POP=128.7)
forecast(reg, newdata = newX)


