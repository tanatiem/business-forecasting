library(fpp2)
library(forecast)


d <- read.table('c3t13.txt', header=TRUE,
                colClasses = c('numeric','factor'))
str(d)
dts <- ts(data=d, frequency=12, start=c(2015,4))
dts
autoplot(dts[,'Mustard']) +
  ylab('Mustard Sales')

newdata <- data.frame(trend=28:33, 
                      season=factor(7:12, levels=1:12),
                      fevent=factor(rep(1,6), levels=1:3))
newdata
forecast(myReg2, newdata=newdata, level=c(90, 95))

# Model 1
reg1 <- tslm(Mustard ~ trend, data=dts)
summary(reg1)

fc <- forecast(reg1, newdata=newdata)
autoplot(dts[,'Mustard'], series='Data') +
  autolayer(fitted(reg1), series='Fitted') +
  autolayer(fc, series='Forecast') +
  ylab('Sales') +
  ggtitle('Mustard ~ trend')

# Model 2
# make factor event
fevent <- factor(dts[,'Event'], levels=1:3)
fevent
levels(fevent)
reg2 <- tslm(Mustard ~ trend + season + fevent, data=dts)
summary(reg2)

fc <- forecast(reg2, newdata=newdata)
autoplot(dts[,'Mustard'], series='Data') +
  autolayer(fitted(reg2), series='Fitted') + 
  autolayer(fc, series='Forecast') +
  ylab('Sales') +
  ggtitle('Mustard ~ trend + season + fevent')

# Model 3
reg3 <- tslm(Mustard ~ trend + season, data=dts)
summary(reg3)

fc <- forecast(reg3, newdata=newdata)
autoplot(dts[,'Mustard'], series='Data') +
  autolayer(fitted(reg3), series='Fitted') +
  autolayer(fc, series='Forecast') + 
  ylab('Sales') +
  ggtitle('Mustard ~ trend + season')

# Model 4
reg4 <- tslm(Mustard ~ trend + fevent, data=dts)
summary(reg4)

fc <- forecast(reg4, newdata=newdata)
autoplot(dts[,'Mustard'], series='Data') +
  autolayer(fitted(reg4), series='Fitted') +
  autolayer(fc, series='Forecast') +
  ylab('Sales') +
  ggtitle('Mustard ~ trend + fevent')

data.frame(MD1=AIC(reg1),MD2=AIC(reg2),MD3=AIC(reg3),MD4=AIC(reg4))
CV(reg1)
CV(reg2)
CV(reg3)
CV(reg4)
CV(myReg2)

