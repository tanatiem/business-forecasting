library(ggplot2)
library(forecast)
library(fpp2)
library(dplyr)
library(caret)
library(e1071)

########################
# 08 - Simple Reg
########################
d <- read.table('d:/projects/business-forecasting/final/c4t6.txt', header=TRUE)
qplot(d$DPI, d$JS)

dts <- ts(d, frequency=4, start=c(2010,1), end=c(2015,4))
autoplot(dts[,2:3])

yJS <- dts[,'JS']

# A)
# decompose
dcompJS <- decompose(yJS, type="multiplicative")
autoplot(dcompJS)

yJSSA <- seasadj(dcompJS)
autoplot(yJS, series="JS Data") +
  autolayer(yJSSA, series="Deasonalized series")

qplot(d$DPI, as.numeric(yJSSA))

# regression
yDPI <- dts[,'DPI']
#reg1 <- tslm(yJSSA ~ yDPI)

reg1 <- tslm(JSSA ~ DPI, data=cbind(DPI=dts[,'DPI'],JSSA=yJSSA))
summary(reg1)
checkresiduals(reg1)

# newdata -> DPI forecast using holt()
DPIFC <- holt(yDPI, h=4)
yJSSAFC <- forecast(reg1, newdata=data.frame(DPI=DPIFC$mean))
autoplot(yJSSAFC)

si <- rep(tail(seasonal(dcompJS),4),1)
yJSFC <- yJSSAFC$mean * si
yJSFC

autoplot(yJS, series='JS') +
  autolayer(yJSSA, series='JSSA') +
  autolayer(fitted(reg1), series='JSSA.Fitted') +
  autolayer(yJSSAFC, series='JSSA.Forecast') +
  autolayer(yJSFC, series='JS.Forecast')

yJStest <- ts(c(6851, 7648, 6735, 11684), frequency = 4, start=c(2016,1))
mean(abs(yJSFC-yJStest)/yJStest)*100  # MAPE
# 1.969522
accuracy(yJSFC, yJStest)

########################
# 09 - Multiple Reg
########################
d <- read.delim('c5f2.csv', sep=',')
dts <- ts(data= d, frequency = 4, start=c(2002, 1), end=c(2016, 4))
head(dts)
autoplot(dts[,2:6], facets = TRUE)

reg1 <- tslm(NCS ~ DPIPC + UR + PR + UMICS, data=dts)
summary(reg1)
checkresiduals(reg1)
autoplot(dts[,'NCS'], series='Data') +
  autolayer(fitted(reg1), series='Fitted')

SSE <- sum(reg1$residuals^2)
SST <- sum((d$NCS - mean(d$NCS))^2)
R2 <- 1 - SSE/SST # R sqaured
R2
n <- nrow(d)
AdjR2 <- 1 - (1-R2)*(n-1)/(n-4-1)
AdjR2

h <- 4
myBaseline <- data.frame(DPIPC=snaive(dts[,'DPIPC'], h=h)$mean,
                      UR=snaive(dts[,'UR'], h=h)$mean,
                      PR=snaive(dts[,'PR'], h=h)$mean,
                      UMICS=snaive(dts[,'UMICS'], h=h)$mean)
fcast.baseline <- forecast(reg1, newdata=myBaseline)
fcast.baseline

myNewScenario <- data.frame(DPIPC=1.05*myBaseline$DPIPC,
                            UR = 0.98*myBaseline$UR,
                            PR = myBaseline$PR,
                            UMICS = 1.03*myBaseline$UMICS)  # optimistic
fcast.newscenario <- forecast(myReg, newdata=myNewScenario)
fcast.newscenario

autoplot(dts[,'NCS'], series='Data') + 
  autolayer(fcast.baseline, series='Baseline', PI=F) +
  autolayer(fcast.newscenario, series='Optimistic', PI=F)

accuracy(reg1)
AIC(reg1)
CV(reg1)

#######################################
# 10 - Regression with Dummy variables
#######################################

# integer, numeric, factor
d <- read.table("c3t13.txt", header=TRUE)
str(d)

dts <- ts(data=d, frequency = 12, start=c(2015, 4))

event <- factor(dts[,"Event"], levels=1:3)
event
levels(event)

reg1 <- tslm(Mustard ~ trend + season + event, data=dts)
summary(reg1)

autoplot(dts[,"Mustard"], series = "Data") +
  autolayer(fitted(reg1), series="Fitted") +
  ylab("Mustard")

newdata <- data.frame(trend=28:33,
                      season=factor(7:12, levels=1:12),
                      event=factor(c(1,1,1,2,1,3), levels=1:3))
fc <- forecast(reg1, newdata=newdata, levels=c(90,95))
fc

autoplot(dts[,"Mustard"], series="Data") +
  autolayer(fitted(reg1), series="Fitted") +
  autolayer(fc, series='Forecast') +
  ylab("Mustard")

AIC(reg1)
CV(reg1)

#############################
# 11 - Non-linear regression
#############################

# Production example
d <- read.table('production.txt', header=TRUE)

qplot(d$Batch, d$Time, xlab='Batch', ylab='Time')

ggplot(data=d) + geom_point(aes(x=Batch, y=Time)) +
  geom_smooth(aes(x=Batch, y=Time), method='lm', formula=y~x) +
  
d <- d %>% mutate(Batch_log = log(Batch), Time_log = log(Time))

ggplot(data=d) + geom_point(aes(x=Batch_log, y=Time_log)) +
  geom_smooth(aes(x=Batch_log, y=Time_log), method='lm', formula=y~x) +
  xlab('Log(Batch)') + ylab('Log(Timee')

reg1 <- lm(Time_log ~ Batch_log, data=d)
summary(reg1)

ggplot(data=d, aes(x=Batch, y=Time)) + geom_point() +
  geom_line(aes(y=exp(fitted(reg1))), size=1, color='red') + 
  xlab('Batch') + ylab('Time')

# forecast
newdata <- data.frame(Batch_log=log(23:37))
fc <- forecast(reg1, newdata=newdata)
fc$mean
exp(fc$mean)
sum(exp(fc$mean))

# Advertisement example
da <- read.table("Advert.txt", header=TRUE, colClasses=c('numeric','factor','numeric'))
str(da)

qplot(da$advert, da$sales, xlab='Advertisement', ylab='Sales')
ggplot(data=da, aes(x=advert, y=sales)) +
  geom_point() + 
  xlab('Advert') + ylab('Sales')

# Transform
da <- da %>% mutate(sales_log=log(sales), advert_log=log(advert)) %>% 
  mutate(advert_sq=advert^2, advert_cb=advert^3)
head(da)

# log-log
reg1 <- lm(sales_log ~ advert_log, data=da)
# log-linear
reg2 <- lm(sales_log ~ advert, data=da)
# linear-log
reg3 <- lm(sales ~ advert_log, data=da)
# poly 2
reg4 <- lm(sales ~ advert + advert_sq, data=da)
# poly 3
reg5 <- lm(sales ~ advert + advert_sq + advert_cb, data=da)

rbind(LOGLOG=CV(reg1),LOGLINEAR=CV(reg2),LINEARLOG=CV(reg3),POLY2=CV(reg4),POLY3=CV(reg5))

#############################
# 12 - Logistic Regression
#############################
d <- read.table("bidding.txt", header=TRUE, 
                colClasses = c("factor", "numeric", "numeric", "numeric", 
                               "factor"))
str(d)

# summary by group
d %>% 
  group_by(BiddingResult) %>% 
  summarise(meanPrice=mean(Price), meanDay=mean(Day), 
            meanWaste=mean(Waste), n=n())  

# visualizing
ggplot(d, aes(Price, BiddingResult)) + 
  geom_point(aes(color=factor(BiddingResult)))

ggplot(d, aes(Price, Day)) + 
  geom_point(aes(color=factor(BiddingResult)))

# logistic regression with one feature (price)
myLogit1 <- glm(BiddingResult~Price, data=d, family="binomial")
summary(myLogit1)

predict(myLogit1, newdata = data.frame(Price=48), type="response")
1/(1+exp(-sum(myLogit1$coefficients*c(1,48))))

pW <- predict(myLogit1, newdata=data.frame(Price=c(48)), type="response")
pL <- 1-pW
odds <- pW/pL
odds

exp(-0.1563404)
odds*exp(-0.1563404)
# e^(c*beta)

# classification
priceAmt <- data.frame(Price=c(40, 50, 60, 70))
pHat <- predict(myLogit1, newdata = priceAmt, type="response")
pHat
ifelse(pHat >= 0.5, "Win", "Lose")


# logistic regression with dummy variable
myLogit3 <- glm(BiddingResult ~ Price + Waste + Cert, 
                data=d, family="binomial")
summary(myLogit3)

myNewdata <- data.frame(Price=c(50,50), Waste=c(30,30), Cert=c("N","Y"))
predict(myLogit3, newdata = myNewdata, type="response")


# model accuracy
preMod1 <- predict(myLogit1, newdata=data.frame(Price=d$Price), type="response")
pred <- ifelse(preMod1 >= 0.5, "1", "0")
mean(pred == d$BiddingResult) # accuracy

tb1 <- table(pred, actual=d$BiddingResult)
tb1
confusionMatrix(tb1)

#############################
# 13 - Advanced Forecasting
#############################
da <- read.table("Advert.txt", header=TRUE)
smooth75 <- loess(da$sales ~ da$advert) %>% predict()
smooth30 <- loess(da$sales ~ da$advert, span=0.3) %>% predict()
smooth43 <- loess(da$sales ~ da$advert, span =  0.4301597) %>% predict()
plot(da$sales, x=da$advert, type="p", xlab="Advertising", ylab="Sales")
lines(smooth75, x=da$advert, col="blue") 
lines(smooth30, x=da$advert, col="red")
lines(smooth43, x=da$advert, col="green")
legend("bottomright", legend=c("s=0.75", "s=0.30", "s=0.43"), lty = rep(1,3),
       col=c("blue", "red", "green"))

ggplot(da, aes(x=advert,y=sales)) + geom_point() +
  geom_line(mapping=aes(y=smooth75, color='s = 0.75')) +
  geom_line(mapping=aes(y=smooth30, color='s = 0.30')) +
  geom_line(mapping=aes(y=smooth43, color='s = 0.43')) + 
  scale_color_manual(name='LOESS', values=c(
    's = 0.75'='blue',
    's = 0.30'='red',
    's = 0.43'='green'))


################
# NNAR
################

d <- read.table("c6p14.csv", header=TRUE, sep=",")

d$index <- 1:length(d$CS)  # create index variable
head(d)

y <- ts(d$CS, frequency = 4, start=c(1996,1))
autoplot(y)

# box-cox transformation with lambda=0 to ensure forecast stay positive
fitNN <- nnetar(y, lambda=0)
fitNN
forecast(fitNN, PI=TRUE, level=c(.8,.95), h=8) %>% autoplot()
