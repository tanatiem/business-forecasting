############################################
# Lec11 Multiple Regression Nonlinear
###########################################

library(fpp2)
library(forecast)

library(dplyr)

setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")


#####################
# Production Example
# log-log
d <- read.table('production.txt', header=TRUE)

qplot(d$Batch, d$Time, xlab='Batch', ylab='Time')
ggplot(data=d) + geom_point(aes(x=Batch, y=Time)) +
  geom_smooth(aes(x=Batch, y=Time), method='lm', formula=y~x)

d <- d %>% mutate(Batch_log = log(Batch)) %>%
  mutate(Time_log = log(Time))
d
ggplot(data=d) + geom_point(aes(x=Batch_log, y=Time_log)) +
  geom_smooth(aes(x=Batch_log, y=Time_log), method='lm', formula=y~x) +
  xlab('Log(Batch)') + ylab('Log(Timee')

reg1 <- lm(Time_log ~ Batch_log, data=d)
summary(reg1)


ggplot(data=d, aes(x=Batch_log, y=Time_log)) + geom_point() +
  geom_line(aes(y=fitted(reg1)), size=1, color='red') + 
  xlab('Log(Batch)') + ylab('Log(Time')

ggplot(data=d, aes(x=Batch, y=Time)) + geom_point() +
  geom_line(aes(y=exp(fitted(reg1))), size=1, color='red')


newdata <- data.frame(Batch_log=log(23:37))
newdata
fc <- forecast(reg1, newdata=newdata)
fc$mean
exp(fc$mean)
sum(exp(fc$mean))

################################
# Heteroscedasticity example
################################
autoplot(eggs)

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80, biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series='Simple back transformation') +
  autolayer(fc2, series='Bias adjusted', PI=FALSE) +
  guides(colour=guide_legend(title='Forecast'))

######################
# Polynomial regression
######################
da <- read.table("Advert.txt", header=TRUE)
str(da)
qplot(da$advert, da$sales, xlab="Advertising", ylab="Sales")

da <- mutate(da, advSq=advert^2)  # add new variables into data frame
str(da)

da <- da %>% mutate(advSq = advert^2 )  # same thing using pipe %>% 
str(da)

da <- da %>% mutate(advCb = advert^3 )  # same thing using pipe %>% 
str(da)

da
# linear model
mod1 <- lm(sales ~ advert, data=da)
mod1

# quadratic model
mod2 <- lm(sales ~ advert + advSq, data=da)
mod2

# cubic model
mod3 <- lm(sales ~ advert + advSq + advCb, data=da)
mod3

CV(mod1)
CV(mod2)
CV(mod3)

######################
# Linear Regression Splines
######################
library(gridExtra)
h <- 10

boston_men <- window(marathon, start=1924)
autoplot(boston_men)

fit.lin <- tslm(boston_men ~ trend)
fcasts.lin <- forecast(fit.lin, h=h)

p1 <- autoplot(fcasts.lin) + 
  autolayer(fitted(fit.lin), series='Fitted') +
  xlab('Year') + ylab('Winning times in minutes')
p2 <- autoplot(residuals(fit.lin)) + ylab('Residuals')
grid.arrange(p1, p2, nrow=2)

fit.exp <- tslm(boston_men ~ trend, lambda=0)
fcasts.exp <- forecast(fit.exp, h=h)


t <- time(boston_men)
t.break1 <- 1950
t.break2 <- 1980

tb1 <- ts(pmax(0, t - t.break1), start=1924)
tb2 <- ts(pmax(0, t - t.break2), start=1924)

fit.pw <- tslm(boston_men ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(boston_men ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata=newdata)

autoplot(boston_men) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

boston_men %>% 
  splinef(lambda=0) %>%
  autoplot()

boston_men %>%
  splinef(lambda=0) %>%
  checkresiduals()
