############################################
# Lec13 Advanced models
# Loess
# NNAR
###########################################
library(ggplot2)
library(forecast)
library(fpp2)

setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")


##############################################
# LOESS
##############################################

# Example: Advertising vs Sales (Revisit)
da <- read.table("Advert.txt", header=TRUE)
plot(da$advert, da$sales, xlab="Advertising", ylab="Sales")
qplot(da$advert, da$sales, xlab="Advertising", ylab="Sales")
ggplot(da, aes(advert,sales)) + geom_point() + xlab('Advertising') + ylab('Sales')

saleLo75 <- loess(da$sales ~ da$advert) # default span 0.75, degree=2
smooth75 <- predict(saleLo75)  # obtain smoothed values 
smooth75

plot(da$sales, x=da$advert, type="p", xlab="Advertising", ylab="Sales")
lines(smooth75, x=da$advert, col="blue") 

ggplot(da, aes(x=advert,y=sales)) + geom_point() +
  geom_line(mapping=aes(y=smooth75, color='s = 0.75')) + 
  scale_color_manual(name='LOESS', values=c('s = 0.75'='blue'))

smooth30 <- loess(da$sales ~ da$advert, span = 0.3) %>% predict()
plot(da$sales, x=da$advert, type="p", xlab="Advertising", ylab="Sales")
lines(smooth75, x=da$advert, col="blue") 
lines(smooth30, x=da$advert, col="red")

ggplot(da, aes(x=advert,y=sales)) + geom_point() +
  geom_line(mapping=aes(y=smooth75, color='s = 0.75')) +
  geom_line(mapping=aes(y=smooth30, color='s = 0.30')) + 
  scale_color_manual(name='LOESS', values=c(
    's = 0.75'='blue',
    's = 0.30'='red'))


help("loess")


# selecting span
# http://users.stat.umn.edu/~helwig/notes/smooth-notes.html#local-regression
loess.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  tune.loess <- function(s){
    lo <- loess(y ~ x, span = s)
    mean((lo$fitted - y)^2) / (1 - lo$trace.hat/nobs)^2
  }
  os <- optimize(tune.loess, interval = c(.01, .99))$minimum
  lo <- loess(y ~ x, span = os)
  list(x = x, y = lo$fitted, df = lo$trace.hat, span = os)
}

loess.gcv(da$sales, da$advert) # finding optimal span

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

# STL (Revisit)

#d <- read.table("c6p14.txt", header=TRUE)
d <- read.table("c6p14.csv", header=TRUE, sep=",")
head(d)

d$index <- 1:length(d$CS)  # create index variable
head(d)

y <- ts(d$CS, frequency = 4, start=c(1996,1))
autoplot(y)

y %>% stl(t.window = 3, s.window = "periodic") %>% 
  autoplot()

y %>% stl(t.window = 7, s.window = "periodic") %>% 
  autoplot()

y %>% mstl() %>% autoplot()

fc <- stlf(y)
autoplot(fc) +
  ylab("Car Sales")




##############################################
# NNAR
##############################################

# box-cox transformation with lambda=0 to ensure forecast stay positive
fitNN <- nnetar(y, lambda=0)
fitNN
forecast(fitNN, h=8)

# the network is trained using different random starting points
fitNN2 <- nnetar(y, lambda=0)
fitNN2    
forecast(fitNN2, h=8)

d <- read.table('c7Gap.csv', sep=',', header=TRUE)
dts <- ts(d$GapSales, start=c(2006,2), frequency=4)
dts
fitNN <- nnetar(dts, lambda=0) # boxcox transformation with lambda=0 to ensure forecast stay positive
fitNN

fNN <- forecast(fitNN, PI=TRUE, level=c(.95,.99), h=7)
fNN
autoplot(fNN)

##############################################
# Bagging
##############################################
etsfc <- y %>% ets() %>% forecast(h=8)
autoplot(etsfc)

baggedfc <- y %>% baggedETS() %>% forecast(h=8)
autoplot(baggedfc)

etsfc$mean
baggedfc$mean
autoplot(y) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE)

help("baggedETS")
snaivefc <- y %>%  snaive(h=8)
autoplot(snaivefc)

baggedsnfc <- baggedModel(y, bootstrapped_series=bld.mbb.bootstrap(y, 200), fn=snaive) %>% forecast(h=8)
autoplot(baggedsnfc)

autoplot(y) +
  autolayer(snaivefc, series="Seasonal naive", PI=FALSE) +
  autolayer(baggedsnfc, series="Bagged Seasonal Naive", PI=FALSE)




nnarfc <- forecast(nnetar(y, lambda=0), h=8)
baggednnarfc <- baggedModel(y, bootstrapped_series=bld.mbb.bootstrap(y, 200), 
                            fn=nnetar, lambda=0) %>% forecast(h=8)
autoplot(nnarfc)
autoplot(baggednnarfc)

autoplot(y) +
  autolayer(nnarfc, series="NNETAR", PI=FALSE) +
  autolayer(baggednnarfc, series="Bagged NNETAR", PI=FALSE)

nnarfc <- forecast(nnetar(y), h=8)
baggednnarfc <- baggedModel(y, bootstrapped_series=bld.mbb.bootstrap(y, 200), 
                            fn=nnetar) %>% forecast(h=8)
nnarfc
baggednnarfc

autoplot(y) +
  autolayer(nnarfc, series="NNETAR", PI=FALSE) +
  autolayer(baggednnarfc, series="Bagged NNETAR", PI=FALSE)
