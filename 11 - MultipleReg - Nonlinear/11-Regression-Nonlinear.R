############################################
# Lec11 Multiple Regression Nonlinear
###########################################

library(fpp2)
library(forecast)

library(dplyr)

setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")




######################
# Polynomial regression
######################
da <- read.table("Advert.txt", header=TRUE)
str(da)
qqplot(da$advert, da$sales, xlab="Advertising", ylab="Sales")

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

