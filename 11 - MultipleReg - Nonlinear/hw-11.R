######################
# Homework 11
######################

library(fpp2)
library(forecast)
library(ggplot2)

library(dplyr)

da <- read.table("Advert.txt", header=TRUE, colClasses=c('numeric','factor','numeric'))
str(da)
ggplot(data=da, aes(x=advert, y=sales)) +
  geom_point() + 
  xlab('Advert') + ylab('Sales')

# Transform
da <- da %>% mutate(advert_sq=advert^2) %>%
  mutate(advert_log=log(advert)) %>% 
  mutate(sales_log=log(sales))
head(da)

# Model1: log-log
reg1 <- lm(sales_log ~ advert_log, data=da)
summary(reg1)
AIC(reg1)
CV(reg1)

ggplot(data=da, aes(x=advert_log, y=sales_log)) +
  geom_point() +
  geom_line(aes(y=fitted(reg1)), size=1, color='red') +
  xlab("Log(Advert)") + ylab("Log(Sales)")

# 1% increase in Advert -> 0.09% increase in Sales

# Model2: Quadratic
reg2 <- lm(sales ~ advert + advert_sq, data=da)
summary(reg2)
AIC(reg2)
CV(reg2)


ggplot(data=da, aes(x=advert, y=sales)) +
  geom_point() +
  geom_line(aes(y=exp(fitted(reg1)),color='red'), size=1) +
  geom_line(aes(y=fitted(reg2), color='blue'), size=1) +
  xlab('Advert') + ylab('Sales') +
  scale_color_identity(name = "Model fit",
                     breaks = c("red", "blue"),
                     labels = c("Log-Log", "Quadratic"),
                     guide = "legend")
                   

                   