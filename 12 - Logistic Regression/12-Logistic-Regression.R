############################################
# Lec12 Logistic Regression
###########################################
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)


#setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")

# incorrect data type
d <- read.table("bidding.txt", header=TRUE)
str(d)

# correct data type
d <- read.table("bidding.txt", header=TRUE, 
                  colClasses = c("factor", "numeric", "numeric", "numeric", 
                                 "factor"))
str(d)

# average price for each group (win, lose)
mean(d$Price[d$BiddingResult=="0"])

# summary by group
d %>% 
  group_by(BiddingResult) %>% 
  summarise(meanPrice=mean(Price), meanDay=mean(Day), 
            meanWaste=mean(Waste), n=n())  


# visualizing
p <- ggplot(d[185:200,], aes(Price, BiddingResult))
p + geom_point(aes(color=factor(BiddingResult)))

p <- ggplot(d[185:200,], aes(Price, Day))
p + geom_point(aes(color=factor(BiddingResult)))


# logistic regression with one feature (price)
myLogit1 <- glm(BiddingResult~Price, data=d, family="binomial")
summary(myLogit1)

predict(myLogit1, newdata = data.frame(Price=57), type="response")
1/(1+exp(-sum(myLogit1$coefficients*c(1,57))))

predict(myLogit1, newdata = data.frame(Price=48), type="response")
1/(1+exp(-sum(myLogit1$coefficients*c(1,48))))

pW <- predict(myLogit1, newdata=data.frame(Price=c(48)), type="response")
pL <- 1-pW
odds <- pW/pL
odds


# interpreting coefficient
pW <- predict(myLogit1, newdata=data.frame(Price=c(48,49)), type="response")
pW
pL <- 1-pW
odds <- pW/pL
odds
logOdds <- log(odds)
logOdds[2] - logOdds[1]

odds[2]
odds[1]*exp(-0.1563404)
# e^(c*beta)

# classification
priceAmt <- data.frame(Price=c(40, 50, 60, 70))
pHat <- predict(myLogit1, newdata = priceAmt, type="response")
pHat
ifelse(pHat >= 0.5, "Win", "Lose")

# logistic regression with two features (price + day)

p <- ggplot(d, aes(Price, Waste))
p + geom_point(aes(color=factor(BiddingResult)))

myLogit2 <- glm(BiddingResult ~ Price + Waste, data=d, family="binomial")
summary(myLogit2)

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


