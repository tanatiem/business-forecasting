#############################################################################
# Lec14 Predictive Analytics
#############################################################################
library(dplyr)
library(ggplot2)
library(caret) # Classification And REgression Training
library(mclust)


#############################################################################
# Classification
#############################################################################

# compute accuracy on training and testing data
computeAccuracy <- function(myModel, dtrain, dtest){
  yTrain <- predict(myModel, type="raw")
  
  # dtrain[,1]: target is in column 1
  accTrain <- confusionMatrix(yTrain, dtrain[,1])$overall[1]  # accuracy
  
  yTest <- predict(myModel, newdata=dtest, type="raw")
  accTest <- confusionMatrix(yTest, dtest[,1])$overall[1]
  
  out <- c(Train=accTrain, Test=accTest)
  return(out)
}


#############################################################################
# Classification: Example 1
#############################################################################
setwd("C:/Users/Admin/Google Drive/Mai/Teaching/Courses/LM7204-BizForecasting/Excel-R")


d <- read.table("bidding.txt", header=TRUE, 
                colClasses = c("factor", "numeric", "numeric", "numeric", 
                               "factor"))

# data exploration
d %>% 
  group_by(BiddingResult) %>% 
  summarise(meanPrice=mean(Price), meanDay=mean(Day), 
            meanWaste=mean(Waste), n=n()) 

p <- ggplot(d, aes(Price, Waste))
p + geom_point(aes(color=factor(BiddingResult)))


# training and testing
n <- nrow(d)
nTrain <- floor(0.8*n)  # 80% training data
set.seed(123)   # set the seed to make your partition reproducible
trainInd <- sample(1:n, size = nTrain)


dtrain <- d[trainInd,]  
dtest <- d[-trainInd,]


# setting up cross validation
set.seed(1234)
cvcontrol <- trainControl(method="cv", number=4)

# knn
set.seed(1234) # freeze result from tuning
mtrain <- train(BiddingResult~., data=dtrain, method="knn",  
                   tuneGrid = expand.grid(k=1:10),
                   metric = "Accuracy",
                   trControl=cvcontrol)
mtrain
computeAccuracy(mtrain, dtrain, dtest)

yTrain <- predict(mtrain, type="raw")
accTrain <- confusionMatrix(yTrain, dtrain[,1])$overall[1] 
accTrain

yTest <- predict(mtrain, newdata=dtest, type="raw")
accTest <- confusionMatrix(yTest, dtest[,1])$overall[1] 
accTest

help("train")
modelLookup("knn")
names(getModelInfo())


#############################################################################
# Classification: Example 2
#############################################################################

d <- read.table("universalBank.txt", header=TRUE, 
                            colClasses = c(
                              "PersonalLoan"="factor",
                              "Age"="numeric", 
                              "Experience"="numeric",
                              "Income"="numeric",
                              "ZIPCode"="character",
                              "Family"="numeric",
                              "CCAvg"="numeric",
                              "Education"="factor",
                              "Mortgage"="numeric",
                              "SecuritiesAccount"="factor",
                              "CDAccount"="factor",
                              "Online"="factor",
                              "CreditCard"="factor"
                            ))
str(d)

n <- nrow(d)
nTrain <- floor(0.6*n)
set.seed(123)   # set the seed to make your partition reproducible
trainInd <- sample(1:n, size = nTrain)

# setting up cross validation
dtrain <- d[trainInd,-5] # suppose we do not use zip code (column 5)
dtest <- d[-trainInd,-5]
set.seed(1234)
cvcontrol <- trainControl(method="cv", number=10)

# n = 5000
# n train = 0.6*5000 = 3000
# 10 fold = holdout = 3000/10 = 300

# knn
set.seed(888)
knn.train <- train(PersonalLoan~., data=dtrain, method="knn",  
                   tuneGrid = expand.grid(k=1:10),
                   metric = "Accuracy",
                   trControl=cvcontrol)
knn.train
computeAccuracy(knn.train, dtrain, dtest)


# glm
glm.train <- train(PersonalLoan~., data=dtrain, method="glm",  
                   trControl=cvcontrol)
summary(glm.train)

glm(PersonalLoan~., data=dtrain, family = "binomial")
computeAccuracy(glm.train, dtrain, dtest)

# classification tree
tree.train <- train(PersonalLoan~., data=dtrain, method="ctree",  
                    trControl=cvcontrol)
tree.train
computeAccuracy(tree.train, dtrain, dtest)

modelLookup("ctree")




#############################################################################
# Clustering
#############################################################################

#############################
# hclust
#############################
de <- read.table("cluster-example.txt", header = TRUE)
plot(de$Income, de$Education, xlab="Income", 
     ylab="Education")
myDist <- dist(as.matrix(de[,2:3]))
myDist
hc <- hclust(myDist)
plot(hc)
rect.hclust(hc, k=2, border=2:3)

cutAvg <- cutree(hc, k=2)
de <- de %>% mutate(cluster=cutAvg)
de %>% 
  group_by(cluster) %>% 
  summarise(meanIncom=mean(Income), 
            meanEdu=mean(Education), n=n()) 



#############################
# k-mean clustering
#############################

d <- read.table("clusteringExample.txt", header=TRUE)
plot(d$CustomerCount, d$Revenue, xlab="CustomerCount", 
     ylab="Revenue")
# column 2: CustomerCount
# column 3: Revenue
d.kmean <- kmeans(d[,2:3],3)  # three clusters
df <- data.frame(d, group=d.kmean$cluster)

df %>%  ggplot(aes(y = Revenue , x = CustomerCount, 
                   color = factor(group))) +
  geom_point() +
  ylab("Revenue") +
  xlab("CustomerCount") +
  scale_color_discrete(name = "Group") +
  ggtitle("K-means Clustering")

