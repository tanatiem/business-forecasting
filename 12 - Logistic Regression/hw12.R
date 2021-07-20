library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(reshape2)

d <- read.table('shipment.csv', sep=',', header=TRUE,
                colClasses=c('factor','numeric','numeric','factor'))
str(d)

head(d)

d %>% group_by(Late) %>%
  summarise(AvgDistance=mean(Distance), AvgWeight=mean(Weight), n=n())

# EDA
ggplot(d, aes(Distance, Weight)) +
  geom_point(aes(color=Late)) +
  facet_wrap(~LSPRank)

# count plot
ggplot(d, aes(fill=Late, x=LSPRank)) + 
  geom_bar(position="dodge")

# normalized count plot
d %>% group_by(LSPRank) %>% 
  summarise(Late.pct=mean(Late=='1'),NotLate.pct=mean(Late=='0')) %>%
  melt(variable.name='Status') %>%
  ggplot(aes(x=LSPRank, y=value, fill=Status)) +
    geom_bar(position='stack', stat='identity') +
    ylab('Status Proportion')

logreg <- glm(Late~Distance + Weight + LSPRank, data=d, family="binomial")
summary(logreg)

newdata <- data.frame(Distance=680,Weight=3.7,LSPRank=factor(2, levels=1:4))
newdata

y_pred <- predict(logreg, newdata=newdata ,type='response')
y_pred
ifelse(y_pred >= 0.5, 'Late', 'Not Late')

