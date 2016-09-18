##Read data
abalone <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",header = FALSE)
head(abalone)
any(is.na(abalone))
str(abalone)

require(ggplot2)
ggplot(abalone,aes(V9)) + geom_histogram(aes(fill = V1))
require(caTools)
set.seed(101)
sample <- sample.split(abalone,SplitRatio = 0.7)
train  <- subset(abalone,sample == TRUE)
test <- subset(abalone,sample == FALSE)
head(train)

model <- lm(V9 ~ .,data = train)
summary(model)
require(dplyr)
final.test <- select(test, -9)
str(final.test)

Age.predict <- predict(model,final.test)
test$Age.predict <- Age.predict
head(test)

ggplot(test,aes(V9,Age.predict)) + geom_point()
