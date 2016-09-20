##Read data 
bc <- read.csv("Analysis.csv", header = F)
head(bc)
bc$V11 <- ifelse(bc$V11 == 2, 0, 1)

##Check for Missing Values
any(is.na(bc))

## Structure and Summary of Dataset
str(bc)
summary(bc)
table(bc$V7)

##Checking for Correlation, seems like V7 is correlated with V3,V4 and V11
require(corrplot)
require(corrgram)
rm(bc.correlation)
corrgram(bc)


##Divide Train and Test data
require(caTools)
sample <- sample.split(bc,SplitRatio = 0.7)
bc.train <- subset(bc,sample == TRUE)
bc.test <- subset(bc,sample == FALSE)
dim(bc.train)
dim(bc.test)

##Simple Logistic Regression - 5.11811% Missclassification
log.model <- glm(V11 ~ .-V1, family = binomial(link = "logit"), data = bc.train)
summary(log.model)
fitted.probabilities <- predict(log.model,bc.test, type = "response")
fitted.probabilities <- ifelse(fitted.probabilities <= 0.5, 0, 1)
head(fitted.probabilities)
table(bc.test$V11,fitted.probabilities)
logistic.missclassification <- 13/NROW(bc.test)
logistic.missclassification

##Decision Tree model : tree.missclassification- 0.06692913
require(rpart)
tree.model <- rpart(V11 ~ .-V1, method = "class", data = bc.train)
plotcp(tree.model)
printcp(tree.model)
plot(tree.model)
text(tree.model)
tree.predict <- predict(tree.model,bc.test)
dim(bc.test)
dim(tree.predict)
tree.predict <- as.data.frame(tree.predict)
tree.predict$Final <- ifelse(tree.predict$`0`>=0.5,0,1)
tree.predict
table(bc.test$V11,tree.predict$Final)
tree.missclassification <- 17/NROW(bc.test)
tree.missclassification

##Random Forest - forest.missclassification - 0.03543307
require(randomForest)
bc.train$V11 <- as.factor(bc.train$V11)
str(bc.train)
rf.model <- randomForest(V11 ~ .-V1, bc.train)
rf.model
forest.predict <- predict(rf.model,bc.test)
NROW(forest.predict)
table(bc.test$V11,forest.predict)
forest.missclassification <- 9/NROW(bc.test)
forest.missclassification

##Support Vector Machines - svm.missclassification:0.03543307
##svm.tuned.missclassifcation: 0.02755906
require(e1071)
model.svm <- svm(V11 ~ .-V1, bc.train)
summary(model.svm)
pred.svm <- predict(model.svm,bc.test)
pred.svm
table(bc.test$V11,pred.svm)
##missclassification without tuning:
svm.missclassification <- 9/NROW(bc.test)
svm.missclassification
##Tuning SVM model

obj <- tune(svm, V11~. -V1, data = bc.train, 
            ranges = list(gamma = seq(0.5,10,0.5), cost = seq(0.1,10,0.1)),
            tunecontrol = tune.control(sampling = "fix"))

summary(obj)
obj
##Best model gamma = 0.5, cost = 3.5
model.svm.tune <- svm(V11 ~ .-V1, bc.train, cost = 3.5, gamma = 0.5)
summary(model.svm.tune)
pred.svm.tune <- predict(model.svm.tune,bc.test)
table(bc.test$V11,pred.svm.tune)
svm.tuned.missclassifcation <- 7/NROW(bc.test)
svm.tuned.missclassifcation

##XGBoost: XGboost.misclassification - 0.04330709
str(bc.train)
bc.train$V1 <- as.numeric(bc.train$V1)
bc.train$V2 <- as.numeric(bc.train$V2)
bc.train$V3 <- as.numeric(bc.train$V3)
bc.train$V4 <- as.numeric(bc.train$V4)
bc.train$V5 <- as.numeric(bc.train$V5)
bc.train$V6 <- as.numeric(bc.train$V6)
bc.train$V7 <- as.numeric(bc.train$V7)
bc.train$V8 <- as.numeric(bc.train$V8)
bc.train$V9 <- as.numeric(bc.train$V9)
bc.train$V10 <- as.numeric(bc.train$V10)

Xgmat=data.matrix(select(bc.train,2:10))
label <- bc.train$V11
label <- as.numeric(levels(label))[label]

all_train<-xgb.DMatrix(data=Xgmat, label=label)

param = list("objective" = "binary:logistic",
             "bst:eta" = 0.15,
             "bst:max_depth" = 3,
             "eval_metric" = "auc",
             "silent" = 1,
             "nthread" = 16)
clf_best <- xgboost(params        = param, 
                    data                = all_train, 
                    nrounds             = 400, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    gamma               = 0.5,
                    booster             = "gbtree",
                    #early.stop.round    = 200,
                    #watchlist           = watchlist,
                    maximize            = FALSE,
                    eval_metric="auc"
                    #nfold=3
)



bc.test$V1 <- as.numeric(bc.test$V1)
bc.test$V2 <- as.numeric(bc.test$V2)
bc.test$V3 <- as.numeric(bc.test$V3)
bc.test$V4 <- as.numeric(bc.test$V4)
bc.test$V5 <- as.numeric(bc.test$V5)
bc.test$V6 <- as.numeric(bc.test$V6)
bc.test$V7 <- as.numeric(bc.test$V7)
bc.test$V8 <- as.numeric(bc.test$V8)
bc.test$V9 <- as.numeric(bc.test$V9)
bc.test$V10 <- as.numeric(bc.test$V10)

final.bc.test <- as.matrix(bc.test[2:10])
test.dMatric <- xgb.DMatrix(final.bc.test)
ypred <- predict(clf_best,test.dMatric)
ypred <- as.data.frame(ypred)
ypred$ypred <- ifelse(ypred$ypred < 0.5,0,1)
table(bc.test$V11,ypred$ypred)
XGboost.misclassification <- 11/NROW(bc.test)
XGboost.misclassification
