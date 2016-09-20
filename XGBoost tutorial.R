## Setting environment
require(xgboost)

##using agaricus dataset which comes from package
data("agaricus.test")
data("agaricus.train")

##train and test data
train <- agaricus.train
test <- agaricus.test
str(test)

## XGboost mode
bst <- xgboost(data = train$data, label = train$label,nround = 4, objective = "binary:logistic", eval_metric = "auc")

##Predict
pred <- predict(bst,test$data)
head(pred)

##Cross validation

cv.res <- xgb.cv(data = train$data, label = train$label, nround = 4, nfold = 5
                 , objective = "binary:logistic", eval_metric = "auc")

cv.res

