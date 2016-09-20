##Read data 
bc <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", header = F)


##Checking for any missing values
any(is.na(bc))
str(bc)
levels(anneal$V39)
ggplot(bc,aes(V7)) + geom_bar()
bc[bc$V7 == "?",]$V7
table(bc$V7)
bc$V7 <- as.numeric(bc$V7)
str(bc)
sample <- sample.split(bc,SplitRatio = 0.7)

train.bc <- subset(bc,sample == T)
test.bc <- subset(bc,sample == F)
Xgmat=data.matrix(select(train.bc,2:10))
NROW(Xgmat)

label <- select(train.bc,11)
NROW(label)
label
NROW(output_vector)
all_train<-xgb.DMatrix(data=Xgmat,label=label)
require(xgboost)
NROW(Xgmat)
length(output_vector)

label=(train.bc[,c("V11")]
)
label
length(label)
label <- ifelse(label == 2,1,0)

param <- list(objective           = "binary:logistic", 
              booster             = "gbtree",
              eta                 = 0.1, # 0.06, #0.01,0.005
              max_depth           = 4, #changed from default of 4,6,8,10,15,20
              subsample           = 0.5, #(.5,0.7,1)
              colsample_bytree    = 0.5, #(.5,0.7,1)
              ##min_child_weight=44.8833  ## 3/ Event rate - Rule of Thumb 
)

param = list("objective" = "binary:logistic",
             "bst:eta" = 0.1,
             "bst:max_depth" = 10,
             "eval_metric" = "auc",
             "silent" = 1,
             "nthread" = 16)

clf_best <- xgboost(params        = param, 
                    data                = all_train, 
                    nrounds             = 600, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    #early.stop.round    = 200,
                    #watchlist           = watchlist,
                    maximize            = FALSE,
                    eval_metric="auc"
                    #nfold=3
)

final.test.bc <- as.matrix(test.bc[2:10])
test.dMatric <- xgb.DMatrix(final.test.bc)

ypred <- predict(clf_best,test.dMatric)
ypred <- as.data.frame(ypred)
ypred <- ifelse(ypred$ypred > 0.5,2,4)
test.bc$pred <- ypred
table(test.bc$V11,test.bc$pred)
head(test.bc)
11/(81+163)
