##Read data 
bc <- read.csv("Analysis.csv", header = F)
head(bc)
write.csv(bc,"Analysis.csv")

##Check for Missing Values
any(is.na(bc))

## Structure and Summary of Dataset
str(bc)
summary(bc)
table(bc$V7)

##Checking for Correlation, seems like V7 is correlated with V3,V4 and V11
require(corrplot)
require(corrgram)
bc.correlation <- subset(bc,bc$V7 != "?")
bc.correlation$V7 <- as.numeric(bc.correlation$V7)
str(bc.correlation)
corrgram(bc.correlation)

##Simple Logistic Regression



