##Reading data set in

airfoil <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat",sep = "\t",header = FALSE)
head(airfoil)
dim(airfoil)

## Names of Dataset

names(airfoil) <- c("Freq", "AOA", "C.length", "FS.velocity", "SSD.thick", "S.pressure")
head(airfoil)

## Check for any missing values

any(is.na(airfoil))

##Checking for correlation
require(corrplot)
airfoil.cor <- cor(airfoil)
airfoil.cor
corrplot(airfoil.cor,method = "ellipse")
## Seems like ssd.thick and Angle of attach are slightly correlated but not strong.

## Best subset.

require(leaps)
fit <- lm(S.pressure ~ ., data = airfoil)
summary(fit)
sub.fit <- regsubsets(S.pressure ~ ., data = airfoil)
best.summary <- summary(sub.fit)

par(mfrow = c(2,2))
plot(best.summary$cp, xlab = "number of features", ylab = "Cp")

plot(sub.fit, scale = "Cp")

which.min(best.summary$bic)
which.max(best.summary$adjr2)

plot(fit)

require(car)
vif(fit)

require(lmtest)
bptest(fit)

par(mfrow = c(1,1))
plot(fit$fitted.values,airfoil$S.pressure, xlab = "Predicted", ylab = "Actuals")
airfoil$Actuals <- airfoil$S.pressure
airfoil$predicted <- predict(fit)

require(ggplot2)
ggplot(airfoil, aes(x=predicted, y=Actuals)) + geom_point() + geom_smooth(method = lm)

require(MPV)
PRESS(fit)

?PRESS
