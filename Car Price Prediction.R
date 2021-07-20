data=read.csv("C:/Users/lenovo/OneDrive/Documents/Python_NMIMS.csv")
head(data)
data=data[,-1]
set.seed(234)
library(caret)
training.samples <- createDataPartition(data$price, p = 0.6, list = FALSE, groups=2)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
attach(train.data)
library(corrplot)
c=cor(train.data)
c
par(mfrow=c(2,2))
corrplot(c)
corrplot.mixed(c,lower.col="black")
corrplot.mixed(c)
corrplot.mixed(c, lower.col = "black")
model1=lm(price~., data=train.data)
summary(model1)
model2=lm(price~enginelocation+carwidth+enginesize+stroke+horsepower, data=train.data)
summary(model2)
model3=lm(price~enginelocation+carwidth+enginesize+horsepower, data=train.data)
summary(model3)
predictions <- predict(object=model3, newdata=test.data)  #predict for test dataset
pred=data.frame(predictions,test.data$price)
library(tibble);view(pred)
RMSE(predictions, test.data$price)
mean(test.data$price)
RMSE(predictions, test.data$price)/mean(test.data$price)
R2(predictions, test.data$price)
plot(model3)

library(lmtest)
bptest(model3)
shapiro.test(model3$residuals)
car::vif(model3)
library(car)

library(MASS)
bc=boxcox(model3)
#first line lower limit of ci, middle line value of lambda and last line upper limit
best.lambda=bc$x[which(bc$y==max(bc$y))]
best.lambda
model3_cox = lm((price ^ best.lambda)  ~ enginelocation+carwidth+enginesize+horsepower, data=train.data)
plot(model3_cox)
library(lmtest)
bptest(model3_cox)
shapiro.test(model3_cox$residuals)
car::vif(model3_cox)
library(car)



