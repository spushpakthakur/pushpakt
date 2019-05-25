#-----------Simple Linear Regression------------
library(ISLR)
library(psych)
A = data.frame(Auto)
A1 = na.omit(A)

sf = sample(2, nrow(A1), replace = TRUE, prob = c(0.7,0.3))
trd = A1[sf == 1,]
tsd = A1[sf == 2,]

str(A1)
pairs.panels(A1)
cor(A[,1:8])

model = lm(displacement ~ cylinders, data = trd)

summary(model)

model1 = lm(displacement ~ cylinders+horsepower, data = trd)
summary(model1)

pred = predict(model,tsd)
cbind(tsd$displacement,pred)

plot(trd$cylinders, trd$displacement)

abline(-127.567,58.842)



#-----------Multiple Linear Regression------------
library(MASS)
library(psych)
A = data.frame(Hitters)
str(A)

cor(A[,c(-14,-15,-20)])

sf = sample(2,nrow(A),replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model1 = lm(AtBat~., data = trd)
summary(model1)

model2 = lm(AtBat~.-League-Division-NewLeague, data = trd)
summary(model1)

pred = predict(model1,tsd)
cbind(trd$AtBat,pred)


#------------Hitters------------
library(MASS)
library(psych)
A = data.frame(Cars93)
A1 = na.omit(A)
str(A1)
cor(A1[,c(-1,-2,-3,-9,-10,-11,-16,-26,-27)])

sf = sample(2,nrow(A1), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model2 = lm(Min.Price ~ .,data = trd)
summary(model2)


#-------------Cars(93)-------------
library(ISLR)
library(psych)
library(MASS)
A = data.frame(iris)
A1 = na.omit(A)
str(A1)

sf = sample(2,nrow(A1), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

pairs.panels(A1)
model2 = lm(Sepal.Length ~ .,data = trd)
summary(model2)

model3 = lm(Sepal.Length ~ Petal.Length+Petal.Width+Species,data = trd)
summary(model3)

View(A1)
table(iris)

pred = predict(model2,tsd)
pred
cbind(pred,tsd$Sepal.Length)

A1
tbp = A[c(1,70,150),2:5]
tbp
predict(model2,tbp)
predict(model2,tsd)
