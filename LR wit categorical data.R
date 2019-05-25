library(ISLR)
ISLR::Hitters
A = data.frame(iris)

library(psych)
pairs.panels(iris)

str(A)
cor(A)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model1 = lm(Sepal.Length ~ .,data = trd)
summary(model1)

model2 = lm(Sepal.Length ~ Petal.Length + Petal.Width + Species, data = trd)

model2

summary(model2)

View(A)
table(iris)

A


tbp = A[c(1,70,150),2:5]
predict(model1,tbp)
tbp = A[c(1,70,150),2:5]
predict(model1,tsd)
str(A)
pred = predict(model1,tsd)
cbind(pred,tsd$Sepal.Length)