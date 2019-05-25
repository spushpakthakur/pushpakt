library(ISLR)
library(naivebayes)
library(e1071)

A = data.frame(iris)
str(A)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb = naive_bayes(Species ~ ., data = trd)

model_e1 = naiveBayes(Species ~ ., data = trd)

pred_nb = predict(model_nb,tsd)
pred_nb
table(pred_nb,tsd$Species)

pred_e1 = predict(model_e1,tsd)
pred_e1
table(pred_e1,tsd$Species)

#--------------CREDIT---------
library(naivebayes)
A = data.frame(Credit)
str(A)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb = naive_bayes(Ethnicity ~ ., data = trd)

model_e1 = naiveBayes(Ethnicity ~ ., data = trd)

pred_nb = predict(model_nb,tsd)
table(pred_nb,tsd$Ethnicity)

pred_e1 = predict(model_e1,tsd)
table(pred_e1,tsd$Ethnicity)

#--------------------
library(naivebayes)
A = data.frame(Credit)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb = naive_bayes(Married ~ . -Ethnicity, data = trd)

model_e1 = naiveBayes(Married ~ . -Ethnicity, data = trd)

pred_nb = predict(model_nb,tsd)
table(pred_nb,tsd$Married)

pred_e1 = predict(model_e1,tsd)
table(pred_e1,tsd$Married)

#-----------------MASS-Cars93--------------------
library(MASS)
MASS::Cars93
str(Cars93)
A = data.frame(Cars93)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))

trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb1 = naive_bayes(A$Airbags ~ ., data = trd)
pred_nb1 = predict(model_nb1,tsd)
table(pred_nb1,tsd$Airbags)