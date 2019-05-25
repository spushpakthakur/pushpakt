#-----------------Naive Bayes on INCOME(CREDIT DATA SET)------------
library(naivebayes)
A = data.frame(Credit)
str(A)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb = naive_bayes(Income ~ ., data = trd)

pred_nb = predict(model_nb,tsd)
table(pred_nb,tsd$Income)

#-----------------Decision Tree on INCOME(CREDIT DATA SET)------------
A = data.frame(Credit)
library(tree)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

m1 = tree(Income~., data = trd)

plot(m1)
text(m1)

pred = predict(m1,tsd)
cbind(pred,tsd$Income)
P = table(pred,tsd$Income)

sum(diag(P))

#----------------NaiveBayes---------
library(naivebayes)
A = data.frame(Credit)
str(A)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

model_nb = naive_bayes(Ethnicity ~ ., data = trd)

pred_nb = predict(model_nb,tsd)
table(pred_nb,tsd$Ethnicity)