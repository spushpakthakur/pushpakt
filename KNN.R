library(ISLR)
library(caret)
A = data.frame(Hitters)

A = na.omit(A)
colnames(A)
set.seed(123)
sf = sample(2,nrow(A),replace = TRUE, prob = c(0.6,0.4))
trd = A[sf == 1,]
tsd = A[sf == 2,]

tc = trainControl(method = 'cv',number = 10)

set.seed(123)

model1 = train(League~.,
               data = trd,
               method = 'knn',
               trControl = tc,
               preProc = c("center","scale"))
PR = predict(model1,tsd)
model1