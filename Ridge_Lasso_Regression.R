library(caret)
library(glmnet)
library(ISLR)
A = data.frame(Credit)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,] 

str(A)
cor(A[,c(2,3,4,5,6,12)])
#----------SIMPLE MODEL-------
m1 = lm(Limit ~ Income + Rating + Balance, data = trd)
pred = predict(m1,tsd)
cbind(pred,tsd$Limit)
summary(m1)
#-----------RIDGE MODEL--------
tc = trainControl(method = "cv", number = 10,verboseIter = T)
A1 = na.omit(A)
model1 = train(Limit ~ Income + Rating + Balance, method = "glmnet", data = A,
               trControl = tc, tuneGrid = expand.grid(alpha = 0,
               lambda = seq(0,500,length = 50)))
pred1 = predict(model1,tsd)
Q2 = cbind(pred1,pred,tsd$Limit)
varImp(model1)
#--------------LASSo------------
model2 = train(Limit ~ Income + Rating + Balance, method = "glmnet", data = A,
               trControl = tc, tuneGrid = expand.grid(alpha = 1,
               lambda = seq(0,500,length = 50)))
pred2 = predict(model2,tsd)
Q2 = cbind(pred2,pred1,pred,tsd$Limit,tsd$Limit-pred2,tsd$Limit-pred1,tsd$Limit-pred)

