#--------------Decision Tree------------
A = data.frame(iris)
library(tree)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

m1 = tree(Species~., data = trd)

plot(m1)
text(m1)

pred = predict(m1,tsd)
cbind(pred,tsd$Species)

Q = ifelse(pred[,1] > 0.5,"Setosa",ifelse(pred[,2] > 0.5,"versicolor","virginica"))
Q1 = table(Q,tsd$Species)
sum(diag(Q1))

sum(diag(Q1))/nrow(tsd)

sum(diag(Q1))/nrow(tsd)*100

100-sum(diag(Q1))/nrow(tsd)*100

#--------------For Credit-----------
library(ISLR)
A = data.frame(Credit)
library(tree)

sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]

m1 = tree(Ethnicity~.,data = trd)
plot(m1)
text(m1)

pred = predict(m1,tsd)
cbind(pred,tsd$Ethnicity)

Q = ifelse(pred[,1] > 0.5,"African American",ifelse(pred[,2] > 0.5,"Asian","Caucasian"))

Q1 = table(Q,tsd$Ethnicity)

100-(sum(diag(Q1))/nrow(tsd))*100
