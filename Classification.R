library(glmnet)
library(ISLR)
str(ISLR::Credit)
A = data.frame(Credit)
fivenum(A$Income)
A$NEWINCOME = ifelse(A$Income>33.11,1,0)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.8,0.2))

trd = A[sf == 1,]
tsd = A[sf == 2,]

model1 = glm(NEWINCOME ~ Limit+Rating, data = trd)
pred = predict(model1,tsd)
pred
pred1 = ifelse(pred<=0.5,0,1)

cbind(pred1,tsd$NEWINCOME)
table(pred1,tsd$NEWINCOME)


#-----------------------

library(glmnet)
library(ISLR)
library(psych)
str(ISLR::Credit)
A = data.frame(Credit)
#pairs.panels(Credit)
#fivenum(A$Rating)
A$NEWRATING = ifelse(A$Income>344,1,0)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.8,0.2))

trd = A[sf == 1,]
tsd = A[sf == 2,]

model1 = glm(Rating ~ Balance, data = trd)
pred = predict(model1,tsd)
pred
pred1 = ifelse(pred<=0.5,0,1)

cbind(pred1,tsd$NEWRATING)
table(pred1,tsd$NEWRATING)
table(tsd$NEWRATING)


#-----------Same for GENDER---pending-----
library(glmnet)
library(ISLR)
str(ISLR::Credit)
A = data.frame(Credit)

#--------HITTERS-------------

