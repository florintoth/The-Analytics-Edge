# STATE DATA REVISITED (OPTIONAL)

data(state)
statedata = data.frame(state.x77)
str(statedata)

# Problem 1
# Linear Regression

RegModel = lm(Life.Exp ~ ., data=statedata)
summary(RegModel)

Predictions = predict(RegModel)
sum((statedata$Life.Exp - Predictions)^2) # or
sum(RegModel$residuals^2)

RegModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(RegModel2)

Predictions2 = predict(RegModel2)
sum((statedata$Life.Exp - Predictions2)^2) # or
SSE = sum(RegModel2$residuals^2)
SSE

cor(statedata$Life.Exp, statedata$Income)
cor(statedata$Life.Exp, statedata$Illiteracy)
cor(statedata$Life.Exp, statedata$Area)

# Problem 2
# CART model - regression tree

CARTmodel = rpart(Life.Exp ~ ., data=statedata)
prp(CARTmodel)

PredictionsCART = predict(CARTmodel)
sum((statedata$Life.Exp - PredictionsCART)^2)

CARTmodel2 = rpart(Life.Exp ~ ., data=statedata, minbucket=5)
prp(CARTmodel2)

PredictionsCART2 = predict(CARTmodel2)
sum((statedata$Life.Exp - PredictionsCART2)^2)

CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
PredictionsCART3 = predict(CARTmodel3)
sum((statedata$Life.Exp - PredictionsCART3)^2)
prp(CARTmodel3)

# Problem 3 - cross-validation

# The purpose of cross-validation is to pick the tree that will perform the best
# on a test set.

library(caret)
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
train(Life.Exp ~ ., data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

CARTmodel4 = rpart(Life.Exp ~ ., data=statedata, cp=0.12)
prp(CARTmodel4)

PredictionsCART4 = predict(CARTmodel4)
sum((statedata$Life.Exp - PredictionsCART4)^2)

set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid )
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
prp(CARTmodel5)

PredictionsCART5 = predict(CARTmodel5)
sum((statedata$Life.Exp - PredictionsCART5)^2)
