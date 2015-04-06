# PREDICTING EARNINGS FROM CENSUS DATA

# Problem 1

census = read.csv("census.csv")

library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

# logistic regression
censusglm = glm(over50k ~ . , family="binomial", data = train)
summary(censusglm)

glmpredict = predict(censusglm, newdata=test, type="response")

# accuracy of the model
table(test$over50k, glmpredict >= 0.5)
(9051+1888)/nrow(test)

# baseline accuracy for testing set
table(test$over50k)
9713/nrow(test)

# AUC
library(ROCR)
ROCRpred = prediction(glmpredict, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 2

# classification tree
censustree = rpart(over50k ~ . , data=train, method="class")
prp(censustree)
PredictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, PredictTest)
(9243+1596)/nrow(test)

library(ROCR)
predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]
# Compute the AUC:
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 3

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

# random forest
library(randomForest)
set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)

predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9586+1093)/nrow(test)

vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

varImpPlot(censusrf)

# Problem 4

library(caret)
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

tr.control = trainControl(method = "cv", number = 10)
tr = train(over50k ~ . , data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

censustreecp = rpart(over50k ~ . , data=train, method="class", cp=0.002)
prp(censustreecp)
PredictTestcp = predict(censustreecp, newdata = test, type = "class")
table(test$over50k, PredictTestcp)
(9178+1838)/nrow(test)
