# PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT

# Problem 1

stocks = read.csv("StocksCluster.csv")
table(stocks$PositiveDec)/nrow(stocks) # or
mean(stocks$PositiveDec)

cor(stocks)
sort(cor(stocks))

lapply(stocks, mean) # or
summary(stocks)

# Problem 2

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec~., data = stocksTrain, family=binomial)
summary(StocksModel)

predictTrain = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, predictTrain > 0.5) 
(990+3640)/nrow(stocksTrain)

predictTest = predict(StocksModel, type="response", newdata = stocksTest)
table(stocksTest$PositiveDec, predictTest > 0.5) 
# Accuracy
(417+1553)/nrow(stocksTest)
# Baseline accuracy
(344+1553)/nrow(stocksTest)

# Problem 3

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# Problem 4

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

colMeans(stocksTrain1)
colMeans(stocksTrain2)
colMeans(stocksTrain3)

StocksModel1 = glm(PositiveDec~., data = stocksTrain1, family=binomial)
summary(StocksModel1)
StocksModel2 = glm(PositiveDec~., data = stocksTrain2, family=binomial)
summary(StocksModel2)
StocksModel3 = glm(PositiveDec~., data = stocksTrain3, family=binomial)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, type="response", newdata = stocksTest1)
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/nrow(stocksTest1)

PredictTest2 = predict(StocksModel2, type="response", newdata = stocksTest2)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/nrow(stocksTest2)

PredictTest3 = predict(StocksModel3, type="response", newdata = stocksTest3)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1544+1110+353)
