# LETTER RECOGNITION

# Problem 1

letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(test$isB)/nrow(test)

# classification tree
CARTb = rpart(isB ~ . - letter, data=train, method="class")
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$isB, PredictTest)
(1118+340)/nrow(test)

# random forest
set.seed(1000)
LetterForest = randomForest(isB ~ . - letter, data=train)
PredictForest = predict(LetterForest, newdata = test)
table(test$isB, PredictForest)
(1165+374)/nrow(test)

# Problem 2

letters$letter = as.factor( letters$letter )

library(caTools)
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(test$letter)/nrow(test)

CARTb1 = rpart(letter ~ . - isB, data=train, method="class")
PredictTest1 = predict(CARTb1, newdata = test, type = "class")
table(test$letter, PredictTest1)
(348+318+363+340)/nrow(test)

set.seed(1000)
LetterForest1 = randomForest(letter ~ . - isB, data=train)
PredictForest1 = predict(LetterForest1, newdata = test)
table(test$letter, PredictForest1)
(390+380+393+364)/nrow(test)
