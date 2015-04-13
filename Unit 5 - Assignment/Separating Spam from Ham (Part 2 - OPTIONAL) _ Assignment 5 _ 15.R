# SEPARATING SPAM FROM HAM (PART 2 - OPTIONAL)

# Problem 5

# Problem 6

wordCount = rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)

train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)
spam2CART = rpart(spam~., data=train2, method="class")
set.seed(123)
spam2RF = randomForest(spam~., data=train2)

prp(spam2CART)

predTest2CART = predict(spam2CART, newdata=test2)[,2]
predTest2RF = predict(spam2RF, newdata=test2, type="prob")[,2]

table(test2$spam, predTest2CART > 0.5)
(1214+384)/nrow(test2)

predictionTest2CART = prediction(predTest2CART, test2$spam)
as.numeric(performance(predictionTest2CART, "auc")@y.values)

table(test2$spam, predTest2RF > 0.5)
(1298+382)/nrow(test2)

predictionTest2RF = prediction(predTest2RF, test2$spam)
as.numeric(performance(predictionTest2RF, "auc")@y.values)
