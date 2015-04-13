# SEPARATING SPAM FROM HAM (PART 1)

# Problem 1

emails = read.csv("emails.csv", stringsAsFactors=FALSE)
sum(emails$spam) # or
table(emails$spam)

emails[2,1]
emails$text[1] 
emails$text[1000]

max(nchar(emails$text))
which.min(nchar(emails$text))

# Problem 2

library(tm)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse)) # or
sort(colSums(emailsSparse))

sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))

# Problem 3

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)

set.seed(123)

spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

library(rpart)
library(rpart.plot)
library(randomForest)

spamLog = glm(spam~., data=train, family=binomial)
spamCART = rpart(spam~., data=train, method="class")

set.seed(123)

spamRF = randomForest(spam ~ ., data=train)

predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

summary(spamLog)

prp(spamCART)

table(train$spam, predTrainLog > 0.5)
(3052+954)/nrow(train)

library(ROCR)
ROCRpred = prediction(predTrainLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values) 

# What is the training set accuracy of spamCART, using a threshold of 0.5 for
# predictions? (Remember that if you used the type="class" argument when making
# predictions, you automatically used a threshold of 0.5. If you did not add in
# the type argument to the predict function, the probabilities are in the second
# column of the predict output.)

table(train$spam, predTrainCART >= 0.5)
(2885+894)/nrow(train)

predROCR = prediction(predTrainCART, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

table(train$spam, predTrainRF>0.5)
(3013+914)/nrow(train)

predROCRRF = prediction(predTrainRF, train$spam)
performance(predROCRRF, "auc")@y.values

# Problem 4

predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]

table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test)

predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)

table(test$spam, predTestCART >= 0.5)
(1228+386)/nrow(test)

predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)

table(test$spam, predTestRF>0.5)
(1290+385)/nrow(test)

predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
