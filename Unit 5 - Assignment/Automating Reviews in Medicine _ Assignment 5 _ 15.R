# AUTOMATING REVIEWS IN MEDICINE

# Problem 1

trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials) 
str(trials)
max(nchar(trials$abstract)) # or
summary(nchar(trials$abstract))

table(nchar(trials$abstract) == 0) # or
sum(nchar(trials$abstract) == 0)

trials$title[which.min(nchar(trials$title))]

# Problem 2

library(tm)

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)
str(dtmAbstract)

which.max(colSums(dtmAbstract))

# Problem 3

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

# Baseline accuracy
table(train$trial)
730/nrow(train)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

pred = predict(trialCART)
pred.prob = pred[,2]
summary(pred.prob)

table(train$trial, pred.prob >= 0.5)
# training set accuracy
(631+441)/nrow(train)
# training set sensitivity
441/(441+131)
# training set specificity
631/(631+99)

# Problem 4

predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)

(261+162)/nrow(test)

library(ROCR)

predROCR = prediction(predTest, test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values

# Problem 5
