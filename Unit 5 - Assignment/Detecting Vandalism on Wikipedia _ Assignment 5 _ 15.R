# DETECTING VANDALISM ON WIKIPEDIA

# Problem 1

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
library(SnowballC)

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
strwrap(corpusAdded[[1]])

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]
strwrap(corpusRemoved[[1]])

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
wordsRemoved
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, split==TRUE)
wikiTest = subset(wikiWords, split==FALSE)

# Baseline accuracy 
table(test$Vandal)
618/(618+545)

wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
prp(wikiCART)
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)

# Compute accuracy
(618+12)/nrow(wikiTest)

# Problem 2

grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)

# Compute new accuracy
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiCART3)
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)

# Compute latest accuracy
(514+248)/nrow(wikiTest3)

# Problem 3
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
prp(wikiCART4)
testPredictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, testPredictCART4)

# Compute latest accuracy
(595+241)/nrow(wikiTest4)
