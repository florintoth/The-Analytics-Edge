# VISUALIZING TEXT DATA USING WORD CLOUDS

# Problem 1

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

str(allTweets) # or
ncol(allTweets)

# Problem 2

install.packages("wordcloud")
library(wordcloud)
?wordcloud

windows()
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

windows()
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

# Problem 3

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# Problem 4

library(RColorBrewer)
?brewer.pal
display.brewer.all()
windows()
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), brewer.pal(9, "Blues")[c(1, 2, 3, 4)])
brewer.pal(9, "Blues")[-1:-4]
brewer.pal(9, "Blues")[5:9]
