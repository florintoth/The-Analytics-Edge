# POPULARITY OF MUSIC RECORDS

songs = read.csv("songs.csv")
str(songs)

# Problem 1

table(songs$year)

table(songs$artistname) # or
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)
nrow(MichaelJackson)

MichaelJackson1 = subset(MichaelJackson, songtitle == "Beat It" | songtitle == "You Rock My World" | songtitle == "Billie Jean" | songtitle == "You Are Not Alone")
MichaelJackson1
# or from
MichaelJackson[c("songtitle", "Top10")]

sort(unique(songs$timesignature))
table(songs$timesignature)
which.max(songs$tempo)
songs[which.max(songs$tempo), ]

# Problem 2

SongsTrain = subset(songs, year<=2009)
SongsTest = subset(songs, year==2010)

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# Problem 3

cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
# "-" works for removing numeric variables
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Problem 4

TestHitPrediction = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, TestHitPrediction >= 0.45)
# Accuracy
(309+19)/(309+5+40+19)
# Baseline Accuracy
(309+5)/(309+5+40+19) # or
table(SongsTest$Top10)
314/(314+59)

table(SongsTest$Top10, TestHitPrediction >= 0.45)
# Sensitivity
19/(40+19)
# Specificity
309/(309+5)
