# READING TEST SCORES

# PROBLEM 1

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)
nrow(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)
nrow(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTest)

# PROBLEM 2

str(climate)

# PROBLEM 3

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore~., data=pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE # or
sqrt(mean(lmScore$residuals^2))

# PROBLEM 3

predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

baseline=mean(pisaTrain$readingScore)
baseline

SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST

R2 = 1 - SSE/SST
R2
