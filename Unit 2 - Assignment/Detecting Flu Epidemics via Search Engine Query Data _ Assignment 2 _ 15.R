# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 

# PROBLEM 1

FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)

ILImax = subset(FluTrain, FluTrain$ILI==max(FluTrain$ILI))
ILImax # or
which.max(FluTrain$ILI)
FluTrain$Week[303]

Queriesmax = subset(FluTrain, FluTrain$Queries==max(FluTrain$Queries))
Queriesmax # or
which.max(FluTrain$Queries)
FluTrain$Week[303]

# PROBLEM 2

hist(FluTrain$ILI)
plot(log(FluTrain$ILI)~FluTrain$Queries)

FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
rsquared = Correlation^2
rsquared

# PROBLEM 3

FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

(FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),"ILI"]-
        PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")])/
        FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),"ILI"]

SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE # or
sqrt(mean((PredTest1-FluTest$ILI)^2))

# PROBLEM 4

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

# In these commands, the value of -2 passed to lag means to return 2
# observations before the current one; a positive value would have returned
# future observations. The parameter na.pad=TRUE means to add missing values for
# the first two weeks of our dataset, where we can't compute the data from 2
# weeks earlier.

summary(FluTrain)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

# PROBLEM 5

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
sqrt(mean((PredTest2-FluTest$ILI)^2))

summary(FluTrend1)
summary(FluTrend2)
