# PREDICTING PAROLE VIOLATORS

# Problem 1

parole = read.csv("parole.csv")
summary(parole)
str(parole)

table(parole$violator)

# Problem 2

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

# Problem 3

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Problem 4

model1 = glm(violator ~ ., data=train, family=binomial)
summary(model1)
exp(1.61)

odds = exp(-4.2411574+0.3869904+0.8867192-0.0001756*50-0.1238867*3+0.0802954*12+0.6837143)
odds
prob = 1/(1+exp(-log(odds)))
prob

# Problem 5

TestPred1 = predict(model1, newdata=test, type="response")
summary(TestPred1)

table(test$violator, TestPred1 >= 0.5)
# sensitivity
12/(11+12)

# specificity
167/(167+12)

# accuracy
(167+12)/(167+12+12+11)

# baseline accuracy
table(test$violator)
179/(179+23)

library(ROCR)
ROCRpred = prediction(TestPred1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
