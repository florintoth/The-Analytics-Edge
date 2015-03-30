# PREDICTING LOAN REPAYMENT

# Problem 1

loans = read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(1533+8045)

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util)
                 | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
table(missing$not.fully.paid)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# or load already imputed
loans = read.csv("loans_imputed.csv")

# Problem 2

library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)

LM1 = glm(not.fully.paid ~ ., data = train, family=binomial)
summary(LM1)
-9.317e-03 * -10
exp(-9.317e-03 * -10)

predicted.risk = predict(LM1, newdata=test, type="response")
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk >= 0.5)
# accuracy
(2400+3)/(2400+3+13+457)
# baseline accuracy
(2400+13)/(2400+3+13+457)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 3

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
cor(train$int.rate, train$fico)

pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)
table(test$not.fully.paid, pred.bivariate >= 0.5) 
# or from summary(pred.bivariate) where max<0.5

# Test set AUC 
library(ROCR)
ROCRpred = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 4

10*exp(0.06*3)

# Problem 5

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)*10

# Problem 6

highInterest = subset(test, test$int.rate>=0.15)
summary(highInterest$profit)

sum(highInterest$not.fully.paid)/nrow(highInterest) # or
table(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
highInterest$predicted.risk

selectedLoans = subset(highInterest, highInterest$predicted.risk<=cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
