# UNDERSTANDING WHY PEOPLE VOTE

# Problem 1

gerber = read.csv("gerber.csv")
str(gerber)
table(gerber$voting)/nrow(gerber)

table(gerber$voting, gerber$civicduty)/nrow(gerber)
table(gerber$voting, gerber$hawthorne)/nrow(gerber)
table(gerber$voting, gerber$self)/nrow(gerber)
table(gerber$voting, gerber$neighbors)/nrow(gerber)
# or

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(LogModel)

predictLog = predict(LogModel, type="response")
table(gerber$voting, predictLog > 0.3)
(134513+51966)/(134513+100875+56730+51966)
(134513+51966)/nrow(gerber)

table(gerber$voting, predictLog > 0.5)
(235388+0)/nrow(gerber)

# baseline accuracy
table(gerber$voting)/nrow(gerber)
0.6841004

library(ROCR)
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Problem 2

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)


CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# Problem 3

CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
prp(CARTsex, digits=6)

LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
abs(0.2908065-0.290456)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)
