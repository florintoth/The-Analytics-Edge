# CLIMATE CHANGE

# PROBLEM 1

climate = read.csv("climate_change.csv")
tail(climate)

climate_train = subset(climate,Year<=2006)
tail(climate_train)

climate_test = subset(climate,Year>2006)
head(climate_test)
tail(climate_test)

TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climate_train)
summary(TempReg)

# PROBLEM 2

cor(climate_train)

# PROBLEM 3

TempReg1 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climate_train)
summary(TempReg1)

# PROBLEM 4

TempRegStep1 = step(TempReg)
summary(TempRegStep1)

# PROBLEM 5

TempPredict = predict(TempRegStep1, newdata=climate_test)
SSE = sum((TempPredict - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2
