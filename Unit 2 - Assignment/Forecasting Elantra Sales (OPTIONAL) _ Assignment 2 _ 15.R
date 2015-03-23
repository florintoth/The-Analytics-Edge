# FORECASTING ELANTRA SALES (OPTIONAL)

# PROBLEM 1

Elantra = read.csv("elantra.csv")
ElantraTrain = subset(Elantra, Year <= 2012)
ElantraTest = subset(Elantra, Year > 2012)

# PROBLEM 2

ElantraLM1 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)
summary(ElantraLM1)

# PROBLEM 3

ElantraLM2 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)
summary(ElantraLM2)

# PROBLEM 4

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
        
ElantraLM3 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(ElantraLM3)

# PROBLEM 5

cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# PROBLEM 6

ElantraLM4 = lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(ElantraLM4)

PredictTest = predict(ElantraLM4, newdata=ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales)^2)
SSE

baseline = mean(ElantraTrain$ElantraSales)
baseline

SST = sum((baseline - ElantraTest$ElantraSales)^2)
Rsquared = 1-SSE/SST
Rsquared

max(abs(PredictTest - ElantraTest$ElantraSales))

ElantraTest$Month[which.max(abs(PredictTest - ElantraTest$ElantraSales))]
ElantraTest$Year[which.max(abs(PredictTest - ElantraTest$ElantraSales))]
