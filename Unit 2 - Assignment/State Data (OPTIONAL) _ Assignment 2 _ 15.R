# STATE DATA (OPTIONAL)

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  
                  state.division, state.name, state.region)
str(statedata)

# PROBLEM 1

plot(statedata$x, statedata$y) 

attach(statedata)

tapply(HS.Grad, state.region, mean)

boxplot(Murder~state.region)

NortheastData = subset(statedata, state.region == "Northeast")
summary(NortheastData)
murderoutlier = subset(NortheastData, Murder==10.9)
murderoutlier

detach(statedata)

# PROBLEM 2

Model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
                    Frost + Area, data = statedata)
summary(Model1)

plot(statedata$Income, statedata$Life.Exp)

# PROBLEM 3

Model2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(Model2)

sort(predict(Model2))
statedata$state.name[which.min(statedata$Life.Exp)]

sort(predict(Model2))
statedata$state.name[which.max(statedata$Life.Exp)]

sort(abs(Model2$residuals)) # or
sort(abs(statedata$Life.Exp - predict(Model2)))

sort(abs(Model2$residuals)) # or
sort(abs(statedata$Life.Exp - predict(Model2)))
