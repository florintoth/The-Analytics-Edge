# ELECTION FORECASTING REVISITED

statesMap = map_data("state")

# Problem 1

str(statesMap)

table(statesMap$group) # or
length(table(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "white", color = "black")

ggplot(statesMap, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "pink")

# Problem 2

polling = read.csv("PollingImputed.csv")
str(polling)

Train = subset(polling, Year < 2012)

Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

table(TestPredictionBinary)
mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
        geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
        geom_polygon(color = "black") +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black") +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Problem 3

str(predictionMap)
predictionMap[predictionMap$region == "florida",]

# Problem 4

?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", linetype=3) +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", size=3) +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", alpha= 0.3) +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
