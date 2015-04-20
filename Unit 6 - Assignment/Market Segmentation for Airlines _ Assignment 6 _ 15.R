# MARKET SEGMENTATION FOR AIRLINES

# Problem 1

airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)

library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# Problem 2

distances = dist(airlinesNorm, method = "euclidean")
hierClust = hclust(distances, method="ward.D")
plot(hierClust)

clusterGroups = cutree(hierClust, k = 5)
table(clusterGroups)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean) # or

colMeans(subset(airlines, clusterGroups == 1))
colMeans(subset(airlines, clusterGroups == 2))
colMeans(subset(airlines, clusterGroups == 3))
colMeans(subset(airlines, clusterGroups == 4))
colMeans(subset(airlines, clusterGroups == 5)) # or best

lapply(split(airlines, clusterGroups), colMeans)

# Problem 3

k = 5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
table(KMC$cluster)

summary(KMC$centers)
