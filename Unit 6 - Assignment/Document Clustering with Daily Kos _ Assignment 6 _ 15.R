# DOCUMENT CLUSTERING WITH DAILY KOS

# Problem 1

dailykos = read.csv("dailykos.csv")

kosDist = dist(dailykos, method="euclidean")

kosHierClust = hclust(kosDist, method="ward.D")

plot(kosHierClust)

hierGroups = cutree(kosHierClust, k = 7)
str(hierGroups)
table(hierGroups) # or

HierCluster1 = subset(dailykos, hierGroups == 1)

HierCluster2 = subset(dailykos, hierGroups == 2)

HierCluster3 = subset(dailykos, hierGroups == 3)

HierCluster4 = subset(dailykos, hierGroups == 4)

HierCluster5 = subset(dailykos, hierGroups == 5)

HierCluster6 = subset(dailykos, hierGroups == 6)

HierCluster7 = subset(dailykos, hierGroups == 7) # or

HierCluster = split(dailykos, hierGroups)

HierCluster[[1]]

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

# Problem 2

set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=7)
str(KmeansCluster)

table(KmeansCluster$cluster) # or

KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)

KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)

KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)

KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)

KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)

KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)

KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7) # or

KmeansCluster = split(dailykos, KmeansCluster$cluster)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

table(hierGroups, KmeansCluster$cluster)
