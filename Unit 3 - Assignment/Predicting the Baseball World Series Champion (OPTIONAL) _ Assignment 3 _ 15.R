# PREDICTING THE BASEBALL WORLD SERIES CHAMPION (OPTIONAL)

# Problem 1

baseball = read.csv("baseball.csv")
str(baseball)

length(table(baseball$Year))

baseball = subset(baseball, Playoffs == 1)
nrow(baseball)

table(table(baseball$Year))

# Problem 2

PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)

PlayoffTable[c("1990", "2001")]

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$NumCompetitors)

# Problem 3

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

summary(glm(WorldSeries~Year, data=baseball, family="binomial"))

# Problem 4

LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
summary(LogModel)

cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)
Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)
Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)
Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)
Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)
Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)

summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)
summary(Model5)
summary(Model6)
