# AN ANALYTICAL DETECTIVE

# PROBLEM 1

mvt = read.csv("mvtWeek1.csv")
str(mvt)

max(mvt$ID)

min(mvt$Beat)

summary(mvt)
Arrested = subset(mvt, Arrest==TRUE)
dim(Arrested) # or summary(mvt)

table(mvt$LocationDescription)

# PROBLEM 2

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

table(mvt$Month)
min(table(mvt$Month))

table(mvt$Weekday)
max(table(mvt$Weekday))

Arrested = subset(mvt, Arrest==TRUE)
table(Arrested$Month)
max(table(Arrested$Month)) # OR
table(mvt$Arrest,mvt$Month)

# PROBLEM 3

hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Arrest,mvt$Year)

# PROBLEM 4

sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
                      LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" |
                      LocationDescription=="DRIVEWAY - RESIDENTIAL") 
nrow(Top5) # OR
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", 
                 "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

table(Top5$LocationDescription)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription,Top5$Arrest)

table(Top5$LocationDescription,Top5$Weekday)
