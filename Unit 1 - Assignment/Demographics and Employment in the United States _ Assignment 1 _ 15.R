# DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES

# PROBLEM 1

CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)

table(CPS$Industry) 

sort(table(CPS$State)) 

table(CPS$Citizenship)

table(CPS$Race,CPS$Hispanic)

# PROBLEM 2

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

table(CPS$Region, is.na(CPS$MetroAreaCode))

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# PROBLEM 3

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

str(MetroAreaMap)
nrow(MetroAreaMap)
str(CountryMap)
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# The first two arguments determine the data frames to be merged (they are
# called "x" and "y", respectively, in the subsequent parameters to the merge
# function). by.x="MetroAreaCode" means we're matching on the MetroAreaCode
# variable from the "x" data frame (CPS), while by.y="Code" means we're matching
# on the Code variable from the "y" data frame (MetroAreaMap). Finally,
# all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even
# if some of the rows' MetroAreaCode doesn't match any codes in MetroAreaMap
# (for those familiar with database terminology, this parameter makes the
# operation a left outer join instead of an inner join).

summary(CPS)
str(CPS)

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

# PROBLEM 4

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
sort(table(CPS$Country))

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
