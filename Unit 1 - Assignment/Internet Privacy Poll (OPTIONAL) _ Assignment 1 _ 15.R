# INTERNET PRIVACY POLL

# PROBLEM 1

poll = read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)

sum(poll$Smartphone, na.rm=TRUE)
sum(is.na(poll$Smartphone)) # OR
table(poll$Smartphone)
summary(poll$Smartphone)

table(poll$Sex, poll$Region)
table(poll$State, poll$Region) # OR
MidwestInterviewees = subset(poll, Region=="Midwest")
table(MidwestInterviewees$State)
SouthInterviewees = subset(poll, Region=="South")
table(SouthInterviewees$State)

# PROBLEM 2

table(poll$Internet.Use, poll$Smartphone)

sum(is.na(poll$Internet.Use))
sum(is.na(poll$Smartphone)) # OR
summary(poll)

limited = subset(poll, poll$Internet.Use==1 | poll$Smartphone==1)
str(limited)
nrow(limited)

# PROBLEM 3

summary(limited)

mean(limited$Info.On.Internet)

table(limited$Info.On.Internet)

table(limited$Worry.About.Info) # or from
summary(limited)

table(limited$Anonymity.Possible) # or from
summary(limited$Anonymity.Possible)

table(limited$Tried.Masking.Identity)
summary(limited$Tried.Masking.Identity)

table(limited$Privacy.Laws.Effective)
summary(limited$Privacy.Laws.Effective)

# PROBLEM 4

hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet) # or
max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1, 2, 3))
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, summary)

tapply(limited$Tried.Masking.Identity, limited$Smartphone, table) $ or
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
