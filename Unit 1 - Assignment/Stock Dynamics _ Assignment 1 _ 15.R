# STOCK DYNAMICS

# PROBLEM 1

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

str(IBM)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
summary(ProcterGamble)

sd(ProcterGamble$StockPrice)

# PROBLEM 2

plot(CocaCola, type = "l")

lines(ProcterGamble$Date, ProcterGamble$StockPrice)

plot(CocaCola, type = "l", col="red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty = 2)

abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1983-01-01")), lwd=2)

# PROBLEM 3

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

colors()

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
lines(Boeing$Date, Boeing$StockPrice, col="green")
lines(GE$Date, GE$StockPrice, col="black")
lines(IBM$Date, IBM$StockPrice, col="orange")

# (If you prefer to change the type of the line instead of the color, here are
# some options for changing the line type: lty=2 will make the line dashed,
# lty=3 will make the line dotted, lty=4 will make the line alternate between
# dashes and dots, and lty=5 will make the line long-dashed.)

abline(v=as.Date(c("2000-03-01")), lwd=1)
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

# PROBLEM 4

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

tapply(GE$StockPrice, months(GE$Date), mean)
max(tapply(GE$StockPrice, months(GE$Date), mean))
mean(GE$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
mean(CocaCola$StockPrice)

tapply(Boeing$StockPrice, months(Boeing$Date), mean)

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
