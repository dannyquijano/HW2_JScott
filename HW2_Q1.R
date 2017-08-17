abia = read.csv('http://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv', header=TRUE)
head(abia)

max(abia$DepDelay, na.rm = TRUE)
min(abia$DepDelay, na.rm = TRUE)
hist(abia[abia$DepDelay > 15,]$DepDelay, breaks = c(seq(15,900, 15)))

abia[abia$Origin == 'AUS',]

plot(abia[abia$DepDelay > 15,]$CRSDepTime,abia[abia$DepDelay > 15,]$DepDelay)
plot(abia[abia$DepDelay > 15,]$CRSDepTime,abia[abia$DepDelay > 15,]$DepTime)

par(mfrow=c(1,2))
#plot of average delay time by scheduled departure hour
plot(tapply(abia$DepDelay, cut(abia$CRSDepTime, seq(0, 2400, by=100)), mean, na.rm = TRUE), ylim=range(0,60))
#plot of average delay time by scheduled departure hour (excluding ontime)
plot(tapply(abia[abia$DepDelay > 0,]$DepDelay, cut(abia[abia$DepDelay > 0,]$CRSDepTime, seq(0, 2400, by=100)), mean, na.rm = TRUE), ylim=range(0,60))

#plot of average delay time by month
plot(tapply(abia$DepDelay, cut(abia$Month, seq(0, 12, by=1)), mean, na.rm = TRUE))
#plot of average delay time by month(excluding ontime)
plot(tapply(abia[abia$DepDelay > 0,]$DepDelay, cut(abia[abia$DepDelay > 0,]$Month, seq(0, 12, by=1)), mean, na.rm = TRUE))


