#CHAIN & CRANE Light data

##Feb 2, 2017
#     Odyssey data normalized to logger B is called dfAnorm, dfBnorm, dfCnorm
#     Normalization based on common garden from 12/19 - 12/24
#     Normalized Odyssey data restricted to experimental period of 12/3-12/6 is called dfAexp,dfBexp,dfCexp

# LiCor collected data from CHAIN 10/29/15 - 11/18/15, logging every 5 min 6am to 7pm
# LiCor deployments
# df1: 10.29.15 - 11.9.15
# df2: 11.9.2015 - 11.18.2015
# lost data from 11.18 - 12.6
# 12.6.2015 - 12.7.2015 (used LiCor as handheld for spot sampling in CRANE)
# df3: 12.6.2015 - 12.10.2015 (coincident with Odyssey sensors: LiCor in CHAIN?, needs calibration constant corrected)
# df4:12.10.2015 - 12.18.2015 (coincident with Odyssey sensors: LiCor in CHAIN)
# 12.18.2015 - 12.26.2015 (cross calibration of LiCor w Odysseys in CRANE Tank 1, currently missing, looks like LiCor ran out of batteries on 12.18 )

# Odyssey loggers deployed on 12/1/15 at ~4pm logging every 15 min
# dfA: 4806 was in tank closest to maturation facility
# dfB: 4802 was in middle tank
# dfC: 2483 was in tank furthest from maturation facility
# On 12/18/2015, placed 3 Odyssey loggers & LiCor together in tank closest to maturation facility
# All loggers pulled at 9am on 12/24/15

#Constant correction for 12.6 - 12.10 deployment
# Constant set at -192.84, should have been set at -190.84
# Starting December 10, set to -190.84 in water 
# Other LiCor settings:  Avg 5 seconds, log LRI, calc = mean, minmax = no, TCoef = 0.0036, log from 0600-1900 hr

df1 <- read.csv2("data/light/10.29-11.09.2015.txt",header=FALSE,sep="\t",colClasses=c("integer","POSIXct","character"),col.names=c("na","datetime","par"))
df1 <- df1[!is.na(as.numeric(df1[,3])),2:3]

df2 <- read.csv2("data/light/11.09-11.18.2015.txt",header=FALSE,sep="\t",col.names=c("na","datetime","par"))
df2$par <- as.numeric(as.character(df2$par))
df2 <- df2[!is.na(df2[,3]),2:3]
df2$datetime <- strptime(df2$datetime,tz="",format="%Y-%m-%d %H:%M:%S")


df3 <- read.csv2("data/light/12.06-12.10.2015.txt",sep="\t",colClasses=c("integer","POSIXct","character"),col.names=c("na","datetime","par"))
df3$par <- as.numeric(as.character(df3$par))
df3 <- df3[!is.na(df3[,3]),2:3]

df4 <- read.csv2("data/light/12.10-12.18.2015.txt",sep="\t",col.names=c("na","datetime","par"))
df4$par <- as.numeric(as.character(df4$par))
df4 <- df4[!is.na(df4[,3]),2:3]

dfA <- read.csv2("data/light/20151224_ODY4806_A.CSV", sep=",",skip=1,nrows=2192,col.names=c("na","date","time","par","na"))
dfA$datetime <- paste(dfA$date,dfA$time,sep=" ")
dfA$datetime <- strptime(dfA$datetime,tz="",format="%d/%m/%Y %H:%M:%S")
dfA <- dfA[dfA$datetime ,c(4,6)]

dfB <- read.csv2("data/light/20151224_ODY4802_B.CSV", sep=",",skip=1,nrows=2192,col.names=c("na","date","time","par","na"))
dfB$datetime <- paste(dfB$date,dfB$time,sep=" ")
dfB$datetime <- strptime(dfB$datetime,tz="",format="%d/%m/%Y %H:%M:%S")
dfB <- dfB[,c(4,6)]

dfC <- read.csv2("data/light/20151224_ODY2483_C.CSV", sep=",",skip=1,nrows=2192,col.names=c("na","date","time","par","na"))
dfC$datetime <- paste(dfC$date,dfC$time,sep=" ")
dfC$datetime <- strptime(dfC$datetime,tz="",format="%d/%m/%Y %H:%M:%S")
dfC <- dfC[,c(4,6)]

dfpar <- read.csv2("data/light/PAR_HIMB_MFox.csv", sep=",")

#For now, ignore LiCor data, and simply adjust Odyssey loggers to one another

#normalization period: daylight hours Dec 19-24
dfAn <- subset(dfA,(format(dfA$datetime,'%m/%d') > '12/18' & format(dfA$datetime,'%H:%M') >= '07:00' & format(dfA$datetime,'%H:%M') <= '18:00'))
dfBn <- subset(dfB,(format(dfB$datetime,'%m-%d')> '12-18' & format(dfB$datetime,'%H:%M') >= '07:00' & format(dfB$datetime,'%H:%M') <= '18:00'))
dfCn <- subset(dfC,(format(dfC$datetime,'%m-%d')> '12-18' & format(dfC$datetime,'%H:%M') >= '07:00' & format(dfC$datetime,'%H:%M') <= '18:00'))

AB <- lm(dfAn$par ~ dfBn$par)
AC <- lm(dfAn$par ~ dfCn$par)
BC <- lm(dfBn$par ~ dfCn$par)

plot(dfAn$par ~ dfBn$par)
abline(AB$coefficients[1],AB$coefficients[2])
plot(dfAn$par ~ dfCn$par)
abline(AC$coefficients[1],AC$coefficients[2])
plot(dfBn$par ~ dfCn$par)
abline(BC$coefficients[1],BC$coefficients[2])

plot(dfAn$par ~ dfBn$par)
points(dfAn$par ~ dfCn$par,col="blue")
abline(0,1)

# A and B are similar; C is further off
# B is in the middle, so normalize A and C to B

CB <- lm(dfCn$par ~ dfBn$par)

#plot normalization to check
plot(dfBn$par,(dfAn$par - AB$coefficients[1])/AB$coefficients[2])
points(dfBn$par,(dfCn$par - CB$coefficients[1])/CB$coefficients[2],col="blue")

#subset time series to experimental period: Dec 3-6
dfAexp <- subset(dfA,(format(dfA$datetime,'%m/%d') >= '12/03' 
                         & format(dfA$datetime,'%m/%d') <= '12/06'
                         & format(dfA$datetime,'%H:%M') >= '07:00' 
                         & format(dfA$datetime,'%H:%M') <= '18:00'))
dfBexp <- subset(dfB,(format(dfB$datetime,'%m/%d') >= '12/03' 
                      & format(dfB$datetime,'%m/%d') <= '12/06'
                      & format(dfB$datetime,'%H:%M') >= '07:00' 
                      & format(dfB$datetime,'%H:%M') <= '18:00'))
dfCexp <- subset(dfC,(format(dfC$datetime,'%m/%d') >= '12/03' 
                      & format(dfC$datetime,'%m/%d') <= '12/06'
                      & format(dfC$datetime,'%H:%M') >= '07:00' 
                      & format(dfC$datetime,'%H:%M') <= '18:00'))

#apply normalization
dfAexp$par <- (dfAexp$par - AB$coefficients[1])/AB$coefficients[2]
dfCexp$par <- (dfCexp$par - CB$coefficients[1])/CB$coefficients[2]

#plot experimental period with normalized data
plot(dfAexp$datetime,dfAexp$par)
points(dfBexp$datetime,dfBexp$par,col="blue")
points(dfCexp$datetime,dfCexp$par,col="red")

#apply normalization to full timeseries
dfAnorm <- dfA
dfBnorm <- dfB
dfCnorm <- dfC
dfAnorm$par <- (dfAnorm$par - AB$coefficients[1])/AB$coefficients[2]
dfCnorm$par <- (dfCnorm$par - CB$coefficients[1])/CB$coefficients[2]

