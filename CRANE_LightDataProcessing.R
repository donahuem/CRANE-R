#CHAIN & CRANE Light data

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
dfA$datetime <- dmy_hms(paste(dfA$date,dfA$time))
dfA <- dfA[,c(4,6)]

dfB <- read.csv2("data/light/20151224_ODY4802_B.CSV", sep=",",skip=1,nrows=2192,col.names=c("na","date","time","par","na"))
dfB$datetime <- dmy_hms(paste(dfB$date,dfB$time))
dfB <- dfB[,c(4,6)]

dfC <- read.csv2("data/light/20151224_ODY2483_C.CSV", sep=",",skip=1,nrows=2192,col.names=c("na","date","time","par","na"))
dfC$datetime <- dmy_hms(paste(dfC$date,dfC$time))
dfC <- dfC[,c(4,6)]

dfpar <- read.csv2("data/light/PAR_HIMB_MFox.csv", sep=",")

