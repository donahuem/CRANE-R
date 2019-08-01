### light normalization calculation
## created on 8/11/2018
# created by Nyssa Silbiger

#clear the workspace
#rm(list = ls())

# load libraries
library(tidyverse)
library(pROC)

#Read in the light from Tank A
LightA<-read.csv('data/light/20151224_ODY4806_A.CSV')
LightA$Date<-as.Date(LightA$Date, "%d/%m/%Y")
LightA$DateTime<-as.POSIXct(paste(LightA$Date, LightA$Time), format="%Y-%m-%d %H:%M:%S")
LightA[c('ï..Scan.No','CALIBRATED.VALUE')]<-NULL
colnames(LightA)[3]<-'PAR.A'
LightA$PAR.A<-0.0613*LightA$PAR.A^0.9414 # calibrations to LiCor from Hollie Putnam 
#Tank B
LightB<-read.csv('data/light/20151224_ODY4802_B.CSV')
LightB$Date<-as.Date(LightB$Date, "%d/%m/%Y")
LightB$DateTime<-as.POSIXct(paste(LightB$Date, LightB$Time), format="%Y-%m-%d %H:%M:%S")
LightB[c('ï..Scan.No','Calibrated')]<-NULL
colnames(LightB)[3]<-'PAR.B'
LightB$PAR.B<-0.0682*LightB$PAR.B^0.9334 # calibrations to LiCor from Hollie Putnam 

#Tank C
LightC<-read.csv('data/light/20151224_ODY2483_C.CSV')
LightC$Date<-as.Date(LightC$Date, "%d/%m/%Y")
LightC$DateTime<-as.POSIXct(paste(LightC$Date, LightC$Time), format="%Y-%m-%d %H:%M:%S")
LightC[c('ï..Scan.No','CALIBRATED.VALUE')]<-NULL
colnames(LightC)[3]<-'PAR.C'
LightC$PAR.C<-0.0876*LightC$PAR.C^0.9316 # calibrations to LiCor from Hollie Putnam 

## join all the light data into one dataframe by datetime
AllLight<-left_join(LightA, LightB)
AllLight<-left_join(AllLight,LightC)
#re-order the columns
AllLight<-AllLight[c(1,2,4,3,5,6)]

# plot all the light data
#plot(AllLight$DateTime, AllLight$PAR.A, type = 'l', col = 'red')
#lines(AllLight$DateTime, AllLight$PAR.B, type = 'l', col = 'blue')
#lines(AllLight$DateTime, AllLight$PAR.C, type = 'l', col = 'green')

# extract data from experiment 1
Exp1<-which(AllLight$DateTime>=as.POSIXct('2015-12-03 10:00:00', format="%Y-%m-%d %H:%M:%S") &
        AllLight$DateTime <= as.POSIXct('2015-12-04 10:00:00', format="%Y-%m-%d %H:%M:%S"))
LightExp1<-AllLight[Exp1,] # new light df

# extract light from exp 2
Exp2<-which(AllLight$DateTime>=as.POSIXct('2015-12-05 10:00:00', format="%Y-%m-%d %H:%M:%S") &
              AllLight$DateTime <= as.POSIXct('2015-12-06 10:00:00', format="%Y-%m-%d %H:%M:%S"))
LightExp2<-AllLight[Exp2,] # new light df

# plot day 1 ves 2 in each bin
pdf('Scaling/plots/light.pdf', width = 5, height = 5)
par(mfrow=c(2,2))
plot(LightExp1$DateTime, LightExp1$PAR.A, type = 'l', col = 'red', ylab = 'PAR', xlab = 'Time', main = 'Tank A')
lines(LightExp1$DateTime, LightExp2$PAR.A, type = 'l', col = 'blue')
legend('topright',c('Exp 1', 'Exp 2'), lty = 1, col = c('red','blue'), bty='n')
plot(LightExp1$DateTime, LightExp1$PAR.B, type = 'l', col = 'red', ylab = 'PAR', xlab = 'Time', main = 'Tank B')
lines(LightExp1$DateTime, LightExp2$PAR.B, type = 'l', col = 'blue')
plot(LightExp1$DateTime, LightExp1$PAR.C,type = 'l', col = 'red', ylab = 'PAR', xlab = 'Time', main = 'Tank C')
lines(LightExp1$DateTime, LightExp2$PAR.C, type = 'l', col = 'blue')

# cumulative light for each experiment
Exp1.sum<-colSums(LightExp1[,c("PAR.A","PAR.B","PAR.C")])
Exp2.sum<-colSums(LightExp2[,c("PAR.A","PAR.B","PAR.C")])
CumLight<-rbind(Exp1.sum,Exp2.sum)

barplot(CumLight, beside = T, col = c('red','blue'), names.arg = c('A','B','C'), ylab = 'Cumulative PAR')
dev.off()
        