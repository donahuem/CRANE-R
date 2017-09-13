####################################################
### CRANE Data Analysis for Biogeochemical Responses####
### Created by Nyssa Silbiger                   ###
### Created on 1/07/2016                       ###
### Edited on ---                               ###
### Edited by ---                               ###
###################################################

# Clear workspace --------------------------------
rm(list=ls())

# Add Libraries ---------------------------------
library('plyr')
library('oce')
library('lme4')
library('lmerTest')
library('reshape2')
# Functions-----------------------------------------
#easy errorbar barplots
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# Load Data-----------------------------------------
#Exp1
Exp1Data<-read.csv('Data/Exp1Sensor.csv') [,1:12] #read in 1st 12 columns only

#Exp2
Exp2Data<-read.csv('Data/Exp2Sensor.csv') [,1:12] #read in 1st 12 columns only

## Data Analysis-------------------------------------

#calculate salinity from conductivity and temp
Exp1Data$Salinity<-swSCTp(Exp1Data$Conductivity, Exp1Data$Temp, pressure=rep(0, each=nrow(Exp1Data)),
       conductivityUnit="mS/cm")

Exp2Data$Salinity<-swSCTp(Exp2Data$Conductivity, Exp2Data$Temp, pressure=rep(0, each=nrow(Exp2Data)),
                          conductivityUnit="mS/cm")

# Subtract pH in each aquarium from the header to see how much the substrate influenced pH at each nutrient level
time<-unique(Exp1Data$SampleTime) #unique sampling time points
Nutlevels <- unique(Exp1Data$NutLevel)#unique nut levels
Dates<-unique(Exp1Data$Date) # unique dates because 10am was done twice each sampling period
Substrates<-unique(Exp1Data$Substrate)

#A postive delta pH means that the substrate elevated the pH and a negative meant that it dropped the pH
#Experiment 1
for (i in 1:length(time)){
  for (j in 1:length(Nutlevels)){
    for (k in 1:length(Dates)){
Exp1Data$DeltapH[Exp1Data$SampleTime == time[i] & Exp1Data$NutLevel == Nutlevels[j]
                  & Exp1Data$Date == Dates[k]]<- -1*(Exp1Data$pH[Exp1Data$BlkTank=='Header' & Exp1Data$SampleTime == time[i] 
                              & Exp1Data$NutLevel == Nutlevels[j] & Exp1Data$Date == Dates[k]]-
                    Exp1Data$pH[ Exp1Data$SampleTime == time[i] & Exp1Data$NutLevel == Nutlevels[j]
                    & Exp1Data$Date == Dates[k]])
    }
  }
}

#Experiment 2
for (i in 1:length(time)){
  for (j in 1:length(Nutlevels)){
    for (k in 1:length(Dates)){
      Exp2Data$DeltapH[Exp2Data$SampleTime == time[i] & Exp2Data$NutLevel == Nutlevels[j]
                       & Exp2Data$Date == Dates[k]]<--1*(Exp2Data$pH[Exp1Data$BlkTank=='Header' & Exp2Data$SampleTime == time[i] 
                                                                 & Exp2Data$NutLevel == Nutlevels[j] & Exp2Data$Date == Dates[k]]-
        Exp2Data$pH[ Exp2Data$SampleTime == time[i] & Exp2Data$NutLevel == Nutlevels[j]
                     & Exp2Data$Date == Dates[k]])
    }
  }
}

## See if there are differences across substrates, nutrient levels nested within time (because whether its higher
#or lower than the header depends on whether it is during the day or night)
#using black tank (essentially light)
#as a random variable

#first move header data into a new dataframe
Exp1Data.Headers<- Exp1Data[Exp1Data$BlkTank=='Header',]
Exp1Data<-Exp1Data[Exp1Data$BlkTank!='Header',] #remove the header data

Exp2Data.Headers<- Exp2Data[Exp2Data$BlkTank=='Header',]
Exp2Data<-Exp2Data[Exp2Data$BlkTank!='Header',] #remove the header data

#merge date and time column
Exp1Data$DateTime<-as.POSIXct(paste(Exp1Data$Date, Exp1Data$SampleTime), format="%m/%d/%Y %H:%M")
Exp2Data$DateTime<-as.POSIXct(paste(Exp2Data$Date, Exp2Data$SampleTime), format="%m/%d/%Y %H:%M")

#add a column for "Day" or "Night"
Exp1Data$DayNight<-ifelse(Exp1Data$SampleTime==time[1]|Exp1Data$SampleTime==time[2]|Exp1Data$SampleTime==time[3], 'Day', 'Night')
Exp2Data$DayNight<-ifelse(Exp2Data$SampleTime==time[1]|Exp2Data$SampleTime==time[2]|Exp2Data$SampleTime==time[3], 'Day', 'Night')


#Experiment 1
#substrate, nutrients, substrate * nutrients with black tanke and sample time as random effects
exp1lm<-lmer(DeltapH~ Substrate*NutLevel + (BlkTank || SampleTime), data=Exp1Data)
anova(exp1lm)

#sample time as a fixed effect instead of a random effect
anova(lmer(DeltapH~ Substrate*NutLevel*SampleTime + (1 |BlkTank), data=Exp1Data))

#Experiment 2
exp2lm<-lmer(DeltapH~ NutLevel + (BlkTank || SampleTime), data=Exp2Data)
anova(exp2lm)

#sample time as fixed effect
anova(lmer(DeltapH~ NutLevel*SampleTime + (1|BlkTank), data=Exp2Data))

Exp1Summary <- ddply(Exp1Data, c("Substrate", "NutLevel"), summarize,
                      N    = length(DeltapH),
                      mean = mean(abs(DeltapH), na.rm = T),
                      sd   = sd(DeltapH, na.rm = T),
                      se   = sd / sqrt(N)
)

#reshape the data for easy plotting
DeltapHMeans<-dcast(Exp1Summary, Substrate~NutLevel, value.var="mean")
row.names(DeltapHMeans)<- DeltapHMeans$Substrate
DeltapHMeans<- DeltapHMeans[,c(2,4,3)]
DeltapHSE<-dcast(Exp1Summary, Substrate~NutLevel, value.var="se")
row.names(DeltapHSE)<- DeltapHSE$Substrate
DeltapHSE<- DeltapHSE[,c(2,4,3)]

#plot of absolute mean pH for each substrate
exp1plot<-barplot(as.matrix(t(DeltapHMeans)), beside = T, ylim = c(-0, 0.2), col = c('red','green','blue'),
                  ylab = 'Abs (delta pH)')
arrows(exp1plot, as.matrix(t(DeltapHMeans))+as.matrix(t(DeltapHSE)), exp1plot, as.matrix(t(DeltapHMeans))-as.matrix(t(DeltapHSE)), angle=90, code=3, length = 0.15)
legend('topright', legend  = c('Ambient','Med','High'), pch = 15, col = c('red','green','blue'), bty='n')

#separate it by day and night

Exp1Summary.DayNight <- ddply(Exp1Data, c("Substrate", "NutLevel", "DayNight"), summarise,
                          N    = length(DeltapH),
                          mean = mean(DeltapH, na.rm = T),
                          sd   = sd(DeltapH, na.rm = T),
                          se   = sd / sqrt(N)
)

#reshape the data for easy plotting
#Day
DeltapHMeansDay<-dcast(Exp1Summary.DayNight[Exp1Summary.DayNight$DayNight=='Day',], Substrate~NutLevel, value.var="mean")
row.names(DeltapHMeansDay)<- DeltapHMeansDay$Substrate
DeltapHMeansDay<- DeltapHMeansDay[,c(2,4,3)]
DeltapHSEDay<-dcast(Exp1Summary.DayNight[Exp1Summary.DayNight$DayNight=='Day',], Substrate~NutLevel, value.var="se")
row.names(DeltapHSEDay)<- DeltapHSEDay$Substrate
DeltapHSEDay<- DeltapHSEDay[,c(2,4,3)]

#Night
DeltapHMeansNight<-dcast(Exp1Summary.DayNight[Exp1Summary.DayNight$DayNight=='Night',], Substrate~NutLevel, value.var="mean")
row.names(DeltapHMeansNight)<- DeltapHMeansNight$Substrate
DeltapHMeansNight<- DeltapHMeansNight[,c(2,4,3)]
DeltapHSENight<-dcast(Exp1Summary.DayNight[Exp1Summary.DayNight$DayNight=='Night',], Substrate~NutLevel, value.var="se")
row.names(DeltapHSENight)<- DeltapHSENight$Substrate
DeltapHSENight<- DeltapHSENight[,c(2,4,3)]


#plot averages by day and night
par(mfrow=c(2,1))
exp1plot<-barplot(as.matrix(t(DeltapHMeansDay)), beside = T, ylim = c(0, 0.2), col = c('red','green','blue'),
                  ylab = 'mean delta pH', main = 'Day')
arrows(exp1plot, as.matrix(t(DeltapHMeansDay))+as.matrix(t(DeltapHSEDay)), exp1plot, as.matrix(t(DeltapHMeansDay))-as.matrix(t(DeltapHSEDay)), angle=90, code=3, length = 0.15)
legend('topright', legend  = c('Ambient','Med','High'), pch = 15, col = c('red','green','blue'), bty='n')


exp1plot<-barplot(as.matrix(t(DeltapHMeansNight)), beside = T, ylim = c(-0.2, 0), col = c('red','green','blue'),
                  ylab = 'mean delta pH', main = 'Night')
arrows(exp1plot, as.matrix(t(DeltapHMeansNight))+as.matrix(t(DeltapHSENight)), exp1plot, as.matrix(t(DeltapHMeansNight))-as.matrix(t(DeltapHSENight)), angle=90, code=3, length = 0.15)

##-Same for experiment 2
#separate it by day and night

Exp2Summary.DayNight <- ddply(Exp1Data, c("NutLevel", "DayNight"), summarise,
                              N    = length(DeltapH),
                              mean = mean(DeltapH, na.rm = T),
                              sd   = sd(DeltapH, na.rm = T),
                              se   = sd / sqrt(N)
)

#reshape the data for easy plotting
#Day/night
DeltapHMeansDay2<-dcast(Exp2Summary.DayNight, NutLevel~DayNight, value.var="mean")
row.names(DeltapHMeansDay2)<- DeltapHMeansDay2$NutLevel
DeltapHMeansDay2<- DeltapHMeansDay2[,-1]
DeltapHMeansDay2<- DeltapHMeansDay2[c(1,3,2),]
DeltapHSEDay2<-dcast(Exp2Summary.DayNight, NutLevel~DayNight, value.var="se")
row.names(DeltapHSEDay2)<- DeltapHSEDay2$NutLevel
DeltapHSEDay2<- DeltapHSEDay2[,-1]
DeltapHSEDay2<- DeltapHSEDay2[c(1,3,2),]

#plot averages by day and night
par(mfrow=c(1,1))
exp1plot<-barplot(as.matrix((DeltapHMeansDay2)), beside = T, ylim = c(-0.15, 0.15), col = c('red','green','blue'),
                  ylab = 'mean delta pH', main = 'Experiment 2')
arrows(exp1plot, as.matrix((DeltapHMeansDay2))+as.matrix((DeltapHSEDay2)), exp1plot, as.matrix((DeltapHMeansDay2))-as.matrix((DeltapHSEDay2)), angle=90, code=3, length = 0.15)
legend('topright', legend  = c('Ambient','Med','High'), pch = 15, col = c('red','green','blue'), bty='n')



##--------
#Averages by time and make subplots for each substrate

Exp1Summary.Time <- ddply(Exp1Data, c("Substrate", "NutLevel", "DateTime"), summarize,
                     N    = length(DeltapH),
                     mean = mean(DeltapH, na.rm = T),
                     sd   = sd(DeltapH, na.rm = T),
                     se   = sd / sqrt(N)
)

par(mfrow=c(2,2))
for (i in 1:4){
plot(NA, ylim=c(-0.3,0.5), xaxt="n", xlab="Time", ylab="Delta pH", main = Substrates[i])
 
abline(h=0)
par(new = TRUE)
cols <- c('red','green','blue')
#
  for (j in 1:length(Nutlevels)){
    par(new = TRUE)
     
plot(as.numeric(Exp1Summary.Time$DateTime [Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]]),
     Exp1Summary.Time$mean[Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]], col = cols[j],
     pch=19, ylim=c(-0.3,0.5), type="b", xaxt='n', ylab='', xlab='')

arrows(unique(Exp1Summary.Time$DateTime), Exp1Summary.Time$mean[Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]]
        + Exp1Summary.Time$se[Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]], 
       unique(Exp1Summary.Time$DateTime), Exp1Summary.Time$mean[Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]]
         - Exp1Summary.Time$se[Exp1Summary.Time$Substrate==Substrates[i] & Exp1Summary.Time$NutLevel==Nutlevels[j]], 
         angle=90, code=3, length = 0.1)
    
  }
#shaded area for night
rect(unique(Exp1Summary.Time$DateTime)[3],-0.4,unique(Exp1Summary.Time$DateTime)[6],0.6,col = rgb(0.5,0.5,0.5,1/4), border = NA)


axis(1, unique(Exp1Summary.Time$DateTime),c('10:00','14:00','18:00','22:00','2:00','6:00','10:00'))
legend('topright',legend=Nutlevels, col=cols, pch=19, bty = 'n')

}

#Experiment 2
Exp2Summary.Time <- ddply(Exp2Data, c( "NutLevel", "DateTime"), summarise,
                          N    = length(DeltapH),
                          mean = mean(DeltapH, na.rm = T),
                          sd   = sd(DeltapH, na.rm = T),
                          se   = sd / sqrt(N)
)

par(mfrow=c(1,1))
plot(NA, ylim=c(-0.3,0.5), xaxt="n", xlab="Time", ylab="Delta pH", main = 'Mixed Substrates')
abline(h=0)
par(new = TRUE)
cols <- c('red','green','blue')
#
for (j in 1:length(Nutlevels)){
  par(new = TRUE)
  
  plot(as.numeric(Exp2Summary.Time$DateTime [Exp2Summary.Time$NutLevel==Nutlevels[j]]),
       Exp2Summary.Time$mean[Exp2Summary.Time$NutLevel==Nutlevels[j]], col = cols[j],
       pch=19, ylim=c(-0.3,0.5), type="b", xaxt='n', ylab='', xlab='')
  
  arrows(unique(Exp1Summary.Time$DateTime), Exp2Summary.Time$mean[Exp2Summary.Time$NutLevel==Nutlevels[j]]
         + Exp2Summary.Time$se[Exp2Summary.Time$NutLevel==Nutlevels[j]], 
         unique(Exp1Summary.Time$DateTime), Exp2Summary.Time$mean[Exp2Summary.Time$NutLevel==Nutlevels[j]]
         - Exp2Summary.Time$se[Exp2Summary.Time$NutLevel==Nutlevels[j]], 
         angle=90, code=3, length = 0.1)
  
}
rect(unique(Exp1Summary.Time$DateTime)[3],-0.4,unique(Exp1Summary.Time$DateTime)[6],0.6,col = rgb(0.5,0.5,0.5,1/4), border = NA)
axis(1, unique(Exp1Summary.Time$DateTime),c('10:00','14:00','18:00','22:00','2:00','6:00','10:00'))
legend('topright',legend=Nutlevels, col=cols, pch=19, bty='n')


## calculate the pH of the water from mixing all the communities together and compare it to the
# pH of the actual mixed communities



