

rm(list=ls())
load('Scaling/ProcBWorkspace.RData')


library('tidyverse')
library('lme4')
library('lmerTest')

# rename the columns so that everything is the same
colnames(Coral)[which(colnames(Coral)=='Volume'|colnames(Coral)=='SA')]<-c('FinalVol','FinalSA')
colnames(Rubble)[which(colnames(Rubble)=='Volume'|colnames(Rubble)=='SA')]<-c('FinalVol','FinalSA')
colnames(Sand)[which(colnames(Sand)=='Vol'|colnames(Sand)=='SA')]<-c('FinalVol','FinalSA')

varlist <-c("AFDW","DW","FinalVol","FinalSA")  #custom list of vars 

# sum by aquarium and sample ID for experiment 1
biolist<-list(Coral, Algae, Rubble, Sand)
substrate<-c('Coral','Algae','Rubble','Sand')
e<-vector('list',4)

for(i in 1:length(biolist)){
  # get sums for experiment 1 by individuals
    a<- biolist[[i]] %>%group_by(Aq_Ex1, SampleID) %>%
    summarise_at(varlist, sum) # sum by aquarium number 
  
  # sum for experiment 2
  b<-biolist[[i]] %>%
    group_by(Aq_Ex2, SampleID) %>%
    summarise_at(varlist, sum)
  
  # get the total sum by aquarium
  c<-biolist[[i]] %>%
    group_by(Aq_Ex1) %>%
    summarise_at(varlist, funs(total=sum))
  
  # merge everything
  d<-left_join(a,b)
  d<-left_join(d,c)
  
  # calculate proportion of aquarium for each sample
  proportions<-d[,varlist]/d[,c("AFDW_total",'DW_total','FinalVol_total',"FinalSA_total")]
  PropData<-cbind(data.frame(d),proportions)
  colnames(PropData)[12:15]<-c('propSA','propAFDW','propDW','propVolume')
  PropData$Substrate<-rep(substrate[i], nrow(d))
  
  e[[i]]<-PropData
  e[[i]][,'Aq_Ex1']<-as.integer(e[[i]][,'Aq_Ex1'])
  }

# bring everything together
ScalDat<-rbind(e[[1]],e[[2]],e[[3]],e[[4]])

### add normalization for residence time
#experiment 1
ResData.1<-AllData[AllData$Experiment==1,c('Aquarium','ResTime.mean')]
# make the column names the same
colnames(ResData.1)[c(1,2)]<-c('Aq_Ex1','ResTime1')
# make it one value per aquarium
ResData.1 <-ResData.1 %>% 
  group_by(Aq_Ex1) %>% 
  summarise_at('ResTime1',mean)

#join with the scaled data
ScalDat<-left_join(ScalDat,ResData.1)
# experiment 2 residence time
ResData.2<-AllData[AllData$Experiment==2,c('Aquarium','ResTime.mean')]
# make the column names the same
colnames(ResData.2)[c(1,2)]<-c('Aq_Ex2','ResTime2')
ResData.2 <-ResData.2 %>% 
  group_by(Aq_Ex2) %>% 
  summarise_at('ResTime2',mean)
#join with the scaled data
ScalDat<-left_join(ScalDat,ResData.2)

# Calculate N uptake
AllData$N.uptake<-NutCalc(AllData$HeaderN, AllData$TankN, AllData$ResTime.mean, AllData$AFDW)
AllData$P.uptake<-NutCalc(AllData$HeaderP, AllData$TankP, AllData$ResTime.mean, AllData$AFDW)

# Experiment one data
DataEx1 <- AllData[AllData$Experiment==1,]
colnames(DataEx1)[1] <- "Aq_Ex1"

# merge with the NEC and NEP data
ScalDat<-left_join(DataEx1,ScalDat,by=c("Aq_Ex1","Substrate"))

# calculate estimated rate based on proportion of weight
ScalDat$NN.wtd<-ScalDat$N.uptake*ScalDat$propAFDW
ScalDat$PO.wtd<-ScalDat$P.uptake*ScalDat$propAFDW
ScalDat$NCP.wtd<-ScalDat$NCP.AFDW*ScalDat$propAFDW
ScalDat$NEC.wtd<-ScalDat$NEC.AFDW*ScalDat$propAFDW


# now calculate the predicted
DataEx2 <- AllData[AllData$Experiment==2,]
colnames(DataEx2)[1] <- "Aq_Ex2"
colnames(DataEx2)[49] <- "DateTimeEx2"

ScalDat_Aqua<-ScalDat %>%
  group_by(Aq_Ex2, DateTime,NutLevel) %>%
  summarise(NN.pred = sum(NN.wtd), PO.pred = sum(PO.wtd), NCP.pred = sum(NCP.wtd), NEC.pred = sum(NEC.wtd))

ScalDat_Aqua$DateTimeEx2<-ScalDat_Aqua$DateTime+60*60*48

DataEx2 <- left_join(DataEx2,ScalDat_Aqua,by=c("Aq_Ex2","DateTimeEx2","NutLevel"))
levels(DataEx2$DateTimeEx2)<- c(1:7)

# calculate observed - predicted
DataEx2 <- mutate(DataEx2,NN.diff = N.uptake - NN.pred,
                  PO.diff = P.uptake - PO.pred,
                  NCP.diff = NCP.AFDW - NCP.pred,
                  NEC.diff = NEC.AFDW - NEC.pred)

DataEx2$DateTimeEx2<- as.factor(DataEx2$DateTimeEx2)
DataEx2$DayNight<- as.factor(DataEx2$DayNight)


# do we see differences in N uptake by nutrients or day night between predicted and observed?
mod1.NN.diff <- lmer(NN.diff ~ DayNight*NutLevel
                           + (1|Tank) 
                           + (DayNight|DateTimeEx2),data=DataEx2)

Uptake.diff.means<-DataEx2 %>%
  group_by(DayNight, NutLevel) %>%
  summarise(.,NN.diff.mean = mean(NN.diff),NN.diff.SE = sd(NN.diff)/sqrt(n()), PO.diff.mean = mean(PO.diff),PO.diff.SE = sd(PO.diff)/sqrt(n()),
            NCP.diff.mean = mean(NCP.diff), NCP.diff.SE = sd(NCP.diff)/sqrt(n()),NEC.diff.mean = mean(NEC.diff), NEC.diff.SE = sd(NEC.diff)/sqrt(n()) )

par(mfrow= c(1,2))
plot(rep(1:3,2), Uptake.diff.means$NN.diff.mean, col = Uptake.diff.means$DayNight,
     pch = 19, cex = 2, ylim = c(-0.2,1), xaxt = 'n', ylab = 'Difference in uptake rate', xlab = '', main = 'N+N')
segments(rep(1:3,2), Uptake.diff.means$NN.diff.mean+Uptake.diff.means$NN.diff.SE, 
         rep(1:3,2), Uptake.diff.means$NN.diff.mean-Uptake.diff.means$NN.diff.SE,col = Uptake.diff.means$DayNight)
abline(h=0)
axis(1, at = c(1:3), c("Ambient","Medium","High"))
plot(rep(1:3,2), Uptake.diff.means$PO.diff.mean, col = Uptake.diff.means$DayNight,
     pch = 19, cex = 2, ylim = c(-0.2,0.15), xaxt = 'n', ylab = 'Difference in uptake rate', xlab = '', main = 'PO')
segments(rep(1:3,2), Uptake.diff.means$PO.diff.mean+Uptake.diff.means$PO.diff.SE, 
         rep(1:3,2), Uptake.diff.means$PO.diff.mean-Uptake.diff.means$PO.diff.SE,col = Uptake.diff.means$DayNight)
abline(h=0)
axis(1, at = c(1:3), c("Ambient","Medium","High"))

# averages by nutrients without day night
Uptake.diff.means<-DataEx2 %>%
  group_by(NutLevel) %>%
  summarise(.,NN.diff.mean = mean(NN.diff),NN.diff.SE = sd(NN.diff)/sqrt(n()), PO.diff.mean = mean(PO.diff),PO.diff.SE = sd(PO.diff)/sqrt(n()),
            NCP.diff.mean = mean(NCP.diff), NCP.diff.SE = sd(NCP.diff)/sqrt(n()),NEC.diff.mean = mean(NEC.diff), NEC.diff.SE = sd(NEC.diff)/sqrt(n()) )
par(mfrow= c(1,2))
plot(1:3, Uptake.diff.means$NN.diff.mean, 
     pch = 19, cex = 2, ylim = c(0,0.7), xaxt = 'n', ylab = 'Difference in uptake rate', xlab = '', main = 'N+N')
segments(1:3, Uptake.diff.means$NN.diff.mean+Uptake.diff.means$NN.diff.SE, 
         1:3, Uptake.diff.means$NN.diff.mean-Uptake.diff.means$NN.diff.SE)
abline(h=0)
axis(1, at = c(1:3), c("Ambient","Medium","High"))
plot(1:3, Uptake.diff.means$PO.diff.mean, pch = 19, cex = 2, ylim = c(-0.05,0.05), xaxt = 'n', ylab = 'Difference in uptake rate', xlab = '', main = 'PO')
segments(1:3, Uptake.diff.means$PO.diff.mean+Uptake.diff.means$PO.diff.SE, 
         1:3, Uptake.diff.means$PO.diff.mean-Uptake.diff.means$PO.diff.SE)
abline(h=0)
axis(1, at = c(1:3), c("Ambient","Medium","High"))

# do we see differences in N uptake by nutrients between predicted and observed?
mod2.NN.diff <- lmer(NN.diff ~ NutLevel
                     + (1|Tank) 
                     + (1|DateTimeEx2),data=DataEx2)


mod2.PO.diff <- lmer(PO.diff ~ NutLevel
                     + (1|Tank) 
                     + (1|DateTimeEx2),data=DataEx2)

## NCP
par(mfrow= c(1,2))
plot(1:3, Uptake.diff.means$NCP.diff.mean, 
     pch = 19, cex = 2, ylim = c(-5,5), xaxt = 'n', ylab = 'Difference in rate', xlab = '', main = 'NCP')
segments(1:3, Uptake.diff.means$NCP.diff.mean+Uptake.diff.means$NCP.diff.SE, 
         1:3, Uptake.diff.means$NCP.diff.mean-Uptake.diff.means$NCP.diff.SE)
abline(h=0)
#NEC
axis(1, at = c(1:3), c("Ambient","Medium","High"))
plot(1:3, Uptake.diff.means$NEC.diff.mean, pch = 19, cex = 2, ylim = c(-1,1), xaxt = 'n', xlab = '', main = 'NEC')
segments(1:3, Uptake.diff.means$NEC.diff.mean+Uptake.diff.means$NEC.diff.SE, 
         1:3, Uptake.diff.means$NEC.diff.mean-Uptake.diff.means$NEC.diff.SE)
abline(h=0)
axis(1, at = c(1:3), c("Ambient","Medium","High"))
