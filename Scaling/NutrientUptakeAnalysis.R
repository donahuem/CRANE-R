

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

#### light normalization #####
## run the light script
source('Scaling/lightnormalization.R')

## average NCP and NEC rates by substrate and incubation tank
TankRates<-AllData %>%
  group_by(Substrate,Tank, NutLevel) %>%
  summarise_at(c('NCP.AFDW','NEC.AFDW'),  mean)

# create an empty dataframe to put the coefficients in 
coefs.sub.nut<-data.frame(matrix(NA, nrow=12, ncol=6))
colnames(coefs.sub.nut)<-c('Substrate','NutLevel','NCP.Intercept','NCP.Slope','NEC.Intercept','NEC.Slope')                          
coefs.sub.nut$Substrate<-rep(sub[1:4],3)
coefs.sub.nut$NutLevel<-c(rep(as.character(Nuts[1]),4),
                          rep(as.character(Nuts[2]),4),rep(as.character(Nuts[3]),4))

# create a dataframe for just the substrate coefficients
coefs.sub<-data.frame(matrix(NA, nrow=4, ncol=5))
colnames(coefs.sub)<-c('Substrate','NCP.Intercept','NCP.Slope','NEC.Intercept','NEC.Slope')                          
coefs.sub$Substrate<-sub[1:4]

# Light by NCP plot across substrates and nutrients
png('Scaling/plots/LightNormNCP.png',width = 10000, height = 12000, res = 800 )
par(mfrow=c(4,3))
for (i in 1:4){
  for (j in 1:3){
  plot(CumLight[1,], TankRates$NCP.AFDW[TankRates$Substrate==sub[i] & TankRates$NutLevel == Nuts[j]], 
                  cex.lab = 2,cex.main = 2, cex.axis = 1.5, cex = 1.5,main = paste(sub[i], Nuts[j]), 
       pch = 19,xlab = 'Cumulative PAR', ylab = 'mean NCP', ylim =c(min(TankRates$NCP.AFDW),
                                                                    max(TankRates$NCP.AFDW)))
mod<-lm(TankRates$NCP.AFDW[TankRates$Substrate==sub[i] & TankRates$NutLevel == Nuts[j]]~CumLight[1,])
lines(CumLight[1,],predict(mod))
legend('topleft',c(paste('r2 = ',round(summary(mod)$r.squared,2)),paste('slope = ',formatC(coef(mod)[2], format = "e", digits = 2))), bty = 'n', cex = 2)
coefs.sub.nut[coefs.sub.nut$Substrate==sub[i] & coefs.sub.nut$NutLevel == Nuts[j],3:4]<-coef(mod)
      }
  }
dev.off()

## same figures, but only across substrates
## average NCP and NEC rates by substrate and incubation tank
TankRates.sub<-AllData %>%
  group_by(Substrate,Tank) %>%
  summarise_at(c('NCP.AFDW','NEC.AFDW'),  mean)
#NCP
png('Scaling/plots/LightNormNCPSub.png',width = 10000, height = 12000, res = 800 )
par(mfrow=c(2,2))
for (i in 1:4){
    plot(CumLight[1,], TankRates.sub$NCP.AFDW[TankRates.sub$Substrate==sub[i]], ylim = c(min(TankRates.sub$NCP.AFDW), max(TankRates.sub$NCP.AFDW)),
         cex.lab = 2,cex.main = 2, cex.axis = 1.5, cex = 1.5,main = paste(sub[i]), pch = 19,xlab = 'Cumulative PAR', ylab = 'mean NCP')
    mod<-lm(TankRates.sub$NCP.AFDW[TankRates.sub$Substrate==sub[i]]~CumLight[1,])
    lines(CumLight[1,],predict(mod))
    legend('topleft',c(paste('r2 = ',round(summary(mod)$r.squared,2)),paste('slope = ',formatC(coef(mod)[2], format = "e", digits = 2))), bty = 'n', cex = 2)
    coefs.sub[coefs.sub$Substrate==sub[i],2:3]<-coef(mod)
    
    }

dev.off()
#NEC
png('Scaling/plots/LightNormNECSub.png',width = 10000, height = 12000, res = 800 )
par(mfrow=c(2,2))
for (i in 1:4){
  plot(CumLight[1,], TankRates.sub$NEC.AFDW[TankRates.sub$Substrate==sub[i]], ylim = c(min(TankRates.sub$NEC.AFDW), max(TankRates.sub$NEC.AFDW)),
       cex.lab = 2,cex.main = 2, cex.axis = 1.5, cex = 1.5,main = paste(sub[i]), pch = 19,xlab = 'Cumulative PAR', ylab = 'mean NCC')
  mod<-lm(TankRates.sub$NEC.AFDW[TankRates.sub$Substrate==sub[i]]~CumLight[1,])
  lines(CumLight[1,],predict(mod))
  legend('topleft',c(paste('r2 = ',round(summary(mod)$r.squared,2)),paste('slope = ',formatC(coef(mod)[2], format = "e", digits = 2))), bty = 'n', cex = 2)
  coefs.sub[coefs.sub$Substrate==sub[i],4:5]<-coef(mod)
  }

dev.off()
# Light by NEC plot across substrates and nutrients
png('Scaling/plots/LightNormNEC.png',width = 10000, height = 12000, res = 800 )
par(mfrow=c(4,3))
for (i in 1:4){
  for (j in 1:3){
    plot(CumLight[1,], TankRates$NEC.AFDW[TankRates$Substrate==sub[i] & TankRates$NutLevel == Nuts[j]], ylim = c(min(TankRates.sub$NEC.AFDW), max(TankRates.sub$NEC.AFDW)),
         cex.lab = 2,cex.main = 2, cex.axis = 1.5, cex = 1.5,main = paste(sub[i], Nuts[j]), pch = 19,xlab = 'Cumulative PAR', ylab = 'mean NCC')
    mod<-lm(TankRates$NEC.AFDW[TankRates$Substrate==sub[i] & TankRates$NutLevel == Nuts[j]]~CumLight[1,])
    lines(CumLight[1,],predict(mod))
    legend('topleft',c(paste('r2 = ',round(summary(mod)$r.squared,2)),paste('slope = ',formatC(coef(mod)[2], format = "e", digits = 2))), bty = 'n', cex = 2)
    coefs.sub.nut[coefs.sub.nut$Substrate==sub[i] & coefs.sub.nut$NutLevel == Nuts[j],5:6]<-coef(mod)
    
      }
}
dev.off()

# calculate estimated rate based on proportion of weight
ScalDat$NN.wtd<-ScalDat$N.uptake*ScalDat$propAFDW
ScalDat$PO.wtd<-ScalDat$P.uptake*ScalDat$propAFDW
ScalDat$NCP.wtd<-ScalDat$NCP.AFDW*ScalDat$propAFDW
ScalDat$NEC.wtd<-ScalDat$NEC.AFDW*ScalDat$propAFDW

### normalize the NEC and NCP data to constant light. ##
# Take the relationship between cumulative light and the rate (NCP, NEC). Put in the delta light 
# between day 1 and 2 into the equation for each bin x substrate. The NCP value calculated from this 
# equation is how much the NCP changed between day 1 and 2 due to differences in light. This value gets
#subtracted from the predicted NCP to account for light effects. THis need to be done on the average daily NCP and NEC
# based on how I ran the regressions
# delta cumulative light between day 1 ans 2
deltaLight<-data.frame(as.matrix(CumLight[1,] - CumLight[2,]))
deltaLight$Tank<-c(1,2,3)
colnames(deltaLight)[1]<-'deltaPAR'
# now calculate the predicted
DataEx2 <- AllData[AllData$Experiment==2,]
colnames(DataEx2)[1] <- "Aq_Ex2"
colnames(DataEx2)[49] <- "DateTimeEx2"

#take the daily averages 
ScalDat_Ave<-ScalDat %>%
  group_by(Aq_Ex2, NutLevel, Substrate, Tank, Experiment) %>%
  summarise(NN.pred = mean(NN.wtd), PO.pred = mean(PO.wtd), NCP.pred = mean(NCP.wtd), NEC.pred = mean(NEC.wtd))%>%
  left_join(.,deltaLight)
ScalDat_Ave<-ScalDat_Ave[-c(121:nrow(ScalDat_Ave)),]
ScalDat_Ave.sub<-ScalDat_Ave

# add light coefs for substrate and nutrients
ScalDat_Ave<-left_join(ScalDat_Ave, coefs.sub.nut) 

# just substrate coefficients
ScalDat_Ave.sub<-left_join(ScalDat_Ave.sub, coefs.sub)

#scale to light with both nutrients and substrate
ScalDat_Ave$NCP.pred.light<-with(ScalDat_Ave,NCP.pred+(NCP.Intercept+deltaPAR*NCP.Slope))
ScalDat_Ave$NEC.pred.light<-with(ScalDat_Ave,NEC.pred+(NEC.Intercept+deltaPAR*NEC.Slope))

#scale to light with only substrate coeffs
ScalDat_Ave.sub$NCP.pred.light<-with(ScalDat_Ave.sub,NCP.pred+(NCP.Intercept+deltaPAR*NCP.Slope))
ScalDat_Ave.sub$NEC.pred.light<-with(ScalDat_Ave.sub,NEC.pred+(NEC.Intercept+deltaPAR*NEC.Slope))

# calculate observed - predicted for both substrate and nutrient coefficients
# sum across all 4 substrates in each aquarium
ScalDat_Avesum<-ScalDat_Ave %>%
  group_by(Aq_Ex2, NutLevel) %>%
  summarise(NCP.pred = sum(NCP.pred.light), NEC.pred = sum(NEC.pred.light))

# average the observed data
DataEx2_Ave<-DataEx2 %>% group_by(Aq_Ex2, NutLevel, Tank) %>%
  summarise(NCP.mean = mean(NCP.AFDW), NEC.mean = mean(NEC.AFDW))

# bring together the observed and predicted
DataEx2_Ave<-left_join(ScalDat_Avesum, DataEx2_Ave)
# calculate observed - predicted
DataEx2_Ave <- mutate(DataEx2_Ave,
                  NCP.diff = NCP.mean - NCP.pred,
                  NEC.diff = NEC.mean - NEC.pred)

# calculate observed - predicted for just substrate coefficients
# sum across all 4 substrates in each aquarium
ScalDat_Avesum.sub<-ScalDat_Ave.sub %>%
  group_by(Aq_Ex2, NutLevel) %>%
  summarise(NCP.pred = sum(NCP.pred.light), NEC.pred = sum(NEC.pred.light))

# average the observed data
DataEx2_Ave.sub<-DataEx2 %>% group_by(Aq_Ex2, NutLevel, Tank) %>%
  summarise(NCP.mean = mean(NCP.AFDW), NEC.mean = mean(NEC.AFDW))

# bring together the observed and predicted
DataEx2_Ave.sub<-left_join(ScalDat_Avesum.sub, DataEx2_Ave.sub)
# calculate observed - predicted
DataEx2_Ave.sub <- mutate(DataEx2_Ave.sub,
                      NCP.diff = NCP.mean - NCP.pred,
                      NEC.diff = NEC.mean - NEC.pred)
### stats #####
# does nutrients affect scaling with nutrient and substrate light coeffs?
mod1.NEC.diff <- lmer(NEC.diff ~ NutLevel
                     + (1|Tank),data=DataEx2_Ave)
mod1.NCP.diff <- lmer(NCP.diff ~ NutLevel
                      + (1|Tank),data=DataEx2_Ave)

# does nutrients affect scaling with substrate only light coeffs?
mod1.NEC.diff.sub <- lmer(NEC.diff ~ NutLevel
                      + (1|Tank),data=DataEx2_Ave.sub)
mod1.NCP.diff.sub <- lmer(NCP.diff ~ NutLevel
                      + (1|Tank),data=DataEx2_Ave.sub)

# plot #####
# nutrient and substrate light coefficients
Uptake.diff.means<-DataEx2_Ave %>%
  group_by(NutLevel) %>%
  summarise(.,NCP.diff.mean = mean(NCP.diff), NCP.diff.SE = sd(NCP.diff)/sqrt(n()),NEC.diff.mean = mean(NEC.diff), NEC.diff.SE = sd(NEC.diff)/sqrt(n()) )

## NCP
#par(mfrow= c(1,2))
par(mfrow= c(1,1))
plot(1:3, Uptake.diff.means$NCP.diff.mean, 
     pch = 19, cex = 2, ylim = c(-10,10), xaxt = 'n', ylab = 'Observed - predicted', xlab = '',type = 'b', lty=2)
segments(1:3, Uptake.diff.means$NCP.diff.mean+Uptake.diff.means$NCP.diff.SE, 
         1:3, Uptake.diff.means$NCP.diff.mean-Uptake.diff.means$NCP.diff.SE)
abline(h=0)
#NEC
axis(1, at = c(1:3), c("Ambient","Medium","High"))
points(1:3, Uptake.diff.means$NEC.diff.mean, pch = 19, cex = 2, ylim = c(-10,10), xaxt = 'n', xlab = '', main = 'NEC', col='grey',type = 'b', lty=2)
segments(1:3, Uptake.diff.means$NEC.diff.mean+Uptake.diff.means$NEC.diff.SE, 
         1:3, Uptake.diff.means$NEC.diff.mean-Uptake.diff.means$NEC.diff.SE)
legend('bottomleft', c('NCP','NCC'), pch = 19, col = c('black','grey'), bty = 'n')

# substrate light coefficients
Uptake.diff.means<-DataEx2_Ave.sub %>%
  group_by(NutLevel) %>%
  summarise(.,NCP.diff.mean = mean(NCP.diff), NCP.diff.SE = sd(NCP.diff)/sqrt(n()),NEC.diff.mean = mean(NEC.diff), NEC.diff.SE = sd(NEC.diff)/sqrt(n()) )

## NCP
#par(mfrow= c(1,2))
par(mfrow= c(1,1))
plot(1:3, Uptake.diff.means$NCP.diff.mean, 
     pch = 19, cex = 2, ylim = c(-5,12), xaxt = 'n', ylab = 'Observed - predicted', xlab = '',type = 'b', lty=2)
segments(1:3, Uptake.diff.means$NCP.diff.mean+Uptake.diff.means$NCP.diff.SE, 
         1:3, Uptake.diff.means$NCP.diff.mean-Uptake.diff.means$NCP.diff.SE)
abline(h=0)
#NEC
axis(1, at = c(1:3), c("Ambient","Medium","High"))
points(1:3, Uptake.diff.means$NEC.diff.mean, pch = 19, cex = 2, ylim = c(-10,10), xaxt = 'n', xlab = '', main = 'NEC', col='grey',type = 'b', lty=2)
segments(1:3, Uptake.diff.means$NEC.diff.mean+Uptake.diff.means$NEC.diff.SE, 
         1:3, Uptake.diff.means$NEC.diff.mean-Uptake.diff.means$NEC.diff.SE)
legend('bottomleft', c('NCP','NCC'), pch = 19, col = c('black','grey'), bty = 'n')

########################### stop ################################
#### without scaling to light or averaging over the day
ScalDat_Aqua<-ScalDat %>%
  group_by(Aq_Ex2, DateTime,NutLevel) %>%
  summarise(NN.pred = sum(NN.wtd), PO.pred = sum(PO.wtd), NCP.pred = sum(NCP.wtd), NEC.pred = sum(NEC.wtd))

ScalDat_Aqua$DateTimeEx2<-ScalDat_Aqua$DateTime+60*60*48 # get the correct time

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
par(mfrow= c(1,1))
plot(1:3, Uptake.diff.means$NCP.diff.mean, 
     pch = 19, cex = 2, ylim = c(-5,5), xaxt = 'n', ylab = 'Difference in rate', xlab = '', lty=2, type ='b')
segments(1:3, Uptake.diff.means$NCP.diff.mean+Uptake.diff.means$NCP.diff.SE, 
         1:3, Uptake.diff.means$NCP.diff.mean-Uptake.diff.means$NCP.diff.SE)
abline(h=0)
#NEC
axis(1, at = c(1:3), c("Ambient","Medium","High"))
points(1:3, Uptake.diff.means$NEC.diff.mean, pch = 19, cex = 2, ylim = c(-1,1), xaxt = 'n', xlab = '', col = 'grey', type = 'b', lty=2)
segments(1:3, Uptake.diff.means$NEC.diff.mean+Uptake.diff.means$NEC.diff.SE, 
         1:3, Uptake.diff.means$NEC.diff.mean-Uptake.diff.means$NEC.diff.SE)
abline(h=0)
#axis(1, at = c(1:3), c("Ambient","Medium","High"))
legend('bottomleft',c('NCP','NCC'), pch = 19, col = c('black','grey'), bty = 'n')