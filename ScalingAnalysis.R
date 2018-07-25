### CRANE Scaling Analysis
### Started Sept 5, 2017
### Last update July 24, 2018
### Testing whether the NEC and NEP rates are additive between single substrate and mixed substrates


#source("CRANEMetabolismAnalysis.R")
metadata <- read.csv2(file="Scaling/data_ProcB/Metadata.csv")
library("sjPlot")
library("dplyr")

#Add columns to substrate data frames that calculate each sample's proportion of the total (AFDW, DW, Vol, SA) per aquarium

propAqua <- function(datfram,x,Aqua) {
  #sum x within each aquarium
  a <- aggregate(datfram[x],datfram[Aqua][1],sum)
  #sum x within each SampleID (necessary for coral bc two species) and bring along Aq_Ex1 identification
  b <- cbind(aggregate(datfram[x],datfram["SampleID"],sum),aggregate(datfram[Aqua][1],datfram["SampleID"],mean),aggregate(datfram[Aqua][2],datfram["SampleID"],mean))
  #calculate sample x as a proportion of aquarium x for each sample
  y <- cbind(b[Aqua],b["SampleID"],b[x]/a[[x]][match(b[,Aqua[1]],a[,Aqua[1]])])
  return(y)
}

#datframs = list(Coral=Coral,Algae=Algae,Rubble=Rubble,Sand=Sand)
datframs = c("Coral","Algae","Rubble","Sand")  #list of data frames to loop over
varlist = c("AFDW","DW","Volume","SA")         #list of variables in each dataframe that function propAqua will be used on
algvarlist = c("AFDW","DW","FinalVol","FinalSA")  #custom list of vars for algae
sandvarlist = c("AFDW","DW","Vol","SA")           #custom list of vars for sand
newvarlist = c("propAFDW","propDW","propVol","propSA")  #list of names for newly calculated variables
aqlist = c("Aq_Ex1","Aq_Ex2")
#loop over dataframes and add a newly calculated variable
#create new dataframe of weights: ScalDat
ScalDat <- data.frame()
for (i in c(1:length(datframs))){
  #get correct variable list for this dataframe
  vr = varlist
  if (datframs[i]=="Algae") vr=algvarlist
  if (datframs[i]=="Sand") vr=sandvarlist
  #loop over variables in the data frame
  for (j in c(1:length(vr))){
    a <- propAqua(get(datframs[i]),vr[j],aqlist)  #call propAqua to calculate the proportion of vr[j] that each sample comprises in its aquarium from Expt 1
    colnames(a) <- c(aqlist,"SampleID",newvarlist[j])   #rename the returned variable from the newvarname list
    #create a temporary matrix with the proportions based on 4 measures for each substrate
    if (j==1) {
      aa <- cbind(matrix(datframs[i],nrow = nrow(a),ncol=1,dimnames=list(NULL,"Substrate")),a)
      }
    else aa <- cbind(aa,a[newvarlist[j]])
    #assign(datframs[i],cbind(get(datframs[i]),a))   #use assign to create a new dataframe with the same name and assign it the value of the old dataframe (using get) cbind'ed with the new variable
  }
  if (i==1) ScalDat <- aa else ScalDat <- rbind(ScalDat,aa)  #concatenate the rows together into a single dataframe
}

DataEx1 <- AllData[AllData$Experiment==1,]
colnames(DataEx1)[1] <- "Aq_Ex1"
nr <- nrow(ScalDat)
ScalDat <- ScalDat[rep(1:nrow(ScalDat),times=length(unique(DataEx1$DateTime))),]
ScalDat$DateTime <- rep(unique(DataEx1$DateTime),each=nr)
ScalDat <- plyr::join(ScalDat,DataEx1,by=c("Aq_Ex1","DateTime","Substrate"),type="left")

ScalDat <- mutate(ScalDat,NEC.AFDW.wtd = NEC.AFDW*propAFDW,NCP.AFDW.wtd = NCP.AFDW*propAFDW)
ScalDat <- mutate(ScalDat,NEC.DW.wtd = NEC.DW*propDW,NCP.DW.wtd = NCP.DW*propDW)
ScalDat <- mutate(ScalDat,NEC.Vol.wtd = NEC.Vol*propVol,NCP.Vol.wtd = NCP.Vol*propVol)
ScalDat <- mutate(ScalDat,NEC.SA.wtd = NEC.SA*propSA,NCP.SA.wtd = NCP.SA*propSA)

#Now, add calculations for predicted NCP and NEC to Expt2 data to compare
DataEx2 <- AllData[AllData$Experiment==2,]
colnames(DataEx2)[1] <- "Aq_Ex2"
colnames(DataEx2)[49] <- "DateTimeEx2"

ScalDat_Aqua <- ddply(ScalDat,c("Aq_Ex2","DateTime","NutLevel"),summarise,
                                 NEC.AFDW.pred = sum(NEC.AFDW.wtd),
                                 NEC.DW.pred = sum(NEC.DW.wtd),
                                 NEC.Vol.pred = sum(NEC.Vol.wtd),
                                 NEC.SA.pred = sum(NEC.SA.wtd),
                                 NCP.AFDW.pred = sum(NCP.AFDW.wtd),
                                 NCP.DW.pred = sum(NCP.DW.wtd),
                                 NCP.Vol.pred = sum(NCP.Vol.wtd),
                                 NCP.SA.pred = sum(NCP.SA.wtd))
ScalDat_Aqua<- mutate(ScalDat_Aqua, DateTimeEx2 = DateTime+60*60*48)

DataEx2 <- join(DataEx2,ScalDat_Aqua,by=c("Aq_Ex2","DateTimeEx2","NutLevel"),"left")
levels(DataEx2$DateTimeEx2)<- c(1:7)
DataEx2 <- mutate(DataEx2,NEC.AFDW.diff = NEC.AFDW - NEC.AFDW.pred,
                          NEC.DW.diff = NEC.DW - NEC.DW.pred,
                          NEC.Vol.diff = NEC.Vol - NEC.Vol.pred,
                          NEC.SA.diff = NEC.SA - NEC.SA.pred,
                          NCP.AFDW.diff = NCP.AFDW - NCP.AFDW.pred,
                          NCP.DW.diff = NCP.DW - NCP.DW.pred,
                          NCP.Vol.diff = NCP.Vol - NCP.Vol.pred,
                          NCP.SA.diff = NCP.SA - NCP.SA.pred)

DataEx2$DateTimeEx2<- as.factor(DataEx2$DateTimeEx2)
DataEx2$DayNight<- as.factor(DataEx2$DayNight)

rvs <- c("NEC","NCP") #,"deltapH")

#Run models for NEC and NCP differences normalized for AFDW:
#  rv ~ Nutrients * DayNight +(1|Tank) + (DayNight|DateTimeEx2)

#Run basic mixed model for NEC normalized to AFDW
pdf('Scaling/plots/NEC.AFDW.diff v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
sink('Scaling/stats/NEC.AFDW.diff v nuts x daynight.txt')
mod1.NEC.AFDW.diff <- lmer(NEC.AFDW.diff ~ DayNight*NutLevel
                           + (1|Tank) 
                           + (DayNight|DateTimeEx2),data=DataEx2)
print(anova(mod1.NEC.AFDW.diff))
print(summary(mod1.NEC.AFDW.diff))
sjp.lmer(mod1.NEC.AFDW.diff,type="fe")
sjp.lmer(mod1.NEC.AFDW.diff,type="re")  
dev.off()
sink()

#Run basic mixed model for NCP normalized to AFDW
pdf('Scaling/plots/NCP.AFDW.diff v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
sink('Scaling/stats/NCP.AFDW.diff v nuts x daynight.txt')
mod1.NCP.AFDW.diff <- lmer(NCP.AFDW.diff ~ DayNight*NutLevel
#                           + (1|Tank) 
                           + (DayNight|DateTimeEx2),data=DataEx2)
print(anova(mod1.NCP.AFDW.diff))
print(summary(mod1.NCP.AFDW.diff))
sjp.lmer(mod1.NCP.AFDW.diff,type="fe")
sjp.lmer(mod1.NCP.AFDW.diff,type="re")  
dev.off()
sink()

#SCALED: Run basic mixed model for NEC normalized to AFDW
DataEx2$NEC.AFDW.diff.rs <- rescale(DataEx2$NEC.AFDW.diff)
pdf('Scaling/plots/NEC.AFDW.diff.rs v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
sink('Scaling/stats/NEC.AFDW.diff.rs v nuts x daynight.txt')
mod1.NEC.AFDW.diff.rs <- lmer(NEC.AFDW.diff ~ DayNight*NutLevel
                           + (1|Tank)
                           + (DayNight|DateTimeEx2),data=DataEx2)
print(anova(mod1.NEC.AFDW.diff.rs))
print(summary(mod1.NEC.AFDW.diff.rs))
sjp.lmer(mod1.NEC.AFDW.diff.rs,type="fe")
sjp.lmer(mod1.NEC.AFDW.diff.rs,type="re")
dev.off()
sink()

#SCALED: Run basic mixed model for NCP normalized to AFDW
DataEx2$NCP.AFDW.diff.rs <- rescale(DataEx2$NCP.AFDW.diff)
pdf('Scaling/plots/NCP.AFDW.diff.rs v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
sink('Scaling/stats/NCP.AFDW.diff.rs v nuts x daynight.txt')
mod2.NCP.AFDW.diff.rs <- lmer(NCP.AFDW.diff.rs ~ DayNight*NutLevel
                           + (1|Tank)
                           + (DayNight|DateTimeEx2),data=DataEx2)
print(anova(mod2.NCP.AFDW.diff.rs))
print(summary(mod2.NCP.AFDW.diff.rs))
sjp.lmer(mod2.NCP.AFDW.diff.rs,type="fe")
sjp.lmer(mod2.NCP.AFDW.diff.rs,type="re")
dev.off()
sink()

RV.diff.means<-ddply(DataEx2, c("DayNight","NutLevel"), summarise,
                   Mean.NCP.AFDW.diff=mean(NCP.AFDW.diff, na.rm=T),
                   Mean.NEC.AFDW.diff=mean(NEC.AFDW.diff, na.rm=T),
                   reps=12,
                   SE.NCP.AFDW.diff=sd(NCP.AFDW.diff)/sqrt(reps),
                   SE.NEC.AFDW.diff=sd(NEC.AFDW.diff)/sqrt(reps)
)


#NEC plots net
  x<-barplot(matrix(RV.diff.means$Mean.NCP.AFDW.diff,nrow=3,ncol=2),beside=TRUE,
             ylab=("Exp(NCP) - Obs(NCP), AFDW normalized")
  errorbars(x,NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]],0,NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  lines(x,c(0,0,0))
}


##Basic Plotting 
for (i in (1:length(rvs))){
  par(mfrow=c(2,2))
  for (j in (1:length(sandvarlist))){
    nmx <- paste0(rvs[i],".",sandvarlist[j])
    nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
    plot(DataEx2[[nmx]],DataEx2[[nmy]],col=as.numeric(unique(DataEx2$NutLevel)),
         xlab=nmx,ylab=nmy)
    legend("topleft",legend=levels(DataEx2$NutLevel),col=as.numeric(unique(DataEx2$NutLevel)),pch=1,bty="n")
    abline(0,1)
  }
}

for (i in (1:length(rvs))){
  par(mfrow=c(2,2))
  for (j in (1:length(sandvarlist))){
    nmx <- paste0(rvs[i],".",sandvarlist[j])
    nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
    plot(DataEx2[[nmx]],DataEx2[[nmy]],col=as.numeric(unique(DataEx2$DayNight)),
         xlab=nmx,ylab=nmy)
    legend("topleft",legend=unique(DataEx2$Time),col=as.numeric(unique(DataEx2$DayNight)),pch=1,bty="n")
    abline(0,1)
    
  }
}

#Plotting Deviation by nutrients and by time
par(mfrow=c(2,2))
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    print(nmy)
    plot(DataEx2[[nmy]]~DataEx2$NutLevel, ylab=nmy)
    abline(0,0,col="blue")
  }
}

#Plotting Deviation by nutrients and by time
par(mfrow=c(2,2))
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    print(nmy)
    plot(DataEx2[[nmy]]~as.factor(DataEx2$DayNight), ylab=nmy)
    abline(0,0,col="blue")
  }
}



#Histograms
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    hist(DataEx2[[nmy]], xlab=paste0(nmy),main="")
  }
}

