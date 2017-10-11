### CRANE Scaling Analysis
### Started Sept 5, 2017
### Testing whether the NEC and NEP rates are additive between single substrate and mixed substrates


source("CRANEMetabolismAnalysis.R")
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

rvs <- c("NEC","NCP")
##Basic Plotting 
for (i in (1:length(rvs))){
  par(mfrow=c(2,2))
  for (j in (1:length(sandvarlist))){
    nmx <- paste0(rvs[i],".",sandvarlist[j])
    nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
    plot(DataEx2[[nmx]],DataEx2[[nmy]],col=as.numeric(DataEx2$NutLevel),
         xlab=nmx,ylab=nmy)
    legend("topleft",legend=levels(DataEx2$NutLevel),col=unique(as.numeric(DataEx2$NutLevel)),pch=1,bty="n")
    abline(0,1)
  }
}

for (i in (1:length(rvs))){
  par(mfrow=c(2,2))
  for (j in (1:length(sandvarlist))){
    nmx <- paste0(rvs[i],".",sandvarlist[j])
    nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
    plot(DataEx2[[nmx]],DataEx2[[nmy]],col=levels(DataEx2$DateTimeEx2),
         xlab=nmx,ylab=nmy)
    legend("topleft",legend=unique(DataEx2$Time),col=unique(levels(DataEx2$DateTimeEx2)),pch=1,bty="n")
    abline(0,1)
    
  }
}

#Plotting Deviation by nutrients and by time
par(mfrow=c(2,1))
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    print(nmy)
    plot(DataEx2[[nmy]]~DataEx2$NutLevel, ylab=nmy)
    abline(0,0,col="blue")
  }
}

#Plotting Deviation by nutrients and by time
par(mfrow=c(2,1))
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    print(nmy)
    plot(DataEx2[[nmy]]~as.factor(DataEx2$DayNight), ylab=nmy)
    abline(0,0,col="blue")
  }
}

###Should scale all the data for analysis

#Loop over all rvs for basic models
sandvarlist <- ("AFDW")
#for (i in (1:length(rvs))){
#  for (j in (1:length(sandvarlist))){
#    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    mod1 <- lmer(NCP.AFDW.diff ~ DayNight*NutLevel
                 + (1|Tank) 
                 + (1|DateTimeEx2),data=DataEx2)
#    print(nmy)
    print(anova(mod1))
#  }
#}
sjp.lmer(mod1,type="fe")
    
    
#Histograms
for (i in (1:length(rvs))){
  for (j in (1:length(sandvarlist))){
    nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
    hist(DataEx2[[nmy]])
  }
}

