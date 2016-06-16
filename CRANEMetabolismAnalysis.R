####################################################
### CRANE Data Analysis for Biogeochemical Responses####
### Created by Nyssa Silbiger                   ###
### Created on 5/03/2016                       ###
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
library('seacarb')
library('MuMIn')
# Functions-----------------------------------------
#easy errorbar barplots
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

NECCalc<-function(HeaderTA,TankTA,ResidenceTime,SurfaceArea, TankVolume=5678,SWDenstiy=1.023){
 
   #NEC<-0.5*(HeaderTA-TankTA)*TankVolume*SWDenstiy/(ResidenceTime*SurfaceArea)/1000
   
   NEC<-0.5*(HeaderTA-TankTA)*(TankVolume/SurfaceArea)*(SWDenstiy/ResidenceTime)/1000
   
   
   return(NEC)
  #NEC calc calculated the net ecosystem calcification of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) rather than change in TA over time (Eulerian-- this is what I used for the 
  #biogeochemistry paper)
  #NEC is in umol cm^-2 hr-1  (or umol g-1 hr-1 if using one of the other measurements)
  
  #HeaderTA is TA from the header in umol/kg
  #TankTAis TA from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in kg/cm3 (default =1.023)
# 
   
}  

NCPCalc<-function(HeaderDIC,TankDIC,ResidenceTime,SurfaceArea, TankVolume=5678,SWDenstiy=1.023, NEC){
  
  deltaDIC<-(TankVolume*SWDenstiy*(HeaderDIC-TankDIC)/(ResidenceTime*SurfaceArea))/1000
  #substract calcification rate
  NCP<-deltaDIC-NEC
  return(NCP)
  #NEC calc calculated the net ecosystem calcification of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) rather than change in TA over time (Eulerian-- this is what I used for the 
  #biogeochemistry paper)
  #NCP is in umol C cm^-2 hr-1  (or umol g-1 hr-1 if using one of the other measurements)
  
  #HeaderDIC is DIC from the header in umol/kg
  #TankDICis DIC from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in kg/cm3 (default =1.023)
  #NEC is the calcification rate at that time 
  
}  

NECCalc2<-function(HeaderTA,TankTA,flow,SurfaceArea, TankVolume=5.678,SWDensity=1.023, time=3){
  
  tankarea <- 0.2032*0.22225; #m length x width of the tank (m2)
  #flow is in ml/min convert to L/hr
  flow<-flow*60/1000;
  
  #time is time in between sampling points
  #SA is in cm^3, convert to m^3
  SurfaceArea<-SurfaceArea/(100^2);
  
  
  #now convert flow rate to kg m-2 hr-1 so that I can use Andersson et al. 2009 eq 
  flowrate<-flow*SWDensity/tankarea;
  
  #DeltaTA/dt total change in TA in the tank over sample times (mmol m-2 hr-1)
  DTAdt<-NULL
  NEC<-NULL
  FinTA<-NULL
  FoutTA<-NULL
  for (i in 1:length(TankTA)-1){
  DTAdt[i]<-((TankTA[i+1]-TankTA[i])/(time*SurfaceArea[i])*TankVolume*SWDensity)/1000;
  #F_in -How much TA in coming in from headers (mmol m-2 hr-1)
  FinTA[i]<-((HeaderTA[i+1]+ HeaderTA[i])/2 *flowrate[i])/1000;
  
  #F_out-How much TA is leaving the tank (mmol m-2 hr-1)
  FoutTA[i]<-((TankTA[i+1]+TankTA[i])/2*flowrate[i])/1000;
  
  #Net ecosystem calcification (mmol m-2 hr-1) due to the critters in tank
  NEC[i]<-12*(FinTA[i]-FoutTA[i]-DTAdt[i])/2; #the 12 makes it per day
  }
  return(NEC)
  #NEC calc calculated the net ecosystem calcification of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) rather than change in TA over time (Eulerian-- this is what I used for the 
  #biogeochemistry paper)
  #NEC is in mmol m^-2 d-1
  
  #HeaderTA is TA from the header in umol/kg
  #TankTAis TA from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in kg/cm3 (default =1.023)
  
}  


NCPCalc2<-function(HeaderDIC,TankDIC,flow,SurfaceArea, TankVolume=5.678,SWDensity=1.023, time=3, NEC){
  
  tankarea <- 0.2032*0.22225; #m length x width of the tank (m2)
  #flow is in ml/min convert to L/hr
  flow<-flow*60/1000;
  
  #time is time in between sampling points
  #SA is in cm^3, convert to m^3
  SurfaceArea<-SurfaceArea/(100^2);
  
  
  #now convert flow rate to kg m-2 hr-1 so that I can use Andersson et al. 2009 eq 
  flowrate<-flow*SWDensity/tankarea;
  
  #DeltaTA/dt total change in TA in the tank over sample times (mmol m-2 hr-1)
  DTAdt<-NULL
  FinTA<-NULL
  FoutTA<-NULL
  NCP1<-NULL
  NCP<-NULL
  for (i in 1:length(TankDIC)-1){
    DTAdt[i]<-((TankDIC[i+1]-TankDIC[i])/(time*SurfaceArea[i])*TankVolume*SWDensity)/1000;
    #F_in -How much TA in coming in from headers (mmol m-2 hr-1)
    FinTA[i]<-((HeaderDIC[i+1]+ HeaderDIC[i]) *flowrate[i])/1000;
    
    #F_out-How much TA is leaving the tank (mmol m-2 hr-1)
    FoutTA[i]<-((TankDIC[i+1]+TankDIC[i])*flowrate[i])/1000;
    
    #Net ecosystem calcification (mmol m-2 hr-1) due to the critters in tank
    NCP1[i]<-12*(FinTA[i]-FoutTA[i]-DTAdt[i]); #the 12 makes it per day
    NCP[i]<-NCP1[i]-NEC[i]
    }
  return(NCP)
  #NEC calc calculated the net ecosystem calcification of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) rather than change in TA over time (Eulerian-- this is what I used for the 
  #biogeochemistry paper)
  #NEC is in mmol m^-2 d-1
  
  #HeaderTA is TA from the header in umol/kg
  #TankTAis TA from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in kg/cm3 (default =1.023)
  
}  


# Load Data-----------------------------------------
#Chem Data
ChemData<-read.csv('Data/AllChemData_noCorrect.csv') #read in 1st 12 columns only
ChemData<-ChemData[,-c(24,25)]
#Biology Data
#sourcing this doesn't work very well because or slow internet connections instead I will pull from the csv files created and 
#stored in the google drive folder
#source('CRANE-DataProcessing.R')

#Coral
Coral <- read.csv("../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/CoralSets_Rprocessed.csv",header=TRUE)

#Rubble
Rubble <- read.csv("../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Rubble_Rprocessed.csv",header=TRUE)

#Algae
Algae <- read.csv("../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Algae_Rprocessed.csv",header=TRUE)

#Sand
Sand<- read.csv("../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Sand_Rprocessed.csv",header=TRUE)


#Both the Tank TA and the header TA are already corrected for %off from CRM in the excel sheet

## Normalize the TA----------------------------------------------------------------------
#normalize the TA to N+N concentrations.  Right now we are using 0,3,6 as place holders until the real data get here
#I am also only normalizing the headers for nw because the N+N in the tanks are very low due to uptake

ChemData$HeaderTA.norm<-ChemData$HeaderTA-ChemData$HeaderN-ChemData$HeaderP
#ChemData$HeaderTA.norm<-ChemData$HeaderTA

#normalize the TA data to salinity---THE SALINITY DATA IS CAUSING PROBLEMS.....
#ChemData$HeaderTA.norm<-ChemData$HeaderTA*ChemData$HeaderSalinity/38
#ChemData$TankTA.norm<-ChemData$TankTA*ChemData$TankSalinity/38
ChemData$TankTA.norm<-ChemData$TankTA

#calculuate all the carbonate parameters with seacarb---------------------------------------
#Tanks
TankCO2<-carb(flag=8, ChemData$TankpH, ChemData$TankTA/1000000, S=ChemData$TankSalinity, T=ChemData$TankTemp, Patm=1, P=0, Pt=0, Sit=0,
     k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential")
#TA is divided by 1000 because all calculations are in mol/kg in the seacarb package

#convert CO2, HCO3, CO3, DIC, and Alk back to micromol for easier interpretation
TankCO2[,c("CO2","HCO3","CO3","DIC","ALK")]<-TankCO2[,c("CO2","HCO3","CO3","DIC","ALK")]*1000000

#add these data to the Dataframe
ChemData[,c("TankCO2","TankHCO3","TankCO3","TankDIC","TankOmegaArag","TankOmegaCalcite","TankpCO2","TankfCO2")]<-
 TankCO2[,c("CO2","HCO3","CO3","DIC","OmegaAragonite","OmegaCalcite","pCO2","fCO2")]

#headers
HeaderCO2<-carbb(flag=8, ChemData$HeaderpH, ChemData$HeaderTA/1000000, S=ChemData$HeaderSalinity, T=ChemData$HeaderTemp, Patm=1, P=0, Pt=0, Sit=0,
              k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential")
HeaderCO2[,c("CO2","HCO3","CO3","DIC","ALK")]<-HeaderCO2[,c("CO2","HCO3","CO3","DIC","ALK")]*1000000

ChemData[,c("HeaderCO2","HeaderHCO3","HeaderCO3","HeaderDIC","HeaderOmegaArag","HeaderOmegaCalcite","HeaderpCO2","HeaderfCO2")]<-
  HeaderCO2[,c("CO2","HCO3","CO3","DIC","OmegaAragonite","OmegaCalcite","pCO2","fCO2")]

## convert flow to residence time
#Flow is now in ml per min
TankVol<-5678 #hole was at 6 quarts is the volume of each tank

ChemData$ResTime<-(1/60)*(1/ChemData$Flow)*TankVol

#calculate the average residence time by aquarium for each experiment (1-36 is exp 1 and 37-72 is exp 2).
ResTime.mean <- ddply(ChemData, c("Aquarium"), summarize,
                          ResTime.mean = mean(ResTime, na.rm = T)
                        #  N=sum(!is.na(ResTime)),
                         # SE= sd(ResTime, na.rm = T)/sqrt(N)
                           )

#calculate the average flow rate by aquarium for each experiment (1-36 is exp 1 and 37-72 is exp 2).
Flow.mean <- ddply(ChemData, c("Aquarium"), summarize,
                      Flow.mean = mean(Flow, na.rm = T)
                      #  N=sum(!is.na(ResTime)),
                      # SE= sd(ResTime, na.rm = T)/sqrt(N)
)


## Sum up all the biological data by aquarium
Coral.Exp1Summary <- ddply(Coral, c("Aq_Ex1"), summarize,
                          SA = sum(SA, na.rm = T), #the SA for coral and algae are in mm2 while Sand is in cm2
                          AFDW = sum(AFDW, na.rm = T),
                          DW = sum(DW, na.rm = T),
                          Volume = sum(Volume, na.rm = T)
)

Coral.Exp2Summary <- ddply(Coral, c("Aq_Ex2"), summarize,
                           SA = sum(SA, na.rm = T),
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(Volume, na.rm = T)
)


Rubble.Exp1Summary <- ddply(Rubble, c("Aq_Ex1"), summarize,
                          SA = sum(SA, na.rm = T),
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(Volume, na.rm = T)
)

Rubble.Exp2Summary <- ddply(Rubble, c("Aq_Ex2"), summarize,
                            SA = sum(SA, na.rm = T),
                            AFDW = sum(AFDW, na.rm = T),
                            DW = sum(DW, na.rm = T),
                            Volume = sum(Volume, na.rm = T)
)

Algae.Exp1Summary <- ddply(Algae, c("Aq_Ex1"), summarize,
                            SA = sum(FinalSA, na.rm = T),
                            AFDW = sum(AFDW, na.rm = T),
                            DW = sum(DW, na.rm = T),
                            Volume = sum(FinalVol, na.rm = T)
)

Algae.Exp2Summary <- ddply(Algae, c("Aq_Ex2"), summarize,
                           SA = sum(FinalSA, na.rm = T),
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(FinalVol, na.rm = T)
)

Sand.Exp1Summary <- ddply(Sand, c("Aq_Ex1"), summarize,
                           SA = sum(SA, na.rm = T),
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(Vol, na.rm = T)
)

Sand.Exp2Summary <- ddply(Sand, c("Aq_Ex2"), summarize,
                          SA = sum(SA, na.rm = T),
                          AFDW = sum(AFDW, na.rm = T),
                          DW = sum(DW, na.rm = T),
                          Volume = sum(Vol, na.rm = T)
)
#join all the biology together
biology<-rbind(Rubble.Exp1Summary,Coral.Exp1Summary, Algae.Exp1Summary,Sand.Exp1Summary)

biology2<-rbind(Rubble.Exp2Summary,Coral.Exp2Summary, Algae.Exp2Summary,Sand.Exp2Summary)

Exp2biology <- ddply(biology2, c("Aq_Ex2"), summarize,
                            SA = sum(SA, na.rm = T),
                            AFDW = sum(AFDW, na.rm = T),
                            DW = sum(DW, na.rm = T),
                            Volume = sum(Volume, na.rm = T)
)

colnames(biology)[1]<-'Aquarium'
colnames(Exp2biology)[1]<-'Aquarium'

biology<-rbind(biology,Exp2biology)

#add the mean residence times
AllData<-merge(ChemData, ResTime.mean, all.x=TRUE)

#add the mean residence times
AllData<-merge(AllData, Flow.mean, all.x=TRUE)

#Make one huge dataset with all the biology and chem data together
AllData<-merge(AllData,biology, by='Aquarium', all.x=TRUE)

AllData$DateTime<-as.POSIXct(paste(AllData$Date, AllData$Time), format="%m/%d/%Y %H:%M")

##sort all the data by time, substrate, nutrient level, and experiment
AllData<-AllData[order(AllData$Experiment, AllData$DateTime, AllData$Substrate, AllData$NutLevel),]

# change the order of the factors for nutrient level so that it is ambient then medium then high

AllData$NutLevel <- factor(AllData$NutLevel, levels = c("Ambient", "Med", "High"))


#add a column for "Day" or "Night"
time<-unique(AllData$Time)
AllData$DayNight<-ifelse(AllData$Time==time[1]|AllData$Time==time[2]|AllData$Time==time[3], 'Day', 'Night')

#Calculate NEC---------------------------------------------

Nuts<-unique(AllData$NutLevel)[c(1,3,2)] #puts the order for loops as ambient, then med, then high
sub<-unique(AllData$Substrate)

#NEC2<-matrix(data=NA, nrow=6, ncol=3)
NEC2 <- array(NA, dim=c(6,72))
NCP2 <- array(NA, dim=c(6,72))
#sort data by aquarium so that I can calculate NEC easier

AllData<-AllData[order(AllData$Aquarium),]

#calculating NEC using eularian....I AM NOT USING THIS... WAS JUST A TEST
for (i in 1:length(unique(AllData$Aquarium))){
  NEC2[,i]<-NECCalc2(HeaderTA = AllData$HeaderTA.norm[AllData$Aquarium==i], 
                       TankTA = AllData$TankTA.norm[AllData$Aquarium==i], 
                       flow =  AllData$Flow.mean[AllData$Aquarium==i], 
                       SurfaceArea = AllData$SA[AllData$Aquarium==i])
  
  
}

#for (i in 1:length(unique(AllData$Aquarium))){
 # NCP2[,i]<-NCPCalc2(HeaderDIC = AllData$HeaderDIC.norm[AllData$Aquarium==i], 
  #                   TankDIC = AllData$TankDIC.norm[AllData$Aquarium==i], 
   #                  flow =  AllData$Flow.mean[AllData$Aquarium==i], 
    #                 SurfaceArea = AllData$SA[AllData$Aquarium==i],
     #                NEC=NEC2[,i])
  
  
#}

#NEC using lagrangian method with AFDW normalization--THIS IS WHAT i AM USING
  AllData$NEC.AFDW<-NECCalc(HeaderTA = AllData$HeaderTA.norm, 
                    TankTA = AllData$TankTA.norm, 
                    ResidenceTime = AllData$ResTime.mean, 
                    SurfaceArea = AllData$AFDW)

  
  #NEC using lagrangian method with dry weight normalization
  AllData$NEC.DW<-NECCalc(HeaderTA = AllData$HeaderTA.norm, 
                            TankTA = AllData$TankTA.norm, 
                            ResidenceTime = AllData$ResTime.mean, 
                            SurfaceArea = AllData$DW)
  
  #NEC using lagrangian method with volume normalization
  AllData$NEC.Vol<-NECCalc(HeaderTA = AllData$HeaderTA.norm, 
                            TankTA = AllData$TankTA.norm, 
                            ResidenceTime = AllData$ResTime.mean, 
                            SurfaceArea = AllData$Volume)  
  
  #NEC using lagrangian method with SA normalization
  AllData$NEC.SA<-NECCalc(HeaderTA = AllData$HeaderTA.norm, 
                           TankTA = AllData$TankTA.norm, 
                           ResidenceTime = AllData$ResTime.mean, 
                           SurfaceArea = AllData$SA) 


  #plot the different NEC calcs versus each other
  par(mfrow=c(2,2))
  plot(AllData$NEC.SA, AllData$NEC.AFDW, col=AllData$Substrate, xlab='SA', ylab='AFDW', main='NEC')
  legend('topright', legend=unique(AllData$Substrate), col=unique(AllData$Substrate), pch=19, bty = 'n')
  plot(AllData$NEC.Vol, AllData$NEC.DW, col=AllData$Substrate, xlab='Volume', ylab='DW')
  plot(AllData$NEC.Vol, AllData$NEC.SA, col=AllData$Substrate, xlab='Volume', ylab='SA')
  plot(AllData$NEC.DW, AllData$NEC.AFDW, col=AllData$Substrate, xlab='Dry Weight', ylab='AFDW')
  

  ###NCP calcs----------------------------------------------
  ## Normalize the DIC Data to a constant salinity f
  AllData$TankDIC.norm<-AllData$TankDIC
  AllData$HeaderDIC.norm<-AllData$HeaderDIC
  
  #AllData$TankDIC.norm<-AllData$TankDIC*AllData$TankSalinity/38
  #AllData$HeaderDIC.norm<-AllData$HeaderDIC*AllData$HeaderSalinity/38
  
  
  #NCP using lagrangian method with AFDW normalization
  AllData$NCP.AFDW<-NCPCalc(HeaderDIC = AllData$HeaderDIC.norm, 
                            TankDIC = AllData$TankDIC.norm, 
                            ResidenceTime = AllData$ResTime.mean, 
                            SurfaceArea = AllData$AFDW,
                            NEC = AllData$NEC.AFDW)
  
  
  #NCP using lagrangian method with dry weight normalization
  AllData$NCP.DW<-NCPCalc(HeaderDIC = AllData$HeaderDIC.norm, 
                            TankDIC = AllData$TankDIC.norm, 
                            ResidenceTime = AllData$ResTime.mean, 
                            SurfaceArea = AllData$DW,
                            NEC = AllData$NEC.DW)
  
  #NCP using lagrangian method with volume normalization
  AllData$NCP.Vol<-NCPCalc(HeaderDIC = AllData$HeaderDIC.norm, 
                            TankDIC = AllData$TankDIC.norm, 
                            ResidenceTime = AllData$ResTime.mean, 
                            SurfaceArea = AllData$Vol,
                            NEC = AllData$NEC.Vol) 
  
  #NEC using lagrangian method with SA normalization
  AllData$NCP.SA<-NCPCalc(HeaderDIC = AllData$HeaderDIC.norm, 
                           TankDIC = AllData$TankDIC.norm, 
                           ResidenceTime = AllData$ResTime.mean, 
                           SurfaceArea = AllData$SA,
                           NEC = AllData$NEC.SA) 
  
  
  
  
  #plot the different NCP calcs versus each other
  par(mfrow=c(2,2))
  plot(AllData$NCP.SA, AllData$NCP.AFDW, col=AllData$Substrate, xlab='SA', ylab='AFDW', main='NCP')
  legend('topright', legend=unique(AllData$Substrate), col=unique(AllData$Substrate), pch=19, bty = 'n')
  plot(AllData$NCP.Vol, AllData$NCP.DW, col=AllData$Substrate, xlab='Volume', ylab='DW')
  plot(AllData$NCP.Vol, AllData$NCP.SA, col=AllData$Substrate, xlab='Volume', ylab='SA')
  plot(AllData$NCP.DW, AllData$NCP.AFDW, col=AllData$Substrate, xlab='Dry Weight', ylab='AFDW')
  
  
  #### calculate means------------------

#sub <- sub[sub!="Mixed"]
#sub<-droplevels(sub)

  #calcification mean by substrate, treatment, and sampling time--- this just takes the means over the replicate aquaria
NEC.mean <- ddply(AllData, c("Substrate","NutLevel","DateTime"), summarize,
                     Mean.AFDW = mean(NEC.AFDW, na.rm = T),
                     N=sum(!is.na(NEC.AFDW)),
                     SE.AFDW= sd(NEC.AFDW, na.rm = T)/sqrt(N),
                     Mean.SA = mean(NEC.SA, na.rm = T),
                     SE.SA= sd(NEC.SA, na.rm = T)/sqrt(N),
                     Mean.DW = mean(NEC.DW, na.rm = T),
                     SE.DW= sd(NEC.DW, na.rm = T)/sqrt(N),
                     Mean.Vol = mean(NEC.Vol, na.rm = T),
                     SE.Vol= sd(NEC.Vol, na.rm = T)/sqrt(N)
)
#production
  NCP.mean <- ddply(AllData, c("Substrate","NutLevel","DateTime"), summarize,
                    Mean.AFDW = mean(NCP.AFDW, na.rm = T),
                    N=sum(!is.na(NCP.AFDW)),
                    SE.AFDW= sd(NCP.AFDW, na.rm = T)/sqrt(N),
                    Mean.SA = mean(NCP.SA, na.rm = T),
                    SE.SA= sd(NCP.SA, na.rm = T)/sqrt(N),
                    Mean.DW = mean(NCP.DW, na.rm = T),
                    SE.DW= sd(NCP.DW, na.rm = T)/sqrt(N),
                    Mean.Vol = mean(NCP.Vol, na.rm = T),
                    SE.Vol= sd(NCP.Vol, na.rm = T)/sqrt(N)
  )
  
    
par(mfrow=c(2,2))
for (i in 1:5){
plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i])
points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
}

##Plot NEC averages across time for each normalization-------------------------------------------------------
##NORMALIZED BY AFDW
par(mfrow=c(3,2))
cols <- c(unique(NEC.mean$NutLevel))
y<-NEC.mean$Mean.AFDW
yse<-NEC.mean$SE.AFDW
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+5), ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NEC.mean$DateTime [NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]),
         y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+5))
    
    arrows(unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           + yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           - yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
     
  }
  axis(1, at=unique(NEC.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NEC.mean$DateTime)[a],min(y),unique(NEC.mean$DateTime)[b],max(y)+5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NEC.mean$NutLevel), col=unique(NEC.mean$NutLevel), pch=19, bty = 'n')

##NORMALIZED BY SA
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NEC.mean$Mean.SA
yse<-NEC.mean$SE.SA
cols <- c(unique(NEC.mean$NutLevel))
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)), ylab=expression(paste("NEC ",mu,"mol cm"^{-2}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NEC.mean$DateTime [NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]),
         y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)))
    
    arrows(unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           + yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           - yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NEC.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NEC.mean$DateTime)[a],min(y),unique(NEC.mean$DateTime)[b],max(y),col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NEC.mean$NutLevel), col=unique(NEC.mean$NutLevel), pch=19, bty = 'n')

##Volume
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NEC.mean$Mean.Vol
yse<-NEC.mean$SE.Vol
#cols <- c(unique(NEC.mean$NutLevel))
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)), ylab=expression(paste("NEC ",mu,"mol cm"^{-3}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NEC.mean$DateTime [NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]),
         y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)))
    
    arrows(unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           + yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           - yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NEC.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NEC.mean$DateTime)[a],min(y),unique(NEC.mean$DateTime)[b],max(y),col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NEC.mean$NutLevel), col=unique(NEC.mean$NutLevel), pch=19, bty = 'n')

##DW
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NEC.mean$Mean.DW
yse<-NEC.mean$SE.DW
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)), ylab=expression(paste("NEC ",mu,"mol g DW"^{-1}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  #cols <- c(unique(NEC.mean$NutLevel))
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NEC.mean$DateTime [NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]),
         y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)))
    
    arrows(unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           + yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           unique(NEC.mean$DateTime), y[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]]
           - yse[NEC.mean$Substrate==sub[i] & NEC.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NEC.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NEC.mean$DateTime)[a],min(y),unique(NEC.mean$DateTime)[b],max(y),col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NEC.mean$NutLevel), col=unique(NEC.mean$NutLevel), pch=19, bty = 'n')

#-------------------------------------------------------------------------------
##Plot NCP averages across time for each normalization
##NORMALIZED BY AFDW
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NCP.mean$Mean.AFDW
yse<-NCP.mean$SE.AFDW
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+5), ylab=expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
#  cols <- unique(NCP.mean$NutLevel)
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NCP.mean$DateTime [NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]),
         y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+5))
    
    arrows(unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           + yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           - yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NCP.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NCP.mean$DateTime)[a],min(y),unique(NCP.mean$DateTime)[b],max(y)+5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NCP.mean$NutLevel), col=unique(NCP.mean$NutLevel), pch=19, bty = 'n')

##NORMALIZED BY SA
par(mfrow=c(3,2))

rm(y)
rm(yse)

y<-NCP.mean$Mean.SA
yse<-NCP.mean$SE.SA

for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+0.5), ylab=expression(paste("NCP ",mu,"mol cm"^{-2}," hr"^{-1})), main = sub[i])
  
 
  par(new = TRUE)
  abline(h=0)
#  cols <- unique(NCP.mean$NutLevel)
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
   
    
    plot(as.numeric(NCP.mean$DateTime [NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]),
         y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.5))
    
    arrows(unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           + yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           - yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NCP.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NCP.mean$DateTime)[a],min(y),unique(NCP.mean$DateTime)[b],max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NCP.mean$NutLevel), col=unique(NCP.mean$NutLevel), pch=19, bty = 'n')

##NORMALIZED BY Vol
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NCP.mean$Mean.Vol
yse<-NCP.mean$SE.Vol
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+0.5), ylab=expression(paste("NCP ",mu,"mol cm"^{-3}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
 # cols <- unique(NCP.mean$NutLevel)
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NCP.mean$DateTime [NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]),
         y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.5))
    
    arrows(unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           + yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           - yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NCP.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NCP.mean$DateTime)[a],min(y),unique(NCP.mean$DateTime)[b],max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NCP.mean$NutLevel), col=unique(NCP.mean$NutLevel), pch=19, bty = 'n')

#Normalized by DW
par(mfrow=c(3,2))
rm(y)
rm(yse)
y<-NCP.mean$Mean.DW
yse<-NCP.mean$SE.DW
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+0.5), ylab=expression(paste("NCP ",mu,"mol g DW"^{-1}," hr"^{-1})), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  #cols <- unique(NCP.mean$NutLevel)
  #
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(NCP.mean$DateTime [NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]),
         y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.5))
    
    arrows(unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           + yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           unique(NCP.mean$DateTime), y[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]]
           - yse[NCP.mean$Substrate==sub[i] & NCP.mean$NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  }
  axis(1, at=unique(NCP.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(NCP.mean$DateTime)[a],min(y),unique(NCP.mean$DateTime)[b],max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
}
legend('topright', legend=unique(NCP.mean$NutLevel), col=unique(NCP.mean$NutLevel), pch=19, bty = 'n')


##----------------------------------------------------------------------
#calculate daily average per substrate and nutrient
#add a day night column
time<-unique(NEC.mean$DateTime)
NEC.mean$DayNight<-ifelse(NEC.mean$DateTime==time[1]|NEC.mean$DateTime==time[2]|NEC.mean$DateTime==time[3]|
                            NEC.mean$DateTime==time[7]|NEC.mean$DateTime==time[8]|
                            NEC.mean$DateTime==time[9]|NEC.mean$DateTime==time[10]|NEC.mean$DateTime==time[11]|
                            NEC.mean$DateTime==time[14], 'Day', 'Night')
NCP.mean$DayNight<-NEC.mean$DayNight

#summarize across all the different times
NEC.mean.Net <- ddply(NEC.mean, c("Substrate","NutLevel"), summarize,
                  Mean.AFDW2 = mean(Mean.AFDW, na.rm = T),
                  N2=sum(!is.na(Mean.AFDW)),
                  SE.AFDW2= sd(Mean.AFDW, na.rm = T)/sqrt(N2),
                  Mean.SA2 = mean(Mean.SA, na.rm = T),
                  SE.SA2= sd(Mean.SA, na.rm = T)/sqrt(N2),
                  Mean.DW2 = mean(Mean.DW, na.rm = T),
                  SE.DW2= sd(Mean.DW, na.rm = T)/sqrt(N2),
                  Mean.Vol2 = mean(Mean.Vol, na.rm = T),
                  SE.Vol2= sd(Mean.Vol, na.rm = T)/sqrt(N2)
)

NCP.mean.Net <- ddply(NCP.mean, c("Substrate","NutLevel"), summarize,
                      Mean.AFDW2 = mean(Mean.AFDW, na.rm = T),
                      N2=sum(!is.na(Mean.AFDW)),
                      SE.AFDW2= sd(Mean.AFDW, na.rm = T)/sqrt(N2),
                      Mean.SA2 = mean(Mean.SA, na.rm = T),
                      SE.SA2= sd(Mean.SA, na.rm = T)/sqrt(N2),
                      Mean.DW2 = mean(Mean.DW, na.rm = T),
                      SE.DW2= sd(Mean.DW, na.rm = T)/sqrt(N2),
                      Mean.Vol2 = mean(Mean.Vol, na.rm = T),
                      SE.Vol2= sd(Mean.Vol, na.rm = T)/sqrt(N2)
)
#NEC.mean.Net <- ddply(AllData, c("Substrate","NutLevel"), summarize,
 #                     Mean.AFDW2 = mean(NEC.AFDW, na.rm = T),
  #                    N2=sum(!is.na(NEC.AFDW)),
  #                    SE.AFDW2= sd(NEC.AFDW, na.rm = T)/sqrt(N2),
   #                   Mean.SA2 = mean(NEC.SA, na.rm = T),
    #                  SE.SA2= sd(NEC.SA, na.rm = T)/sqrt(N2),
     #                 Mean.DW2 = mean(NEC.DW, na.rm = T),
      #                SE.DW2= sd(NEC.DW, na.rm = T)/sqrt(N2),
       #               Mean.Vol2 = mean(NEC.Vol, na.rm = T),
                  #    SE.Vol2= sd(NEC.Vol, na.rm = T)/sqrt(N2)
#)


#calculate averages for day and night
NEC.mean.DayNight <- ddply(NEC.mean, c("Substrate","NutLevel", "DayNight"), summarize,
                      Mean.AFDW2 = mean(Mean.AFDW, na.rm = T),
                      N2=sum(!is.na(Mean.AFDW)),
                      SE.AFDW2= sd(Mean.AFDW, na.rm = T)/sqrt(N2),
                      Mean.SA2 = mean(Mean.SA, na.rm = T),
                      SE.SA2= sd(Mean.SA, na.rm = T)/sqrt(N2),
                      Mean.DW2 = mean(Mean.DW, na.rm = T),
                      SE.DW2= sd(Mean.DW, na.rm = T)/sqrt(N2),
                      Mean.Vol2 = mean(Mean.Vol, na.rm = T),
                      SE.Vol2= sd(Mean.Vol, na.rm = T)/sqrt(N2)
)

NCP.mean.DayNight <- ddply(NCP.mean, c("Substrate","NutLevel", "DayNight"), summarize,
                      Mean.AFDW2 = mean(Mean.AFDW, na.rm = T),
                      N2=sum(!is.na(Mean.AFDW)),
                      SE.AFDW2= sd(Mean.AFDW, na.rm = T)/sqrt(N2),
                      Mean.SA2 = mean(Mean.SA, na.rm = T),
                      SE.SA2= sd(Mean.SA, na.rm = T)/sqrt(N2),
                      Mean.DW2 = mean(Mean.DW, na.rm = T),
                      SE.DW2= sd(Mean.DW, na.rm = T)/sqrt(N2),
                      Mean.Vol2 = mean(Mean.Vol, na.rm = T),
                      SE.Vol2= sd(Mean.Vol, na.rm = T)/sqrt(N2))

#PR
NCP.mean.PRbyTank <- ddply(AllData, c("Substrate","NutLevel", "Tank", "Aquarium"), summarize,
                     #Mean.AFDW2.p = mean(NCP.AFDW[NCP.AFDW>0], na.rm=T),
                     Mean.AFDW2.p = mean(NCP.AFDW[DayNight=='Day'], na.rm=T),
                     Mean.AFDW2.r =mean(abs(NCP.AFDW[DayNight=='Night']), na.rm = T),       
                     GPP=Mean.AFDW2.p+Mean.AFDW2.r,
                     PR.AFDW2=GPP/Mean.AFDW2.r,
                     Mean.AFDW2.Day = mean(NEC.AFDW[DayNight=='Day'], na.rm=T), #day NEC
                     Mean.AFDW2.Night = mean(NEC.AFDW[DayNight=='Night'], na.rm=T) #Night NEC
                     #N2=sum(!is.na(Mean.AFDW2.p)),
                     #SE.AFDW2.pr=sd(PR.AFDW2)/sqrt(N2)
                     )

#NCP.mean.PRbyTank[is.na(NCP.mean.PRbyTank)]<-0 #replace the NAs with 0

NCP.mean.PR<-ddply(NCP.mean.PRbyTank, c("Substrate","NutLevel"), summarize,
                Mean.AFDW2=mean(PR.AFDW2, na.rm=T),
                Mean.GPP=mean(GPP, na.rm=T),
                N2=3,
                SE.AFDW2=sd(PR.AFDW2)/sqrt(N2),
                SE.GPP=sd(GPP)/sqrt(N2)
)


#NEC plots net
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]], main=sub[i], ylim=c(-5,15), 
             ylab=expression(paste("Net NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]],0,NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  lines(x,c(0,0,0))
}

#NEC plots by day
y2<-c(20,10)
DN<-c('Day','Night')
for (j in 1:2){
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NEC.mean.DayNight$Mean.AFDW2[NEC.mean.DayNight$Substrate==sub[i] & NEC.mean.DayNight$DayNight==DN[j]], main=sub[i], ylim=c(-2,y2[j]), xlab = DN[j],
             ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NEC.mean.DayNight$Mean.AFDW2[NEC.mean.DayNight$Substrate==sub[i]& NEC.mean.DayNight$DayNight==DN[j]],0,NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==sub[i]& NEC.mean.DayNight$DayNight==DN[j]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  lines(x,c(0,0,0))
}  
}


#NCP net

par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.Net$Mean.AFDW2[NCP.mean.Net$Substrate==sub[i]], main=sub[i], ylim=c(-10,25),
             ylab=expression(paste("Net NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NCP.mean.Net$Mean.AFDW2[NCP.mean.Net$Substrate==sub[i]],0,NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  lines(x,c(0,0,0))
}

#NCP day and night
y1<-c(0,-25)
y2<-c(35,0)
for (j in 1:2){
  par(mfrow=c(3,2))
  for (i in 1:length(sub)){
    x<-barplot(NCP.mean.DayNight$Mean.AFDW2[NCP.mean.DayNight$Substrate==sub[i] & NCP.mean.DayNight$DayNight==DN[j]], main=sub[i], ylim=c(y1[j],y2[j]), xlab = DN[j],
               ylab=expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
    errorbars(x,NCP.mean.DayNight$Mean.AFDW2[NCP.mean.DayNight$Substrate==sub[i]& NCP.mean.DayNight$DayNight==DN[j]],0,NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==sub[i]& NCP.mean.DayNight$DayNight==DN[j]])
    axis(1, at=x, labels=c("Ambeint","Medium","High"))
    lines(x,c(0,0,0))
  }  
}

#GPP: add dark respiration to light net photosynthesis to get gross photosynthesis
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]], main=sub[i], ylim=c(0,max(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]])+2),
             ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]],0,NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate==sub[i]])
            
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  
}

#P/R
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]], main=sub[i], ylim=c(0,max(NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]])+2), ylab = 'Mean P/R')
  errorbars(x,NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]],0,NCP.mean.PR$SE.AFDW2[NCP.mean.PR$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  
}

## find % of time calcifying  or dissolving
#put a 1 next to times that are calcifying and a 0 for times dissolving
AllData$CD<-ifelse(AllData$NEC.AFDW>0,1,0)

PercentCalc<-ddply(AllData, c("Substrate","NutLevel", "Tank"), summarize,
                   N = sum(!is.na(CD)),
                   PercentCalc = sum(CD)/N, #percent of the time calcifying
                   PercentDis = 1-PercentCalc) #percent of the time dissolving
#average across tanks
PercentCalc.mean<-ddply(PercentCalc, c("Substrate","NutLevel"), summarize,
                        Mean.PercentCalc = mean(PercentCalc, na.rm = T),
                        N2=sum(!is.na(PercentCalc)),
                        SE.PercentCalc= sd(PercentCalc, na.rm = T)/sqrt(N2),
                        Mean.PercentDis = mean(PercentDis, na.rm = T),
                        SE.PercentDis= sd(PercentDis, na.rm = T)/sqrt(N2))

par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(PercentCalc.mean$Mean.PercentCalc[PercentCalc.mean$Substrate==sub[i]], main=sub[i], ylim=c(0,1), ylab = 'Mean % Calc')
  errorbars(x,PercentCalc.mean$Mean.PercentCalc[PercentCalc.mean$Substrate==sub[i]],0,PercentCalc.mean$SE.PercentCalc[PercentCalc.mean$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  
}

par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(PercentCalc.mean$Mean.PercentDis[PercentCalc.mean$Substrate==sub[i]], main=sub[i], ylim=c(0,1), ylab = 'Mean % Dissolution')
  errorbars(x,PercentCalc.mean$Mean.PercentDis[PercentCalc.mean$Substrate==sub[i]],0,PercentCalc.mean$SE.PercentDis[PercentCalc.mean$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  
}
## Stats-------------------------------------
#I took the average across times then ran GLMs for each inidividual substrate using black tank as a random effect

#NEC net

#Take the average of everytihng across aquariums so its easier to code in the model.  I need to drop the factors for that to work
AllData2<-AllData
levels(AllData2$Substrate)<-c(1:5)
AllData2$Substrate<-as.numeric(droplevels(AllData2$Substrate))
levels(AllData2$NutLevel)<-c(1:3)
AllData2$NutLevel<-as.numeric(droplevels(AllData2$NutLevel))

Mean.Time<-aggregate(AllData2, list(AllData2$Aquarium), mean)
#now add the factors back for the GLM
Mean.Time$NutLevel<-as.factor(Mean.Time$NutLevel)
levels(Mean.Time$NutLevel)<-c('Ambient','Med','High')
Mean.Time$Substrate<-as.factor(Mean.Time$Substrate)
levels(Mean.Time$Substrate)<-c('Algae','Coral','Mixed','Rubble','Sand')


#add mean respiration rates by aquarium to all data so that I can add it to the photosynthesis
#to get GPP across each time point

AllData <-merge(AllData, NCP.mean.PRbyTank[,c(4,6)], all.x=TRUE, by.x = "Aquarium")
AllData$GPP<-AllData$NCP.AFDW+AllData$Mean.AFDW2.r

#Algae
##how to do the model as a repeated measures anova for both time and aquarium
#model.NECNight.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Night',]) 

#AlgaeMean<-Mean.Time[Mean.Time$Substrate=='Algae',]
#model.NECNet.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Algae',]) #Net NEC mean across 24 hours
#model.NECDay.algae<-lmer(Mean.AFDW2.Day~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Algae',]) # day calcification
#model.NECNight.algae<-lmer(Mean.AFDW2.Night~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Algae',]) # night calcification
#model.NCPNet.algae<-lmer(NCP.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Algae',]) # Net NCP mean across 24 hours
#model.GCP.algae<-lmer(GPP~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Algae',]) # GCP light +dark photosynthesi
#model.R.algae<-lmer(Mean.AFDW2.r~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Algae',]) # dark respiration

## HOW DO I RUN THESE SAME MODELS FOR GPP AND HOLD THE POWER??? ONE IDEA: ADD AVERAGE RESPIRATION TO ALL THE DAY RATES

model.NECNet.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' ,]) 
model.NECDay.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Day',]) 
model.NECNight.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Night',]) 
model.NCPNet.algae<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' ,]) 
model.R.algae<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Night',]) 
model.GCP.algae<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Day',]) 


#Coral
#CoralMean<-Mean.Time[Mean.Time$Substrate=='Coral',]
#model.NECNet.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Coral',])
#model.NECDay.Coral<-lmer(Mean.AFDW2.Day~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Coral',]) 
#model.NECNight.Coral<-lmer(Mean.AFDW2.Night~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Coral',]) 
#model.NCPNet.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Coral',])
#model.GCP.Coral<-lmer(GPP~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Coral',])
#model.R.Coral<-lmer(Mean.AFDW2.r~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Coral',]) # dark respiration

model.NECNet.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank) +(1|DateTime), data=AllData[AllData$Substrate=='Coral' ,]) 
model.NECDay.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Day',]) 
model.NECNight.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Night',]) 
model.NECNet.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' ,]) 
model.R.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Night',]) 
model.GCP.Coral<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Day',]) 



#Rubble
#RubbleMean<-Mean.Time[Mean.Time$Substrate=='Rubble',]
#model.NECNet.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Rubble',])
#model.NECDay.Rubble<-lmer(Mean.AFDW2.Day~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Rubble',]) 
#model.NECNight.Rubble<-lmer(Mean.AFDW2.Night~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Rubble',]) 
#model.NCPNet.Rubble<-lmer(NCP.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Rubble',])
#model.GCP.Rubble<-lmer(GPP~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Rubble',])
#model.R.Rubble<-lmer(Mean.AFDW2.r~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Rubble',]) # dark respiration


model.NECNet.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' ,]) 
model.NECDay.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Day',]) 
model.NECNight.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Night',]) 
model.NCPNet.Rubble<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' ,]) 
model.R.Rubble<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Night',]) 
model.GCP.Rubble<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Day',]) 

#Sand
#SandMean<-Mean.Time[Mean.Time$Substrate=='Sand',]
#model.NECNet.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Sand',])
#model.NECDay.Sand<-lmer(Mean.AFDW2.Day~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Sand',]) 
#model.NECNight.Sand<-lmer(Mean.AFDW2.Night~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Sand',]) 
#model.NCPNet.Sand<-lmer(NCP.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Sand',])
#model.GCP.Sand<-lmer(GPP~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Sand',])
#model.R.Sand<-lmer(Mean.AFDW2.r~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Sand',]) # dark respiration

model.NECNet.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' ,]) 
model.NECDay.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Day',]) 
model.NECNight.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Night',]) 
model.NCPNet.Sand<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' ,]) 
model.R.Sand<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Night',]) 
model.GCP.Sand<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Day',]) 


#Mixed
#model.NECNet.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Mixed',])
#model.NECDay.Mixed<-lmer(Mean.AFDW2.Day~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Mixed',]) 
#model.NECNight.Mixed<-lmer(Mean.AFDW2.Night~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Mixed',]) 
#model.NCPNet.Mixed<-lmer(NCP.AFDW~NutLevel +(1|Tank), data=Mean.Time[Mean.Time$Substrate=='Mixed',])
#model.GCP.Mixed<-lmer(GPP~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Mixed',])
#model.R.Mixed<-lmer(Mean.AFDW2.r~NutLevel +(1|Tank), data=NCP.mean.PRbyTank[NCP.mean.PRbyTank$Substrate=='Mixed',]) # dark respiration

model.NECNet.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' ,]) 
model.NECDay.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Day',]) 
model.NECNight.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Night',]) 
model.NCPNet.Mixed<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' ,]) 
model.R.Mixed<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Night',]) 
model.GCP.Mixed<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Day',]) 

#omega vs NEC------------------- varying intercepts for aquarium within black tank and varying slopes (and intercept) by nutrient level

model.NECOmega.Algae<-lmer(NEC.AFDW~ (1+TankOmegaArag|NutLevel)+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Algae',])

model.NECOmega.Coral<-lmer(NEC.AFDW~ (1+TankOmegaArag|NutLevel)+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Coral',])
#coef(model.NECOmega.Coral) #get the coefficients

model.NECOmega.Sand<-lmer(NEC.AFDW~ (1+TankOmegaArag|NutLevel)+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Sand',])

model.NECOmega.Rubble<-lmer(NEC.AFDW~ (1+TankOmegaArag|NutLevel)+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Rubble',])

model.NECOmega.Mixed<-lmer(NEC.AFDW~ (1+TankOmegaArag|NutLevel)+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Mixed',])

# models for pH vs NCP
 model.pH.NCP<-lm(AllData$TankpH~AllData$NCP.AFDW)
##TA vs DIC plots--------------------------------------------
b<-matrix(nrow=5,ncol=3)
b0<-matrix(nrow=5,ncol=3)
r2<-matrix(nrow=5,ncol=3)
substrate<-matrix(nrow=5,ncol=3)
Nut<-matrix(nrow=5,ncol=3)
colors<-c('white','blue','magenta')

##make a contour plot for Deffey diagram
AT <- seq(2000e-6, 2341e-6, length.out=10)
DIC <- seq(1492e-6, 2300e-6, length.out=10)
dat <- expand.grid(AT, DIC)

carb <- carb(flag=15, var1=,dat$Var1, var2=dat$Var2, S=35, T=25, P=0, Pt=0, Sit=0, k1k2="l", kf="pf", pHscale="T")

arag <- carb$OmegaAragonite
dim(arag) <- c(length(DIC), length(AT)) # 

#contour(DIC, AT, arag)
#THE ARAGNOATITE SATURATION STATE CONTOURS ARE WRONG
arag<-arag[c(10,9,8,7,6,5,4,3,2,1),]
arag<-arag[,c(10,9,8,7,6,5,4,3,2,1)]

par(mfrow=c(1,1))
for(i in 1:5){
  
  #plot(NA, ylim=c(2100,2200),xaxt="n", xlab="DIC", ylab="TA", main = sub[i])
  #par(new = TRUE)
  filled.contour(DIC*1e6, AT*1e6, arag, 
                 xlab="DIC",
                 ylab="Total alkalinity",
                 key.title = title(main = expression(paste(Omega[arg]))),
                 #levels=seq(0, 9, by=0.25), 
                 levels = pretty(c(0,9), 50),
                 #labcex=1.5,
                 #method="edge",
                 col = rainbow(50),
                 lwd=2,
                 lty="solid",
                 main=sub[i]
                    
  )
   #calcification dissolution line
  x.DIC<-seq(from=2050, to= 2080, by  = 1)
  y.TA<-2*x.DIC-1840
  lines(x.DIC,y.TA, col='black', lwd=3)
  arrows(min(x.DIC),min(y.TA),max(x.DIC), max(y.TA), col='black', code=3,lwd=3,length=0.1)
  text(max(x.DIC)-25, max(y.TA)+8, 'Calcification', cex=0.5)
  text(min(x.DIC)-15, min(y.TA)-10, 'Dissolution', cex=0.5)
  #photosynthesis/respiration line
  x.DIC<-seq(from=2015, to= 2110, by  = 1)
  y.TA<-0.16*x.DIC +1955
  lines(x.DIC,y.TA, col='black', lwd=3)
  arrows(min(x.DIC),min(y.TA),max(x.DIC), max(y.TA), col='black', code=3,lwd=3, length=0.1)
  text(max(x.DIC)-1, max(y.TA)+10, 'Photosynthesis', cex=0.5)
  text(min(x.DIC)-15, min(y.TA)-10, 'Respiration', cex=0.5)
    #abline(h=0)
   # par(new = TRUE)
    #plot(AllData$TankDIC[ AllData$Substrate==sub[i]],AllData$TankTA[ AllData$Substrate==sub[i]], main=sub[i], 
     #     type = 'p', col = colors[j], pch=19, yaxt="n", ylab="", xlab="")
    for (j in 1:3){
      #par(new = TRUE)
    points(AllData$TankDIC.norm[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]],AllData$TankTA.norm[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]], main=sub[i], 
           type = 'p', col = colors[j], pch=19)
    
    
    modelTA.DIC<-lm(AllData$TankTA.norm[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]]  ~AllData$TankDIC.norm[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]])
    b0[i,j]<-modelTA.DIC$coefficients[1]
    b[i,j]<-modelTA.DIC$coefficients[2]
    r2[i,j]<-summary(modelTA.DIC)$r.squared
    substrate[i,j]<-sub[i]
    Nut[i,j]<-Nuts[j]
    x<-seq(from=1500, to=2100,by=0.5)
    y<-b[i,j]*x+b0[i,j]
    lines(x,y, col=colors[j], lwd=3)
    
    
    
    }
legend('topleft',legend=c('Ambient',"Medium","High"), col=c('blue','magenta','white'), pch=19, bty = 'n')
}


###Looking at feedbacks--------------------------------------------

plot(AllData$NCP.AFDW, AllData$TankpH, col=AllData$NutLevel, xlab='NCP', ylab='pH')
legend('topleft',legend=c('Ambient',"Medium","High"), col=unique(AllData$NutLevel), pch=19, bty = 'n')

plot(AllData$TankOmegaArag, AllData$NEC.AFDW, col=AllData$NutLevel)

pdf("NECvsOmega.pdf", width=6, height=8)
par(mfrow=c(3,2))
cols <- c(unique(NEC.mean$NutLevel))
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
for (i in 1:length(sub)){
  plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
       ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), main = sub[i], xlim=c(1.5,6))
  
  abline(h=0, lty=2)
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]],
         y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = cols[j],
         pch=21, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])))
    
   model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
    lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3)
  }
}
  legend('top', horiz = TRUE, legend=unique(NEC.mean$NutLevel), col=unique(NEC.mean$NutLevel), pch=19, bty = 'n')

dev.off()


## pH plots----------------------------------------------------------------
deltapHMeans <- ddply(AllData, c("Substrate","NutLevel", "DayNight"), summarize,
                           pHMean = mean(TankpH-HeaderpH, na.rm = T),
                           N2=sum(!is.na(TankpH)),
                           pHSE= sd(TankpH-HeaderpH, na.rm = T)/sqrt(N2)
                           
)


#delta pH day and night
y1<-c(0,-0.20)
y2<-c(0.2,0)
for (j in 1:2){
  par(mfrow=c(3,2))
  for (i in 1:length(sub)){
    x<-barplot(deltapHMeans$pHMean[deltapHMeans$Substrate==sub[i] & deltapHMeans$DayNight==DN[j]], main=sub[i], ylim=c(y1[j],y2[j]), xlab = DN[j],
               ylab=expression(paste(Delta,"pH")))
    errorbars(x,deltapHMeans$pHMean[deltapHMeans$Substrate==sub[i]& deltapHMeans$DayNight==DN[j]],0,deltapHMeans$pHSE[deltapHMeans$Substrate==sub[i]& deltapHMeans$DayNight==DN[j]])
    axis(1, at=x, labels=c("Ambeint","Medium","High"))
    lines(x,c(0,0,0))
  }  
}

#absolute change in pH
deltapHMeans.net <- ddply(AllData, c("Substrate","NutLevel"), summarize,
                      pHMean = mean(abs(TankpH-HeaderpH), na.rm = T),
                      N2=sum(!is.na(TankpH)),
                      pHSE= sd(abs(TankpH-HeaderpH), na.rm = T)/sqrt(N2)
                      
)


  par(mfrow=c(3,2))
  for (i in 1:length(sub)){
    x<-barplot(deltapHMeans.net$pHMean[deltapHMeans.net$Substrate==sub[i] ], main=sub[i], ylim=c(0,0.2), 
               ylab=expression(paste('abs(',Delta,"pH)")))
    errorbars(x,deltapHMeans.net$pHMean[deltapHMeans.net$Substrate==sub[i]],0,deltapHMeans.net$pHSE[deltapHMeans.net$Substrate==sub[i]])
    axis(1, at=x, labels=c("Ambeint","Medium","High"))
    lines(x,c(0,0,0))
  }  

#pH across time by substrate
  
  deltapHMeans.time <- ddply(AllData, c("Substrate","NutLevel", "DateTime"), summarize,
                        pHMean = mean(TankpH-HeaderpH, na.rm = T),
                        N2=sum(!is.na(TankpH)),
                        pHSE= sd(TankpH-HeaderpH, na.rm = T)/sqrt(N2)
                        
  )
  
  #Delta pH across time
  par(mfrow=c(3,2))
  rm(y)
  rm(yse)
  y<-deltapHMeans.time$pHMean
  yse<-deltapHMeans.time$pHSE
  for (i in 1:length(sub)){
    plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+0.1), ylab=expression(paste(Delta,"pH")), main = sub[i])
    
    abline(h=0)
    par(new = TRUE)
    #cols <- unique(NCP.mean$NutLevel)
    #
    for (j in 1:length(Nuts)){
      par(new = TRUE)
      
      plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
           y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
           pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.1))
      
      arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
             + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
             - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             angle=90, code=3, length = 0.1)
      start<-ifelse(i<=4,c(1),c(8))
      stops<-ifelse(i<=4,c(7),c(14))
      
    }
    axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
    
    #shaded area for night
    a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
    b<-ifelse(i<=4,6,13)
    rect(unique(deltapHMeans.time $DateTime)[a],min(y),unique(deltapHMeans.time $DateTime)[b],max(y)+.1,col = rgb(0.5,0.5,0.5,1/4), border = NA)
  }
  legend('topright', legend=unique(deltapHMeans.time $NutLevel), col=unique(deltapHMeans.time $NutLevel), pch=19, bty = 'n')
  
  ##nutrient plots --- these are currently inflated because I don't have all the data for the header tanks
  meanNuts<-ddply(AllData, 'NutLevel', summarize,
        meanNN=mean(HeaderN, na.rm=T),
        SENN=sd(HeaderN, na.rm=T)/sqrt(7),
        meanP=mean(HeaderP, na.rm=T),
        SEP=sd(HeaderP, na.rm=T)/sqrt(7))
        
  par(mfrow=c(1,2)) #average nitrate for experiment
  x<-barplot(meanNuts$meanNN, main=expression(paste('NO'[3]^{'2-'},'+ NH'[4]^{'+'})), 
             ylab=expression(paste(mu,'M')), ylim=c(0,10))
  errorbars(x,meanNuts$meanNN,0,meanNuts$SENN)
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  
  #average phosphate for experiment
  x<-barplot(meanNuts$meanP, main=expression(paste('PO'[4]^{'3-'})), 
             ylab='', ylim=c(0,2.5))
  errorbars(x,meanNuts$meanP,0,meanNuts$SEP)
  axis(1, at=x, labels=c("Ambeint","Medium","High"))
  