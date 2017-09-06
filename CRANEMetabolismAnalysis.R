####################################################
### CRANE Data Analysis for Biogeochemical Responses####
### Created by Nyssa Silbiger                   ###
### Created on 5/03/2016                       ###
### Edited on ---                               ###
### Edited by ---                               ###
###################################################

#{r message=FALSE, warning=FALSE} #hide the warning messages in rmarkdown
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
library('effects')
library('RColorBrewer')
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
  #NEC is in mmol cm^-2 hr-1  (or umol g-1 hr-1 if using one of the other measurements)
  
  #HeaderTA is TA from the header in umol/kg
  #TankTAis TA from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in g/cm3 (default =1.023)
# divide by 1000 to make umol g-1 h-1
   
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

##Calculate Nutrient uptake
NutCalc<-function(HeaderN,TankN,ResidenceTime,SurfaceArea, TankVolume=5678,SWDenstiy=1.023){
  
  #NEC<-0.5*(HeaderTA-TankTA)*TankVolume*SWDenstiy/(ResidenceTime*SurfaceArea)/1000
  
  NutRate<-(HeaderN-TankN)*(TankVolume/SurfaceArea)*(SWDenstiy/ResidenceTime)/1000
  
  
  return(NutRate)
  #Nutcalc calculated the nutrient uptake of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) r
  #Nutrate is in umol cm^-2 hr-1  (or umol g-1 hr-1 if using one of the other measurements)
  
  
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
# old file location ~/../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs
Coral <- read.csv("data/CoralSets_Rprocessed.csv",header=TRUE)

#Rubble
Rubble <- read.csv("data/Rubble_Rprocessed.csv",header=TRUE)

#Algae
Algae <- read.csv("data/Algae_Rprocessed.csv",header=TRUE)

#Sand
Sand<- read.csv("data/Sand_Rprocessed.csv",header=TRUE)


#I did not use the TA corrected to CRM because it made the data messier.... something was off

## Normalize the TA----------------------------------------------------------------------
#normalize the TA to N+N concentrations.  
#I am also only normalizing the headers for now because the N+N in the tanks are very low due to uptake and we dont have all those data

ChemData$HeaderTA.norm<-ChemData$HeaderTA-ChemData$HeaderN-ChemData$HeaderP
#ChemData$HeaderTA.norm<-ChemData$HeaderTA

#normalize the TA data to salinity---THE SALINITY DATA IS CAUSING PROBLEMS.....
#ChemData$HeaderTA.norm<-ChemData$HeaderTA*ChemData$HeaderSalinity/38
#ChemData$TankTA.norm<-ChemData$TankTA*ChemData$TankSalinity/38
ChemData$TankTA.norm<-ChemData$TankTA ### this is not salinity normalize... but its named norm here because of the code below

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
TankVol<-5678 #hole was at 6 quarts... this is the volume of each tank in ml

ChemData$ResTime<-(1/60)*(1/ChemData$Flow)*TankVol


#Add the algae bits to the final biomass
Algae$FinalSA<-Algae$FinalSA+Algae$BitsSA
Algae$FinalWW<-Algae$FinalWW+Algae$BitsWW
Algae$FinalVol<-Algae$FinalVol+Algae$BitsVol
Algae$AFDW<-Algae$AFDW+Algae$AFDWbits
Algae$DW<-Algae$DW+Algae$DWbits


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


## Sum up all the biological data by aquarium for each experiment
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
biology<-rbind(Rubble.Exp1Summary,Coral.Exp1Summary, Algae.Exp1Summary,Sand.Exp1Summary) #exp 1

biology2<-rbind(Rubble.Exp2Summary,Coral.Exp2Summary, Algae.Exp2Summary,Sand.Exp2Summary) #exp2

#this sums up all 4 parts for each aquarium for total biomass etc per aquarium
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

#add the mean Flow rates times
AllData<-merge(AllData, Flow.mean, all.x=TRUE)

#Make one huge dataset with all the biology and chem data together
AllData<-merge(AllData,biology, by='Aquarium', all.x=TRUE)
#format the dates
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

#sort data by aquarium so that I can calculate NEC easier

AllData<-AllData[order(AllData$Aquarium),]


#NEC using lagrangian method with AFDW normalization--THIS IS WHAT I AM USING
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
  ## Normalize the DIC Data to a constant salinity... again.. didnt do this cuz salinity data is no good
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
  
  ### Nutrient uptake ------------
  #N
  AllData$NUptake.AFDW<- NutCalc(HeaderN = AllData$HeaderN,
                                   TankN = AllData$TankN,
                                   ResidenceTime = AllData$ResTime.mean,
                                   SurfaceArea = AllData$AFDW)
  #P
  AllData$PUptake.AFDW<- NutCalc(HeaderN = AllData$HeaderP,
                                 TankN = AllData$TankP,
                                 ResidenceTime = AllData$ResTime.mean,
                                 SurfaceArea = AllData$AFDW)
  
  AllData$SiUptake.AFDW<- NutCalc(HeaderN = AllData$HeaderSi,
                                 TankN = AllData$TankSi,
                                 ResidenceTime = AllData$ResTime.mean,
                                 SurfaceArea = AllData$AFDW)
  
  #Bicarbonate uptake
  AllData$HCO3Uptake.AFDW<- NutCalc(HeaderN = AllData$HeaderHCO3,
                                  TankN = AllData$TankHCO3,
                                  ResidenceTime = AllData$ResTime.mean,
                                  SurfaceArea = AllData$AFDW)
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
  
  
# Mean bicarbonate uptake
                                 
  HCO3.mean <- ddply(AllData, c("Substrate","NutLevel","DateTime"), summarize,
                    HCO3Uptake = mean(HCO3Uptake.AFDW, na.rm = T),
                    #N=sum(!is.na(HCO3Uptake)),
                    N=3,
                    SE= sd(HCO3Uptake.AFDW, na.rm = T)/sqrt(N),
                    Mean.HCO3.Tank = mean(TankHCO3, na.rm = T),
                    SE.Tank= sd(TankHCO3, na.rm = T)/sqrt(N),
                    #this is only one because there is only one header per tank
                    Mean.HCO3.Header = mean(HeaderHCO3, na.rm = T),
                    SE.Header= sd(HeaderHCO3, na.rm = T)/sqrt(N)
  )
                    
par(mfrow=c(3,2))
for (i in 1:5){
plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i])
points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
}

par(mfrow=c(3,2))
for (i in 1:5){
  plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$NCP.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i])
  points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$NCP.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
  points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$NCP.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
}

par(mfrow=c(3,2))
for (i in 1:5){
  plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$NUptake.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i], ylim=c(-2,5), 
       ylab='N uptake (umol g AFDW-1 hr-1', xlab='')
  points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$NUptake.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
  points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$NUptake.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
  abline(h=0)
}
legend('topleft', legend= c('Ambient','Med','High'), col=c('black','blue','red'), pch=20, bty = 'n')

par(mfrow=c(3,2))
for (i in 1:5){
  plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$PUptake.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i], ylim=c(-1,1), 
       ylab='P uptake (umol g AFDW-1 hr-1', xlab='')
  points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$PUptake.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
  points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$PUptake.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
  abline(h=0)
}

par(mfrow=c(3,2))
for (i in 1:5){
  plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$SiUptake.AFDW[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i], ylim=c(-1,1), 
       ylab='Si uptake (umol g AFDW-1 hr-1', xlab='')
  points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$SiUptake.AFDW[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
  points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$SiUptake.AFDW[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
  abline(h=0)
}

# plot the nutrient uptake rates against each other
par(mfrow=c(1,3))
plot(AllData$NUptake.AFDW, AllData$PUptake.AFDW, xlab='N Uptake', ylab='P Uptake')
plot(AllData$NUptake.AFDW, AllData$SiUptake.AFDW, xlab='N Uptake', ylab='Si Uptake')
plot(AllData$PUptake.AFDW, AllData$SiUptake.AFDW, xlab='P Uptake', ylab='Si Uptake')

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
                            NEC.mean$DateTime==time[9]|NEC.mean$DateTime==time[10]|
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
                           Mean.AFDW2.NCP = mean(NCP.AFDW, na.rm=T),
                           GPP=Mean.AFDW2.p+Mean.AFDW2.r,
                           PR.AFDW2=GPP/Mean.AFDW2.r,
                           Mean.AFDW2.Day = mean(NEC.AFDW[DayNight=='Day'], na.rm=T), #day NEC
                           Mean.AFDW2.Night = mean(NEC.AFDW[DayNight=='Night'], na.rm=T), #Night NEC
                           Mean.AFDW2.NEC = mean(NEC.AFDW, na.rm=T) #Night NEC
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
  axis(1, at=x, labels=c("Ambient","Medium","High"))
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
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  lines(x,c(0,0,0))
}  
}


#NCP net

par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.Net$Mean.AFDW2[NCP.mean.Net$Substrate==sub[i]], main=sub[i], ylim=c(-10,25),
             ylab=expression(paste("Net NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NCP.mean.Net$Mean.AFDW2[NCP.mean.Net$Substrate==sub[i]],0,NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambient","Medium","High"))
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
    axis(1, at=x, labels=c("Ambient","Medium","High"))
    lines(x,c(0,0,0))
  }  
}

#GPP: add dark respiration to light net photosynthesis to get gross photosynthesis
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]], main=sub[i], ylim=c(0,max(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]])+2),
             ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  errorbars(x,NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]],0,NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate==sub[i]])
            
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  
}

#P/R
par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]], main=sub[i], ylim=c(0,max(NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]])+2), ylab = 'Mean P/R')
  errorbars(x,NCP.mean.PR$Mean.AFDW2[NCP.mean.PR$Substrate==sub[i]],0,NCP.mean.PR$SE.AFDW2[NCP.mean.PR$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  
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
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  
}

par(mfrow=c(3,2))
for (i in 1:length(sub)){
  x<-barplot(PercentCalc.mean$Mean.PercentDis[PercentCalc.mean$Substrate==sub[i]], main=sub[i], ylim=c(0,1), ylab = 'Mean % Dissolution')
  errorbars(x,PercentCalc.mean$Mean.PercentDis[PercentCalc.mean$Substrate==sub[i]],0,PercentCalc.mean$SE.PercentDis[PercentCalc.mean$Substrate==sub[i]])
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  
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

#Algae
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
model.NCPNet.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' ,]) 
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

model.NECOmega.Algae<-lmer(NEC.AFDW~ TankOmegaArag*NutLevel+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Algae',])

model.NECOmega.Coral<-lmer(NEC.AFDW~ TankOmegaArag*NutLevel+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Coral',])
#coef(model.NECOmega.Coral) #get the coefficients

model.NECOmega.Sand<-lmer(NEC.AFDW~ TankOmegaArag*NutLevel+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Sand',])

model.NECOmega.Rubble<-lmer(NEC.AFDW~ TankOmegaArag*NutLevel+ (1|Tank/Aquarium), data=AllData[AllData$Substrate=='Rubble',])

model.NECOmega.Mixed<-lmer(NEC.AFDW~ TankOmegaArag*NutLevel+(1|Tank/Aquarium), data=AllData[AllData$Substrate=='Mixed',])

# models for pH vs NCP
# model.pH.NCP<-lm(AllData$TankpH~AllData$NCP.AFDW)
#Ranef to account for repeated measures of aquarium within tank and time  
model.pH.NCP<-lmer(TankpH~NCP.AFDW*NutLevel*Substrate+ (1|Tank/Aquarium) , data=AllData)
 
# modelfor pH as a function of species, nutrients and day night
#model.pH.sp.nut<-lmer(TankpH~Substrate*NutLevel*DayNight + (1|DateTime) + (1|Tank/Aquarium), data=AllData)

 
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


#### Change color for pub quality figures
mypalette<-brewer.pal(9,"Blues") # light pink to purple for ambient to high nutrients
mypalette<-mypalette[c(3,6,9)] # pull out 3 contrasting colors for ambient, med, and high

#add the colors to AllData
AllData$colors<-NA
  for (i in 1:length(Nuts)){
AllData$colors[AllData$NutLevel ==Nuts[i]]<-mypalette[i] 
  }

###Looking at feedbacks--------------------------------------------
##plot for the NEC versus aragonite saturation state relationships by substrate and treatment
pdf(file = 'Plots/MSplots/NCPvspH.pdf', height = 6, width = 6)
par(mfrow=c(1,1), pty='s')
#plot(AllData$NCP.AFDW, AllData$TankpH, col=AllData$NutLevel, xlab='NCP', ylab='pH')
#legend('topleft',legend=c('Ambient',"Medium","High"), col=unique(AllData$NutLevel), pch=19, bty = 'n')
plot(AllData$NCP.AFDW, AllData$TankpH, col=AllData$colors, xlab='NCP', ylab='pH', pch=19)
legend('topleft',legend=c('Ambient',"Medium","High"), col=unique(AllData$colors), pch=19, bty = 'n')

dev.off()

#par(pty='r')
plot(AllData$TankOmegaArag, AllData$NEC.AFDW, col=AllData$NutLevel)

## NEC vs Omegan plot-------------------------------------
pdf("plots/MSplots/NECvsOmega.pdf", width=6, height=8)
par(mfrow=c(3,2))
#cols <- c(unique(NEC.mean$NutLevel))
cols <- mypalette
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
for (i in 1:length(sub)){
  plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
       ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), main = sub[i], xlim=c(1.5,6))
  
  abline(h=0, lty=2)
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], yaxt='n',
         y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = adjustcolor( cols[j], alpha.f = 0.5),
         pch=19, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])))
   
    
    model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
    #if the p>0.05 make it a dashed line (or don't include)
    p<-anova(model)$`Pr(>F)`[1]
    if(p<=0.05){
      lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3, lty=1)
    }
     # else{
      #lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=0.5, lty=2)
    #}
  }
}
#empty plot to put the legend
plot(NA, ylim=c(0,1), xlim=c(0,1), axes=FALSE,  ylab="", xlab="")
legend('center', horiz = FALSE, legend=unique(NEC.mean$NutLevel), col=mypalette, pch=19, bty = 'n')

dev.off()

# calculate contribution of NEC and NCP to pH---------------------
#function to calculate pH controbution that returns NEC and NCP contrinutions
pHContributions<-function(pH.ctrl, pH.treat, TA.ctrl, TA.treat, DIC.ctrl, temp=25.5, sal=35.5){
  #This function calculates the contributions of NEC and NCP to change in pH between the header tanks and the control tanks
  #the formula is based on Page et al. 2016 Coral Reefs
  
  #NEC contribution to pH
  #change in TA from calcification
  TA.pHNEC<-TA.ctrl+(TA.treat-TA.ctrl)
  #change in DIC from calcification
  DIC.pHNEC<-DIC.ctrl+(0.5*(TA.treat-TA.ctrl))
  #use TA and DIC to calculate pH from the seacarb function
  
  AllCO2<-carb(flag=15, TA.pHNEC/1000000, DIC.pHNEC/1000000, S=sal, T=temp, Patm=1, P=0, Pt=0, Sit=0,
               k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential")
  #TA is divided by 1000 because all calculations are in mol/kg in the seacarb package
  
  pH.NEC<-AllCO2$pH## calculate pH at temp = 25.5 and salinity = 35.5
  
  #calculate change in pH due to calcification
  pHNEC.treat<-pH.ctrl-pH.NEC
  #NCP contribution to pH
  # subtract the change in pH due to calcification from delta pH
  pHNCP.treat<-pH.treat-pH.ctrl- pHNEC.treat
  
  
  return(cbind(pHNEC.treat,pHNCP.treat))
}

#add NEC and NCP pH contronutions to ChemData
AllData<-cbind(AllData,pHContributions(pH.ctrl=ChemData$HeaderpH, pH.treat = ChemData$TankpH, TA.ctrl = ChemData$HeaderTA, TA.treat = ChemData$TankTA, DIC.ctrl = ChemData$HeaderDIC))

## pH plots----------------------------------------------------------------
deltapHMeans <- ddply(AllData, c("Substrate","NutLevel", "DayNight"), summarize,
                           pHMean = mean(TankpH-HeaderpH, na.rm = T),
                           N2=sum(!is.na(TankpH)),
                           pHSE= sd(TankpH-HeaderpH, na.rm = T)/sqrt(N2),
                           pH.NEC=mean(pHNEC.treat),
                           pH.NECSE= sd(pHNEC.treat, na.rm = T)/sqrt(N2),
                           pH.NCP=mean(pHNCP.treat),
                           pH.NCPSE= sd(pHNCP.treat, na.rm = T)/sqrt(N2)
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
    axis(1, at=x, labels=c("Ambient","Medium","High"))
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
    axis(1, at=x, labels=c("Ambient","Medium","High"))
    lines(x,c(0,0,0))
  }  

#pH across time by substrate
  
  deltapHMeans.time <- ddply(AllData, c("Substrate","NutLevel", "DateTime"), summarize,
                        pHMean = mean(TankpH-HeaderpH, na.rm = T),
                        N2=sum(!is.na(TankpH)),
                        pHSE= sd(TankpH-HeaderpH, na.rm = T)/sqrt(N2),
                        pH.NEC=mean(pHNEC.treat),
                        pH.NECSE= sd(pHNEC.treat, na.rm = T)/sqrt(N2),
                        pH.NCP=mean(pHNCP.treat),
                        pH.NCPSE= sd(pHNCP.treat, na.rm = T)/sqrt(N2)
  )
  
  #Delta pH across time
  pdf(file = 'Plots/MSplots/DeltapHTime.pdf', height = 8, width = 6)
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
           pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.1), yaxt='n')
      
      arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
             + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
             - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             angle=90, code=3, length = 0.05, col = cols[j])
      start<-ifelse(i<=4,c(1),c(8))
      stops<-ifelse(i<=4,c(7),c(14))
      
    }
    axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
    
    #shaded area for night
    a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
    b<-ifelse(i<=4,6,13)
    rect(unique(deltapHMeans.time $DateTime)[a]+3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.1,col = rgb(0.5,0.5,0.5,1/4), border = NA)
  }
  legend('bottomleft', legend=unique(deltapHMeans.time $NutLevel), col=mypalette, pch=19, bty = 'n')
  
  dev.off()
  ##nutrient plots --- just for the headers.... 
  #calculate the mean nutrient conditions
  meanNuts<-ddply(AllData, 'NutLevel', summarize,
        meanNN=mean(HeaderN, na.rm=T),
        SENN=sd(HeaderN, na.rm=T)/sqrt(7),
        meanP=mean(HeaderP, na.rm=T),
        SEP=sd(HeaderP, na.rm=T)/sqrt(7))
  pdf(file = 'Plots/MSplots/NutrientsColor.pdf', height = 5, width = 8)      
  par(mfrow=c(1,2)) #average nitrate for experiment
  x<-barplot(meanNuts$meanNN, main=expression(paste('NO'[3]^{'-'},'+ NO'[2]^{'-'})), 
             ylab=expression(paste(mu,'M')), ylim=c(0,10), col = mypalette)
  errorbars(x,meanNuts$meanNN,0,meanNuts$SENN)
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  
  #average phosphate for experiment
  x<-barplot(meanNuts$meanP, main=expression(paste('PO'[4]^{'3-'})), 
             ylab='', ylim=c(0,3.0), col= mypalette)
  errorbars(x,meanNuts$meanP,0,meanNuts$SEP)
  axis(1, at=x, labels=c("Ambient","Medium","High"))
  dev.off()
## DOC Analysis------------------------------------------
  #DOC Data are from Craig from Day 14 and 28 from the long term experiment.  There are not DOC data for the mixed community
  #Input is from the header tanks, source is the ocean (I think?? need to double check)
  Data.DOC<-read.csv('Data/DOCforNyssa.csv')
  
  AllDOC<-merge(Data.DOC, NCP.mean.PRbyTank, all.x=TRUE)
  AllDOC$excessDOC<-Data.DOC$DOC - Data.DOC$InputDOC #excess DOC is what is left over
 # AllDOC$NEC.NCP<-AllDOC$Mean.AFDW2.NEC/AllDOC$Mean.AFDW2.NCP #NEC/NCP ratios
  
  #Take the averages by substrate, nutrients, and day for plotting
  
  DOC.means <- ddply(AllDOC, c("Substrate","NutLevel", "Days"), summarize,
                             #Mean.AFDW2.p = mean(NCP.AFDW[NCP.AFDW>0], na.rm=T),
                             N=sum(!is.na(DOC)),
                             r =mean(Mean.AFDW2.r, na.rm = T),
                             r.SE=sd(Mean.AFDW2.r)/sqrt(N),
                             NCP = mean(Mean.AFDW2.NCP, na.rm=T),
                             NCP.SE=sd(Mean.AFDW2.NCP)/sqrt(N),
                             GPP.mean=mean(GPP, na.rm=T),
                             GPP.SE=sd(GPP)/sqrt(N),
                             PR=mean(PR.AFDW2, na.rm=T),
                             PR.SE=sd(PR.AFDW2)/sqrt(N),        
                             NEC.Day = mean(Mean.AFDW2.Day, na.rm=T), #day NEC
                             NEC.Day.SE=sd(Mean.AFDW2.Day)/sqrt(N),        
                             NEC.Night = mean(Mean.AFDW2.Night, na.rm=T), #Night NEC
                             NEC.Night.SE=sd(Mean.AFDW2.Night)/sqrt(N),
                             NEC.Net = mean(Mean.AFDW2.NEC, na.rm=T), #Night NEC
                             NEC.Net.SE=sd(Mean.AFDW2.NEC)/sqrt(N),
                             NEC.NCP.mean= mean(Slope, na.rm=T),
                             NEC.NCP.SE=sd(Slope)/sqrt(N),
                             DOC.mean = mean(DOC, na.rm=T),
                             DOC.SE=sd(DOC)/sqrt(N)
                             
  )
  
  #plot DOC versus NEC:NCP ratios
  par(mfrow=c(2,2))
 plot(0,type='n', xlim=c(60,90), ylim=c(-0.2,0.2), xlab='DOC', ylab='NEC:NCP', main ='Algae')
    for (j in 1:3){ #nutrients
      #par(new = TRUE)
      x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
      y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
      points(x,y,type = 'p', col = cols[j], pch=19)
      arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
             x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
             angle=90, code=3, length = 0.1)
      arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
             x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
             angle=90, code=3, length = 0.1)
    }
 
 plot(0,type='n', xlim=c(60,150), ylim=c(0,0.2), xlab='DOC', ylab='NEC:NCP', main='Coral')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
   y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          angle=90, code=3, length = 0.1)
 }
 legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')
 
 
 plot(0,type='n', xlim=c(60,80), ylim=c(-0.2,0.5), xlab='DOC', ylab='NEC:NCP', main='Rubble')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
   y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          angle=90, code=3, length = 0.1)
 }

 plot(0,type='n', xlim=c(60,90), ylim=c(-.05,0.2), xlab='DOC', ylab='NEC:NCP', main='Sand')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
   y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          angle=90, code=3, length = 0.1)
 }
 
###---------------
 #Same plot versus GPP
 #plot DOC versus NEC:NCP ratios
 par(mfrow=c(2,2))
 plot(0,type='n', xlim=c(60,90), ylim=c(0,50), xlab='DOC', ylab='GPP', main ='Algae')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
   y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,150), ylim=c(0,60), xlab='DOC', ylab='GPP', main='Coral')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
   y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          angle=90, code=3, length = 0.1)
 }
 legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')
 
 
 plot(0,type='n', xlim=c(60,80), ylim=c(0,15), xlab='DOC', ylab='GPP', main='Rubble')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
   y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,90), ylim=c(0,30), xlab='DOC', ylab='GPP', main='Sand')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
   y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          angle=90, code=3, length = 0.1)
 }
 
#Same plot versus R--------------------
 
 par(mfrow=c(2,2))
 plot(0,type='n', xlim=c(60,90), ylim=c(0,15), xlab='DOC', ylab='R', main ='Algae')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
   y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,150), ylim=c(0,30), xlab='DOC', ylab='R', main='Coral')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
   y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          angle=90, code=3, length = 0.1)
 }
 legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')
 
 
 plot(0,type='n', xlim=c(60,80), ylim=c(0,10), xlab='DOC', ylab='R', main='Rubble')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
   y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,90), ylim=c(0,15), xlab='DOC', ylab='R', main='Sand')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
   y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          angle=90, code=3, length = 0.1)
 }
 
#Same plot versus NCP--------------------
 
 par(mfrow=c(2,2))
 plot(0,type='n', xlim=c(60,90), ylim=c(0,20), xlab='DOC', ylab='NCP', main ='Algae')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
   y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,150), ylim=c(0,10), xlab='DOC', ylab='NCP', main='Coral')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
   y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          angle=90, code=3, length = 0.1)
 }
 legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')
 
 
 plot(0,type='n', xlim=c(60,80), ylim=c(-5,1), xlab='DOC', ylab='NCP', main='Rubble')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
   y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,90), ylim=c(-2,6), xlab='DOC', ylab='NCP', main='Sand')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
   y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 #-
 
 #-------------
#Same plot versus NEC--------------------
 
 par(mfrow=c(2,2))
 plot(0,type='n', xlim=c(60,90), ylim=c(-3,2), xlab='DOC', ylab='NEC', main ='Algae')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
   y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,150), ylim=c(0,15), xlab='DOC', ylab='NEC', main='Coral')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
   y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
          angle=90, code=3, length = 0.1)
 }
 legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')
 
 
 plot(0,type='n', xlim=c(60,80), ylim=c(-2,0), xlab='DOC', ylab='NEC', main='Rubble')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
   y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 plot(0,type='n', xlim=c(60,90), ylim=c(-4,2), xlab='DOC', ylab='NEC', main='Sand')
 for (j in 1:3){ #nutrients
   #par(new = TRUE)
   x=DOC.means$DOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
   y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
   points(x,y,type = 'p', col = cols[j], pch=19)
   arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
          angle=90, code=3, length = 0.1)
   arrows(x + DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          x - DOC.means$DOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
          angle=90, code=3, length = 0.1)
 }
 
 
#Plot NEC versus NCP-----------------------
 b<-matrix(nrow=5,ncol=3)
 for(i in 1:5){
   plot(0,type='n', xlim=c(-20,60), ylim=c(-10,20), xlab='NCP', ylab='NEC',main=sub[i])
   for (j in 1:3){
     #par(new = TRUE)
     points(AllData$NCP.AFDW[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]], main=sub[i], 
            type = 'p', col = cols[j], pch=19)
     
     
     modelTA.DIC<-lm(AllData$NEC.AFDW[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]]  ~AllData$NCP.AFDW[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]])
     b0[i,j]<-modelTA.DIC$coefficients[1]
     b[i,j]<-modelTA.DIC$coefficients[2]
     r2[i,j]<-summary(modelTA.DIC)$r.squared
     substrate[i,j]<-sub[i]
     Nut[i,j]<-Nuts[j]
     x<-seq(from=-20, to=60,by=0.5)
     y<-b[i,j]*x+b0[i,j]
     lines(x,y, col=cols[j], lwd=3)
     
     
     
   }
   legend('topleft',legend=c('Ambient',"Medium","High"), col=cols, pch=19, bty = 'n')
 }
 
##-----
 
 # plot the amount of bicarbonate in each tank over time
 par(mfrow=c(3,2))
 cols <- c(unique(HCO3.mean$NutLevel))
 #y<-HCO3.mean$HCO3Uptake
 #yse<-HCO3.mean$SE
 y<-HCO3.mean$Mean.HCO3.Tank
 yse<-HCO3.mean$SE.Tank
 for (i in 1:length(sub)){
   plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+5), ylab=expression(paste("HCO3")), main = sub[i])
   
   abline(h=0)
   par(new = TRUE)
   
   #
   for (j in 1:length(Nuts)){
     par(new = TRUE)
     
     plot(as.numeric(HCO3.mean$DateTime [HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]),
          y[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], col = cols[j],
          pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+5))
     
     arrows(unique(HCO3.mean$DateTime), y[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]
            + yse[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], 
            unique(HCO3.mean$DateTime), y[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]
            - yse[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], 
            angle=90, code=3, length = 0.1)
     lines(as.numeric(HCO3.mean$DateTime [HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]),
          HCO3.mean$Mean.HCO3.Header[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], col = 'yellow',
          pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+5))
     arrows(unique(HCO3.mean$DateTime), HCO3.mean$Mean.HCO3.Header[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]
            + HCO3.mean$SE.Header[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], 
            unique(HCO3.mean$DateTime), HCO3.mean$Mean.HCO3.Header[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]]
            - HCO3.mean$SE.Header[HCO3.mean$Substrate==sub[i] & HCO3.mean$NutLevel==Nuts[j]], 
            angle=90, code=3, length = 0.1)
     
     start<-ifelse(i<=4,c(1),c(8))
     stops<-ifelse(i<=4,c(7),c(14))
     
   }
   axis(1, at=unique(HCO3.mean$DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
   
   #shaded area for night
   a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
   b<-ifelse(i<=4,6,13)
   rect(unique(HCO3.mean$DateTime)[a],min(y),unique(HCO3.mean$DateTime)[b],max(y)+5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
 }
 legend('topright', legend=unique(HCO3.mean$NutLevel), col=unique(HCO3.mean$NutLevel), pch=19, bty = 'n')
 
 ##### Mixed effects models for the change in BW for algae, rubble, coral 
 #coral
 # NOTE THE pc DATA IS PROPORTIONAL, NOT PERCENT SO MULTUPLY EVERYTHING BY 100
 Coral$pcDeltaBW<-Coral$pcDeltaBW*100
 Algae$pcDeltaWW<-Algae$pcDeltaWW*100
 Rubble$pcDeltaBW<-Rubble$pcDeltaBW*100
 
 CoralBW.mod<-lmer(pcDeltaBW ~ Nuts+Species + (1|Tank), data= Coral) # I looked for an interaction, but found none
 #CoralBW.mod<-lmer(pcDeltaBW ~ Nuts + (1|Tank/Species), data= Coral) # this puts species nested within tank as a random effect
 
 # order the nutrient levels
  Coral$Nuts<-ordered(Coral$Nuts, levels =c('Ambient','Medium','High') )
 # calculate means and SE for plotting
 Coral.mean<-ddply(Coral, c("Species","Nuts"), summarize,
       pcBW.mean = mean(pcDeltaBW, na.rm=TRUE),
       n = sum(!is.na(pcDeltaBW)),
       pcBW.se = sd(pcDeltaBW, na.rm=TRUE)/sqrt(n))
 
 #algae
 AlgaeBW.mod<-lmer(pcDeltaWW ~ Nuts + (1|Tank), data= Algae) # I looked for an interaction, but found none
 Algae$Nuts<-ordered(Algae$Nuts, levels =c('Ambient','Medium','High') )
 Algae.mean<-ddply(Algae, c("Nuts"), summarize,
                   pcBW.mean = mean(pcDeltaWW, na.rm=TRUE),
                   n = sum(!is.na(pcDeltaWW)),
                   pcBW.se = sd(pcDeltaWW, na.rm=TRUE)/sqrt(n))
 
 # rubble
RubbleBW.mod<-lmer(pcDeltaBW ~ Nuts + (1|Tank), data= Rubble) # I looked for an interaction, but found none
Rubble$Nuts<-ordered(Rubble$Nuts, levels =c('Ambient','Medium','High') )
Rubble.mean<-ddply(Rubble, c("Nuts"), summarize,
                  pcBW.mean = mean(pcDeltaBW, na.rm=TRUE),
                  n = sum(!is.na(pcDeltaBW)),
                  pcBW.se = sd(pcDeltaBW, na.rm=TRUE)/sqrt(n))

pdf('plots/MSplots/BuoyantWeight.pdf', width = 4, height = 8.5)
par(mfrow=c(3,1))
# create a matrix for the coral data
m<-t(matrix(Coral.mean$pcBW.mean,3,2))
mse<-t(matrix(Coral.mean$pcBW.se,3,2))
x<-barplot(m,beside=TRUE,  ylim=c(0, 5), ylab = 'Coral % change BW',
           legend.text = c(expression(italic('M. capitata')), expression(italic('P. compressa'))), args.legend = list(bty='n'))
arrows(x, m + mse,  # x , mean + SE
       x, m - mse,  # x, mean - SE
       angle=90, code=3, length = 0.05)

# algae
x<-barplot(Algae.mean$pcBW.mean,   ylim = c(0,300), ylab = 'Algae % change WW')
arrows(x, Algae.mean$pcBW.mean + Algae.mean$pcBW.se,  # x , mean + SE
       x, Algae.mean$pcBW.mean - Algae.mean$pcBW.se,  # x, mean - SE
       angle=90, code=3, length = 0.05)

#Rubble
x<-barplot(Rubble.mean$pcBW.mean, names.arg = c('Ambient', 'Medium','High'),  ylab = 'Rubble % change BW', ylim=c(-0.6,0.6))
arrows(x, Rubble.mean$pcBW.mean + Rubble.mean$pcBW.se,  # x , mean + SE
       x, Rubble.mean$pcBW.mean - Rubble.mean$pcBW.se,  # x, mean - SE
       angle=90, code=3, length = 0.05)
abline(h=0)
dev.off()

pdf('plots/MSplots/BuoyantWeight_dot.pdf', width = 4, height = 8.5)
par(mfrow=c(3,1))
# create a matrix for the coral data
plot(c(1:3), Coral.mean$pcBW.mean[1:3], pch=19, ylim=c(0, 5), xlab="", xaxt='n', ylab = 'Coral % change BW', col = mypalette, cex=1.5)
lines(c(1:3), Coral.mean$pcBW.mean[1:3], col = 'black', type = "c")
arrows(c(1:3), Coral.mean$pcBW.mean + Coral.mean$pcBW.se,  # x , mean + SE
       c(1:3), Coral.mean$pcBW.mean - Coral.mean$pcBW.se,  # x, mean - SE
       angle=90, code=3, length = 0.05)
points(c(1:3), Coral.mean$pcBW.mean[4:6], pch=15, ylim=c(0, 5), col = mypalette, cex=1.5)
lines(c(1:3), Coral.mean$pcBW.mean[4:6], col = 'black', type = "c")
legend('topright',legend = c(expression(italic('M. capitata')), expression(italic('P. compressa'))), bty='n',
       pch = c(19,15), col = 'black')

  
# algae
plot(1:3,Algae.mean$pcBW.mean,   ylim = c(200,300), xlab="",xaxt='n',ylab = 'Algae % change WW', pch = 19, col = mypalette, cex=1.5)
lines(c(1:3), Algae.mean$pcBW.mean, col = 'black', type = "c")
arrows(1:3, Algae.mean$pcBW.mean + Algae.mean$pcBW.se,  # x , mean + SE
       1:3, Algae.mean$pcBW.mean - Algae.mean$pcBW.se,  # x, mean - SE
       angle=90, code=3, length = 0.05)
       

#Rubble
plot(Rubble.mean$pcBW.mean, xaxt='n',  ylab = 'Rubble % change BW',xlab="",
     pch = 19, col = mypalette, cex=1.5, ylim=c(-0.6,0.6))
arrows(1:3, Rubble.mean$pcBW.mean + Rubble.mean$pcBW.se,  # x , mean + SE
       1:3, Rubble.mean$pcBW.mean - Rubble.mean$pcBW.se,  # x, mean - SE
       angle=90, code=3, length = 0.05)
abline(h=0, lty=2)
lines(c(1:3), Rubble.mean$pcBW.mean, col = 'black', type = "c")
text(x = 1:3, par("usr")[3] ,  labels = c("Ambient","Medium","High"), pos = 1, xpd = TRUE)

dev.off()


### NEC and NCP plots and stats file #####
substrate<-c('Coral','Algae','Rubble','Sand','Mixed')

# create a list of all the models
mods.NEC<-list(model.NECNet.Coral,model.NECNet.algae, model.NECNet.Rubble,model.NECNet.Sand,model.NECNet.Mixed)
mods.NECD<-list(model.NECDay.Coral, model.NECDay.algae, model.NECDay.Rubble, model.NECDay.Sand,model.NECDay.Mixed)            
mods.NECN<-list(model.NECNight.Coral, model.NECNight.algae, model.NECNight.Rubble, model.NECNight.Sand,model.NECNight.Mixed)            

#  create a dataframe of the stats to report in a the paper
NEC.net.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))
NEC.day.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))
NEC.night.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))

# Make a function so I can minimize errors
Nutplot.NEC<-function(species,i, Main = TRUE, YLAB = 'NEC'){
  
  #Day
  y<-summary(mods.NECD[[i]])$coefficients[,1:2]
  a<-anova(mods.NECD[[i]])
  ef<-as.data.frame(effect("NutLevel", mods.NECD[[i]]))
  
  SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species & NEC.mean.DayNight$DayNight=='Day']
  SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species& NEC.mean.DayNight$DayNight=='Day']
  
  # make plotting y axis beter
  ef2<-as.data.frame(effect("NutLevel", mods.NECN[[i]]))
  SE.low<-ef2$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species& NEC.mean.DayNight$DayNight=='Night']
  
  #plot
  plot(1:3-0.2,ef$fit, pch = 2,  cex = 3, main=ifelse(Main ==TRUE, species,NA),
       ylim=c(c(ifelse(min(SE.low)>0,0,floor(min(SE.low))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.low)>0,0,floor(min(SE.low))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB=='NEC', expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), 
                   expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1}))),
       cex.main=2, cex.axis=1.5, cex.lab=1.5, xlab = "", col=mypalette, xaxt='n', xlim=c(0.25,3.75))
  
  lines(1:3-0.2,ef$fit, col = 'black', type = 'c' )
  arrows(1:3-0.2, SE.upper,1:3-0.2, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  # add star or NS for significance
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
 
  #calculate 95%CI using effects
  y<-summary(mods.NEC[[i]])$coefficients[,1:2]
  a<-anova(mods.NEC[[i]])
  #calculate 95%CI using effects
  ef<-as.data.frame(effect("NutLevel", mods.NEC[[i]]))
  
  SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species]
  SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species]
  
  points(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
         col=mypalette, xaxt='n', cex=3)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2, col='grey')
 
  #add a star to the graph if it is statistically significant
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
  
  # night
  y<-summary(mods.NECN[[i]])$coefficients[,1:2]
  a<-anova(mods.NECN[[i]])
  ef<-as.data.frame(effect("NutLevel", mods.NECN[[i]]))
  
  SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species& NEC.mean.DayNight$DayNight=='Night']
  SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species& NEC.mean.DayNight$DayNight=='Night']
  
  points(1:3+0.2,ef$fit, cex=3, pch = 17, col = mypalette)
  lines(1:3+0.2,ef$fit, col = 'black', type = 'c' )
  arrows(1:3+0.2, SE.upper,1:3+0.2, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
  
  axis(1, at=1:3, labels = c("Ambient","Medium","High"), las = 2, cex.lab=1.5,cex.axis=1.5)
  
}

pdf("plots/MSplots/MeanRates_NEC.pdf", width=18, height=5)
j<-2
par(bg=NA) 
par(pty="m")
##  plot of all metabolic rates by substrate
par(mfrow=c(1,5))
par(mar=c(6.2,6.1,4.1,1.5))
par(lwd = 2) 
for(i in 1:5){
  Nutplot.NEC(substrate[i],i,'TRUE','NEC')
  # save the stats
  #day
  a<-anova(mods.NECD[[i]])
  NEC.day.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
  #night
  a<-anova(mods.NECN[[i]])
  NEC.night.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
  #net
  a<-anova(mods.NEC[[i]])
  NEC.net.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
  
}
legend('topright', legend = c('Day','Night','Net'), pch = c(2,17,19), col = mypalette, bty='n', cex=1.5)
dev.off()
###############

## NCP
#  create a dataframe of the stats to report in a the paper
NCP.net.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))
NCP.day.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))
NCP.night.stats<-data.frame(matrix(NA,nrow=5,ncol=7, dimnames = list(c(1:5),c('Substrate','SS','MS','NumDF','DenDF','F','P'))))

Nutplot.NCP<-function(species,i, Main = TRUE, YLAB = 'NCP'){
  
  #Day
  y<-summary(mods.NECD[[i]])$coefficients[,1:2]
  a<-anova(mods.NECD[[i]])
  ef<-as.data.frame(effect("NutLevel", mods.NECD[[i]]))
  
  SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species & NCP.mean.DayNight$DayNight=='Day']
  SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species& NCP.mean.DayNight$DayNight=='Day']
  
  # make plotting y axis beter
  ef2<-as.data.frame(effect("NutLevel", mods.NECN[[i]]))
  SE.low<-ef2$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species& NCP.mean.DayNight$DayNight=='Night']
  
  #plot
  plot(1:3-0.2,ef$fit, pch = 2,  cex = 3, main=ifelse(Main ==TRUE, species,NA),
       ylim=c(c(ifelse(min(SE.low)>0,0,floor(min(SE.low))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.low)>0,0,floor(min(SE.low))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB=='NEC', expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), 
                   expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1}))),
       cex.main=2, cex.axis=1.5, cex.lab=1.5, xlab = "", col=mypalette, xaxt='n', xlim=c(0.25,3.75))
  
  lines(1:3-0.2,ef$fit, col = 'black', type = 'c' )
  arrows(1:3-0.2, SE.upper,1:3-0.2, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  # add star or NS for significance
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
  
  #calculate 95%CI using effects
  y<-summary(mods.NEC[[i]])$coefficients[,1:2]
  a<-anova(mods.NEC[[i]])
  #calculate 95%CI using effects
  ef<-as.data.frame(effect("NutLevel", mods.NEC[[i]]))
  
  SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==species]
  SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==species]
  
  points(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
         col=mypalette, xaxt='n', cex=3)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2, col='grey')
 
  #add a star to the graph if it is statistically significant
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
  
  # night
  y<-summary(mods.NECN[[i]])$coefficients[,1:2]
  a<-anova(mods.NECN[[i]])
  ef<-as.data.frame(effect("NutLevel", mods.NECN[[i]]))
  
  SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species& NCP.mean.DayNight$DayNight=='Night']
  SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species& NCP.mean.DayNight$DayNight=='Night']
  
  points(1:3+0.2,ef$fit, cex=3, pch = 17, col = mypalette)
  lines(1:3+0.2,ef$fit, col = 'black', type = 'c' )
  arrows(1:3+0.2, SE.upper,1:3+0.2, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  if(a$`Pr(>F)`<=0.055){text(3+0.6,ef$fit[3],'*', cex=3)}   #  legend("top",'*', cex=2, bty="n")
  if(a$`Pr(>F)`>=0.055){text(3+0.6,ef$fit[3],'NS')}
  
  axis(1, at=1:3, labels = c("Ambient","Medium","High"), las = 2, cex.lab=1.5,cex.axis=1.5)
  
}

pdf("plots/MSplots/MeanRates_NCP.pdf", width=18, height=5)
j<-2
par(bg=NA) 
par(pty="m")
##  plot of all metabolic rates by substrate
par(mfrow=c(1,5))
par(mar=c(6.2,6.1,4.1,1.5))
par(lwd = 2) 

# create a list of all the NCP models
mods.NEC<-list(model.NCPNet.Coral,model.NCPNet.algae, model.NCPNet.Rubble,model.NCPNet.Sand,model.NCPNet.Mixed)
mods.NECD<-list(model.GCP.Coral, model.GCP.algae, model.GCP.Rubble, model.GCP.Sand,model.GCP.Mixed)            
mods.NECN<-list(model.R.Coral, model.R.algae, model.R.Rubble, model.R.Sand,model.R.Mixed)            

for(i in 1:5){
  Nutplot.NCP(substrate[i],i, 'TRUE', 'NCP')
  a<-anova(mods.NECD[[i]])
  NCP.day.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
  #night
  a<-anova(mods.NECN[[i]])
  NCP.night.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
  #net
  a<-anova(mods.NEC[[i]])
  NCP.net.stats[i,1:7]<-t(matrix(c(substrate[i],a[1:6])))
}
legend('topright', legend = c('GCP','R','NCP'), pch = c(2,17,19), col = mypalette, bty='n', cex=1.5)
dev.off()

# write the stats to a csv
write.csv(NEC.day.stats, 'stats/NEC.day.stats.csv')
write.csv(NEC.night.stats, 'stats/NEC.night.stats.csv')
write.csv(NEC.net.stats, 'stats/NEC.net.stats.csv')
write.csv(NCP.day.stats, 'stats/NCP.day.stats.csv')
write.csv(NCP.night.stats, 'stats/NCP.night.stats.csv')
write.csv(NCP.net.stats, 'stats/NCP.net.stats.csv')
