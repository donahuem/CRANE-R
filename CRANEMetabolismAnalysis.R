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
# Functions-----------------------------------------
#easy errorbar barplots
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

NECCalc<-function(HeaderTA,TankTA,ResidenceTime,SurfaceArea, TankVolume=5678,SWDenstiy=1.023){
 
   NEC<-((HeaderTA-TankTA)/(ResidenceTime*SurfaceArea)*TankVolume*SWDenstiy)/1000
   
   return(NEC)
  #NEC calc calculated the net ecosystem calcification of a flow through mesocosm system at a given time point
  #this uses residence time (lagrangian) rather than change in TA over time (Eulerian-- this is what I used for the 
  #biogeochemistry paper)
  #NEC is in umol cm^-2 hr-1
  
  #HeaderTA is TA from the header in umol/kg
  #TankTAis TA from the tank in umol/kg
  #Residence time is the residence time in hours
  #Surface area is SA of the substrate in cm2
  #TankVolume is the volume in cm3 = (default = 5678)
  #SWDensity is density of seawater in kg/cm3 (default =1.023)

}  


# Load Data-----------------------------------------
#Chem Data
ChemData<-read.csv('Data/AllChemData.csv') #read in 1st 12 columns only

#Biology Data
source('CRANE-DataProcessing.R')
#Coral
#Coral<-read.csv('Data/CoralBiology.csv', nrows=72)
#Rubble
#Rubble<-read.csv('Data/RubbleBiology.csv',nrows = 36)
#Algae
#Algae<-read.csv('Data/AlgaeBiology.csv', nrows=36)

Coral <- CoralSet

#Both the Tank TA and the header TA are already corrected for %off from CRM in the excel sheet

## Normalize the TA----------------------------------------------------------------------
#normalize the TA to N+N concentrations.  Right now we are using 0,3,6 as place holders until the real data get here
#I am also only normalizing the headers for nw because the N+N in the tanks are very low due to uptake

ChemData$HeaderTA.norm<-ChemData$HeaderTA+ChemData$NN

#normalize the TA data to salinity
ChemData$HeaderTA.norm<-ChemData$HeaderTA.norm*ChemData$HeaderSalinity/38
ChemData$TankTA.norm<-ChemData$TankTA*ChemData$TankSalinity/38

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

##NEED TO FILL IN ALL RESIDENCE TIMES WITH AN AVERAGE

## Sum up all the biological data by aquarium
Coral.Exp1Summary <- ddply(Coral, c("Aq_Ex1"), summarize,
                          SA = sum(SA, na.rm = T),
                          AFDW = sum(AFDW, na.rm = T),
                          DW = sum(DW, na.rm = T),
                          Volume = sum(Volume, na.rm = T)
)


Rubble.Exp1Summary <- ddply(Rubble, c("Aq_Ex1"), summarize,
                           SA = NA,
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

#join all the biology together
biology<-rbind(Rubble.Exp1Summary,Coral.Exp1Summary, Algae.Exp1Summary)

colnames(biology)[1]<-'Aquarium'

#Make one huge dataset with all the biology and chem data together
AllData<-merge(ChemData,biology, by='Aquarium', all.x=TRUE)

#Calculate NEC---------------------------------------------
AllData$NECExp1<-NECCalc(HeaderTA = AllData$HeaderTA.norm, TankTA = AllData$TankTA.norm, ResidenceTime = 2.7, SurfaceArea = AllData$DW)

plot(AllData$Time[AllData$NutLevel=='Ambient'& AllData$Substrate=='Rubble'],AllData$NECExp1[AllData$NutLevel=='Ambient'& AllData$Substrate=='Rubble'])
points(AllData$Time[AllData$NutLevel=='High'& AllData$Substrate=='Rubble'],AllData$NECExp1[AllData$NutLevel=='High'& AllData$Substrate=='Rubble'], col='red')
points(AllData$Time[AllData$NutLevel=='Med'& AllData$Substrate=='Rubble'],AllData$NECExp1[AllData$NutLevel=='Med'& AllData$Substrate=='Rubble'], col='blue')
