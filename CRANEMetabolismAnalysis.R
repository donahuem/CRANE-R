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

ChemData$HeaderTA.norm<-ChemData$HeaderTA+ChemData$NN

#normalize the TA data to salinity---THE SALINITY DATA IS CAUSING PROBLEMS.....
#ChemData$HeaderTA.norm<-ChemData$HeaderTA.norm*ChemData$HeaderSalinity/38
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


## Sum up all the biological data by aquarium
Coral.Exp1Summary <- ddply(Coral, c("Aq_Ex1"), summarize,
                          SA = sum(SA, na.rm = T),
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
                           SA = NA,
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(Volume, na.rm = T)
)

Rubble.Exp2Summary <- ddply(Rubble, c("Aq_Ex2"), summarize,
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

Algae.Exp2Summary <- ddply(Algae, c("Aq_Ex2"), summarize,
                           SA = sum(FinalSA, na.rm = T),
                           AFDW = sum(AFDW, na.rm = T),
                           DW = sum(DW, na.rm = T),
                           Volume = sum(FinalVol, na.rm = T)
)

Sand.Exp1Summary <- ddply(Sand, c("Aq_Ex1"), summarize,
                           SA = sum(SA, na.rm = T),
                           AFDW = NA,
                           DW = sum(DW, na.rm = T),
                           Volume = sum(Vol, na.rm = T)
)

Sand.Exp2Summary <- ddply(Sand, c("Aq_Ex2"), summarize,
                          SA = sum(SA, na.rm = T),
                          AFDW = NA,
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

#Make one huge dataset with all the biology and chem data together
AllData<-merge(AllData,biology, by='Aquarium', all.x=TRUE)

AllData$DateTime<-as.POSIXct(paste(AllData$Date, AllData$Time), format="%m/%d/%Y %H:%M")

#Calculate NEC---------------------------------------------
AllData$NECExp1<-NECCalc(HeaderTA = AllData$HeaderTA.norm, TankTA = AllData$TankTA.norm, ResidenceTime = AllData$ResTime.mean, SurfaceArea = AllData$DW)

sub<-unique(AllData$Substrate)
#sub <- sub[sub!="Mixed"]
#sub<-droplevels(sub)

NEC.mean <- ddply(AllData, c("Substrate","NutLevel","DateTime"), summarize,
                      Mean = mean(NECExp1, na.rm = T),
                      N=sum(!is.na(NECExp1)),
                      SE= sd(NECExp1, na.rm = T)/sqrt(N)
)


par(mfrow=c(2,2))
for (i in 1:5){
plot(AllData$DateTime[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]],AllData$NECExp1[AllData$NutLevel=='Ambient'& AllData$Substrate==sub[i]], main=sub[i])
points(AllData$DateTime[AllData$NutLevel=='High'& AllData$Substrate==sub[i]],AllData$NECExp1[AllData$NutLevel=='High'& AllData$Substrate==sub[i]], col='red')
points(AllData$DateTime[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]],AllData$NECExp1[AllData$NutLevel=='Med'& AllData$Substrate==sub[i]], col='blue')
}

par(mfrow=c(2,2))
for (i in 1:5){
  plot(NEC.mean$DateTime[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]], main=sub[i], type = 'b',
       ylim=c(min(NEC.mean$Mean[NEC.mean$Substrate==sub[i]])-0.2,max(NEC.mean$Mean[NEC.mean$Substrate==sub[i]])+0.2))
  error.bar(NEC.mean$DateTime[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]],
           NEC.mean$SE[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]],NEC.mean$SE[NEC.mean$NutLevel=='Ambient'& NEC.mean$Substrate==sub[i]])
  lines(NEC.mean$DateTime[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]], col='red', type = 'b')
  error.bar(NEC.mean$DateTime[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]],
            NEC.mean$SE[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]],NEC.mean$SE[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]])
  lines(NEC.mean$DateTime[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='Med'& NEC.mean$Substrate==sub[i]], col='blue', type = 'b')
  error.bar(NEC.mean$DateTime[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]],NEC.mean$Mean[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]],
            NEC.mean$SE[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]],NEC.mean$SE[NEC.mean$NutLevel=='High'& NEC.mean$Substrate==sub[i]])
  
}

Exp1Data<-AllData[AllData$Experiment==1 ,]
NECExp1.model <- lmer(NECExp1 ~ NutLevel*Time + Substrate + (1|Tank), data=Exp1Data)
#NECExp1.model <- lm(NECExp1 ~ NutLevel*Time + Tank + Substrate, data=Exp1Data)
anova(NECExp1.model)


#integrate under the curve to calculate net NEC.... 
Exp1Data$DateTime<-as.factor(Exp1Data$DateTime)

levels(Exp1Data$DateTime)<-c(1:7)
Exp1Data$DateTime <- droplevels(Exp1Data$DateTime)
Exp1Data$DateTime <- as.integer(Exp1Data$DateTime)

require(pracma)
AUC = trapz(Exp1Data$DateTime[1:7],AllData$NECExp1[1:7])

NEC.int<-NA
for (i in 1:36){
NEC.int[i]<-trapz(Exp1Data$DateTime[Exp1Data$Aquarium==i],Exp1Data$NECExp1[Exp1Data$Aquarium==i])
}

ddply(Exp1Data, c("Aquarium"), summarize,
      NEC.int=trapz(Exp1Data$DateTime,Exp1Data$NECExp1)
      )

##TA vs DIC plots
b<-matrix(nrow=5,ncol=3)
b0<-matrix(nrow=5,ncol=3)
r2<-matrix(nrow=5,ncol=3)
substrate<-matrix(nrow=5,ncol=3)
Nut<-matrix(nrow=5,ncol=3)
Nuts<-unique(AllData$NutLevel)
colors<-c('white','blue','magenta')

##make a contour plot for Deffey diagram
AT <- seq(2000e-6, 2341e-6, length.out=10)
DIC <- seq(1492e-6, 2300e-6, length.out=10)
dat <- expand.grid(AT, DIC)

carb <- carb(flag=15, var1=dat$Var1, var2=dat$Var2, S=35, T=25, P=0, Pt=0, Sit=0, k1k2="l", kf="pf", pHscale="T")
arag <- carb$OmegaAragonite
dim(arag) <- c(length(DIC), length(AT)) # 
#contour(DIC, AT, arag)

par(mfrow=c(1,1))
for(i in 1:5){
  
  #plot(NA, ylim=c(2100,2200),xaxt="n", xlab="DIC", ylab="TA", main = sub[i])
  #par(new = TRUE)
  filled.contour(DIC*1e6, AT*1e6, arag, 
                 xlab="DIC",
                 ylab="Total alkalinity",
                 key.title = title(main = expression(paste(Omega[arg]))),
                 levels=seq(0, 9, by=0.25),  
                 #labcex=1.5,
                 #method="edge",
                 col = rainbow(40),
                 lwd=2,
                 lty="solid",
                 main=sub[i]
                    
  )
   #calcification dissolution line
  x.DIC<-seq(from=1820, to= 1950, by  = 1)
  y.TA<-2*x.DIC-1600
  lines(x.DIC,y.TA, col='black', lwd=3)
  arrows(min(x.DIC),min(y.TA),max(x.DIC), max(y.TA), col='black', code=3,lwd=3)
  text(max(x.DIC)-15, max(y.TA)+20, 'Calcification')
  text(min(x.DIC)-15, min(y.TA)-20, 'Dissolution')
  #photosynthesis/respiration line
  x.DIC<-seq(from=1720, to= 2100, by  = 1)
  y.TA<-0.16*x.DIC +1850
  lines(x.DIC,y.TA, col='black', lwd=3)
  arrows(min(x.DIC),min(y.TA),max(x.DIC), max(y.TA), col='black', code=3,lwd=3)
  text(max(x.DIC)-15, max(y.TA)+20, 'Photosynthesis')
  text(min(x.DIC)-15, min(y.TA)-20, 'Respiration')
    #abline(h=0)
   # par(new = TRUE)
    #plot(AllData$TankDIC[ AllData$Substrate==sub[i]],AllData$TankTA[ AllData$Substrate==sub[i]], main=sub[i], 
     #     type = 'p', col = colors[j], pch=19, yaxt="n", ylab="", xlab="")
    for (j in 1:3){
      #par(new = TRUE)
    points(AllData$TankDIC[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]],AllData$TankTA[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]], main=sub[i], 
           type = 'p', col = colors[j], pch=19)
    
    
    modelTA.DIC<-lm(AllData$TankTA[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]]  ~AllData$TankDIC[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]])
    b0[i,j]<-modelTA.DIC$coefficients[1]
    b[i,j]<-modelTA.DIC$coefficients[2]
    r2[i,j]<-summary(modelTA.DIC)$r.squared
    substrate[i,j]<-sub[i]
    Nut[i,j]<-Nuts[j]
    #x<-seq(from=1500, to=2100,by=0.5)
    y<-b[i,j]*x+b0[i,j]
    lines(x,y, col=colors[j], lwd=3)
    
    
    
    }
legend('topleft',legend=c('Ambient',"Medium","High"), col=c('blue','magenta','white'), pch=19, bty = 'n')
}


##make a contour plot
AT <- seq(2080e-6, 2241e-6, length.out=10)
DIC <- seq(1592e-6, 2068e-6, length.out=10)
dat <- expand.grid(AT, DIC)

carb <- carb(flag=15, var1=dat$Var1, var2=dat$Var2, S=35, T=25, P=0, Pt=0, Sit=0, k1k2="l", kf="pf", pHscale="T")


# Do a simple contour plot
arag <- carb$OmegaAragonite
dim(arag) <- c(length(DIC), length(AT)) # 
contour(DIC, AT, arag)


filled.contour(DIC*1e6, AT*1e6, arag, 
        xlab="DIC",
        ylab="Total alkalinity (umol/kg)",
        levels=seq(1, 7, by=0.25),  
        labcex=1.5,
        method="edge",
        col = rainbow(40),
        lwd=2,
        lty="solid"
)
par(new = TRUE)
image(DIC*1e6, AT*1e6, arag, col=rainbow(6),useRaster=TRUE,
      xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
      main = "Surface elevation data")
box()

levelplot(arag ~ DIC*1e6 + AT*1e6, 
          xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
          main = "Surface elevation data",
          col.regions = terrain.colors(100)
)