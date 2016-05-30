#Data Processing for Weights, Volumes, and Surface Areas

library(googlesheets)
suppressMessages(library(dplyr))
library(gdata)

#authorize access to google sheets
gs_auth(token=NULL, new_user=FALSE,key = getOption("googlesheets.client_id"),secret = getOption("googlesheets.client_secret"),
        cache = getOption("googlesheets.httr_oauth_cache"), verbose = TRUE)

#CRANE - Coral Processing
#Coral DW & AFDW
Coral_Wts_sheet <- gs_key("1UXNqCK_xTOqMluSi-7VdIZ5m7Dcr3CERy6txjbvMduo", lookup=TRUE)
Coral_Wts_raw <- gs_read(Coral_Wts_sheet)

#CRANE_sheet has BW for CoralSets, Rubble, Sand; normalizations for BW; CoralSet holder volumes; WW plus SA and Vol conversions for algae;Expt IDs
CRANE_sheet <- gs_key("1Fks1OzSzpRkNe1X6u9Ez1W6qHmPmTgxR0scueF3OARA",lookup=TRUE)

CRANE_coralnubb <- gs_read(CRANE_sheet,ws="CoralNubbin")
CRANE_coralset <- gs_read(CRANE_sheet,ws="CoralSet")
CRANE_BWnorms <- gs_read(CRANE_sheet,ws="BW_Normalization")
Calibration <- read.csv("../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/buoyant weight calibration_curves.csv",header=TRUE)

TrayWts_sheet <- gs_key("1IePBN4KmpqXHjJLy0201li2MjZDwXD3JchqznrrKtrk", lookup=TRUE)
TrayWts_raw <- gs_read(TrayWts_sheet,range = "A1:B500")

#get codes for experiments
CRANE_IDexpt1 <- gs_read(CRANE_sheet,ws="Experiment1ID")
coralIDex1 <- subset(CRANE_IDexpt1,Substrate=="Coral")
CRANE_IDexpt2 <- gs_read(CRANE_sheet,ws="Experiment2ID")
coralIDex2 <- subset(CRANE_IDexpt2,Substrate=="Coral")

#SA: get wax dip weights and Wax dipping normalizations
WaxWts_sheet <- gs_key("18bC8SBWhdq1tSiThx4HxDjzCkEyfq9zkEU2qtSs73qI")
WaxWts_raw <- gs_read(WaxWts_sheet,range = "A1:C300")
WaxtoSA_sheet <-gs_key("1P_IP3Csn0XCjwOeGdu1_6pXJth7B5K1w2xnPZiHpseE")
WaxtoSA_raw <- gs_read(WaxtoSA_sheet)
SAmod <- lm(WaxtoSA_raw$`SA(mm2)`~ 0 + WaxtoSA_raw$`Delta Weight`)
WaxConversionFactor <- SAmod$coefficients[1]/100  #from Jess' 6 wax dips of dowel standards
  #aggregate samples with multiple pieces
WaxWts_byNubb <- aggregate(cbind(WaxWts_raw$`Initial Weight`,WaxWts_raw$`Final Weight`)~WaxWts_raw$`Tray #`,FUN=sum)
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="V1"]<-"InitWt"
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="V2"]<-"FinalWt"
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="WaxWts_raw$`Tray #`"]<-"TrayNum"
WaxWts_byNubb$SA <- with(WaxWts_byNubb,(FinalWt-InitWt)*WaxConversionFactor)

#BW Calibration Calculations
source("DryWeightCalcFromBW.R")

#Create dataframe for Nubbin-scale measures
CoralNubb <- with(CRANE_coralnubb,data.frame(SampleID, Species, Clone,Volume,HPC,PercentBleached,PercentDead,TrayNum))
CoralNubb$TrayWt <- TrayWts_raw$TrayWt[match(CoralNubb$TrayNum,TrayWts_raw$TrayNum)]
CoralNubb$DW <- Coral_Wts_raw$`Dry Wt w Tray`[match(CoralNubb$TrayNum,Coral_Wts_raw$TrayNum)]-CoralNubb$TrayWt
CoralNubb$AW <- Coral_Wts_raw$`Ash Wt w Tray`[match(CoralNubb$TrayNum,Coral_Wts_raw$TrayNum)]-CoralNubb$TrayWt
CoralNubb$AFDW <- CoralNubb$DW - CoralNubb$AW
CoralNubb$pcAFDW <- (CoralNubb$DW - CoralNubb$AW)/CoralNubb$DW
CoralNubb$SA <- WaxWts_byNubb$SA[match(CoralNubb$TrayNum,WaxWts_byNubb$TrayNum)]
CoralNubb$Check <- NA
CoralNubb$Compromised <- (CoralNubb$HPC=="C"|CoralNubb$HPC=="PC"|CoralNubb$HPC=="CP")
CoralNubb$Aqua <- coralIDex1$Aquarium[match(CoralNubb$SampleID,coralIDex1$SampleID)]
CoralNubb$Tank <- coralIDex1$BlackTank[match(CoralNubb$SampleID,coralIDex1$SampleID)]
CoralNubb$Nuts <- coralIDex1$NutLevel[match(CoralNubb$SampleID,coralIDex1$SampleID)]
CoralNubb$Nuts <- factor(CoralNubb$Nuts,level=c("Ambient","Medium","High"))
#Check for outliers in nubbin data
#Trays 233 is outliers in AFDW; 177 may be an outlier
plot(CoralNubb$SA ~ CoralNubb$AFDW,col=CoralNubb$Species,type="n")
text(x=CoralNubb$AFDW,y=CoralNubb$SA,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$Vol ~ CoralNubb$AW,col=CoralNubb$Species,type="n")
text(x=CoralNubb$Vol,y=CoralNubb$AFDW,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$Vol ~ CoralNubb$AW,col=CoralNubb$Species,type="n")
text(jitter(CoralNubb$Vol),y=CoralNubb$AFDW,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$SA ~ CoralNubb$Volume,col=CoralNubb$Species,type="n")
text(x=CoralNubb$Volume,y=CoralNubb$SA,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

CoralNubb$Check[CoralNubb$TrayNum==233]<- "Outlier in AFDW"
CoralNubb$Check[CoralNubb$TrayNum==177]<- "Low outlier in SA"

write.table(CoralNubb,file="../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/CoralNubbins_Rprocessed.csv",sep = " ,", col.names = NA)


#Create dataframe for set-scale measures (coral set = 3 colonies mounted together)
CoralSet <- with(CRANE_coralset, data.frame(SampleID,Substrate,Species,InitialBW,InitialTemp,InitialDate,FinalBW,FinalTemp,FinalDate))
StopperAir <- mean(CRANE_BWnorms$Weight[CRANE_BWnorms$Medium=="AIR"])
CoralSet$InitBW <- BWCalc(StopperAir,CoralSet$InitialTemp, CoralSet$InitialBW)
CoralSet$FinalBW <- BWCalc(StopperAir,CoralSet$FinalTemp, CoralSet$FinalBW)
CoralSet$DeltaBW <- CoralSet$FinalBW - CoralSet$InitBW
CoralSet$pcDeltaBW <- (CoralSet$FinalBW - CoralSet$InitBW)/CoralSet$InitBW

agg<-aggregate(cbind(Volume,SA,DW,AW,AFDW) ~ Species+SampleID,FUN=sum,data=CoralNubb)
CoralSet$Volume <- agg$Volume[match(interaction(CoralSet$SampleID,CoralSet$Species),interaction(agg$SampleID,agg$Species))]
CoralSet$SA <- agg$SA[match(interaction(CoralSet$SampleID,CoralSet$Species),interaction(agg$SampleID,agg$Species))]
CoralSet$AW <- agg$AW[match(interaction(CoralSet$SampleID,CoralSet$Species),interaction(agg$SampleID,agg$Species))]
CoralSet$DW <- agg$DW[match(interaction(CoralSet$SampleID,CoralSet$Species),interaction(agg$SampleID,agg$Species))]
CoralSet$AFDW <- agg$AFDW[match(interaction(CoralSet$SampleID,CoralSet$Species),interaction(agg$SampleID,agg$Species))]
CoralSet$pcAFDW <- CoralSet$AFDW/CoralSet$DW
###NOTE:  Wax Dipping was done with the intention of capturing live coral tissue SA when a line was evident between 
#dead and live tissue.  However, for the "all dead" nubbins, the full SA is probably given.  Check these.

# Add aquarium, tank, and treatment codes
CoralSet$Nuts <- coralIDex1$NutLevel[match(CoralSet$SampleID,coralIDex1$SampleID)]
CoralSet$Tank <- coralIDex1$BlackTank[match(CoralSet$SampleID,coralIDex1$SampleID)]
CoralSet$Aq_Ex1 <- coralIDex1$Aquarium[match(CoralSet$SampleID,coralIDex1$SampleID)]
CoralSet$Aq_Ex2 <- coralIDex2$Aquarium[match(CoralSet$SampleID,coralIDex2$SampleID)]
CoralSet$Check <- NA


#MC15 and PC14 are outliers in AFDW; PC22 may be low in AFDW
plot(Volume ~SA,data=CoralSet,type="n")
text(x=CoralSet$SA,y=CoralSet$Volume,labels=CoralSet$SampleID,col=as.numeric(CoralSet$Species),cex=0.7)
plot(AFDW ~SA,col=Species, data=CoralSet)
plot(Volume ~AW, col=Species, data=CoralSet)
CoralSet$Check[CoralSet$Species=="Porites" & CoralSet$SampleID==22] <- "low in AFDW?"
CoralSet$Check[CoralSet$Species=="Porites" & CoralSet$SampleID==14] <- "outlier in AFDW?"
CoralSet$Check[CoralSet$Species=="Montipora" & CoralSet$SampleID==15] <- "outlier in AFDW?"

write.table(CoralSet, file="../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/CoralSets_Rprocessed.csv", sep=",", col.names = NA)

#Assemble Rubble Data
#Rubble BWs
CRANE_rubble <- gs_read(CRANE_sheet,ws="RUBBLE")
Rubble <- with(CRANE_rubble,data.frame(SampleID,InitialTemp,FinalTemp,Volume,TrayNum, deltaSAWt))
Rubble$InitBW <- BWCalc(StopperAir,Rubble$InitialTemp, CRANE_rubble$`InitialBW (g)`)
Rubble$FinalBW <- BWCalc(StopperAir,Rubble$FinalTemp, CRANE_rubble$`FinalBw (g)`)
Rubble$DeltaBW <- Rubble$FinalBW - Rubble$InitBW
Rubble$pcDeltaBW <- (Rubble$FinalBW - Rubble$InitBW)/Rubble$InitBW

#Rubble DW & AFDW
Rubble_Wts_sheet <- gs_key("1eD0DqrBoojarmvJxPT3r5ntcCVnfy6vEbYofk9wj3Mc", lookup=TRUE)
Rubble_Wts_raw <- gs_read(Rubble_Wts_sheet)
Rubble$TrayWt <- TrayWts_raw$TrayWt[match(Rubble$TrayNum,TrayWts_raw$TrayNum)]
Rubble$DW <- Rubble_Wts_raw$`Dry Wt w Tray`[match(Rubble$TrayNum,Rubble_Wts_raw$`Tray #`)]-Rubble$TrayWt
Rubble$AW <- Rubble_Wts_raw$`Ash Wt w Tray`[match(Rubble$TrayNum,Rubble_Wts_raw$`Tray #`)]-Rubble$TrayWt
Rubble$AFDW <- Rubble$DW - Rubble$AW
Rubble$pcAFDW <- Rubble$AFDW/Rubble$DW
#Rubble SA
Rubble$SA<-Rubble$deltaSAWt*WaxConversionFactor


rubbleIDex1 <- subset(CRANE_IDexpt1,Substrate=="Rubble")
rubbleIDex2 <- subset(CRANE_IDexpt2,Substrate=="Rubble")
Rubble$Nuts <- rubbleIDex1$NutLevel[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Tank <- rubbleIDex1$BlackTank[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Aq_Ex1 <- rubbleIDex1$Aquarium[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Aq_Ex2 <- rubbleIDex2$Aquarium[match(Rubble$SampleID,rubbleIDex2$SampleID)]

plot(DW~AW,data=Rubble)
points(x=Rubble$AW[Rubble$TrayNum==120],y=Rubble$DW[Rubble$TrayNum==120],col="red")

plot(Volume ~SA,data=Rubble,type="n")
text(x=Rubble$SA,y=Rubble$Volume,labels=Rubble$SampleID,cex=0.7)
plot(AFDW ~SA, data=Rubble)

write.table(Rubble,file="../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Rubble_Rprocessed.csv",sep=",", col.names = NA)

#Algae Data
CRANE_algae <- gs_read(CRANE_sheet,ws="ALGAE")
CRANE_AlgalVol <- gs_read(CRANE_sheet,ws="AlgalVol")
CRANE_AlgalSA <- gs_read(CRANE_sheet,ws="AlgalSAcalcs")
Algae <- with(CRANE_algae,data.frame(SampleID,InitialWW,FinalWW,BitsWW,TrayNum1,TrayNumBits))
Algae$DeltaWW <- Algae$FinalWW - Algae$InitialWW
Algae$pcDeltaWW <- (Algae$FinalWW - Algae$InitialWW)/Algae$InitialWW

algaeIDex1 <- subset(CRANE_IDexpt1,Substrate=="Algae")
algaeIDex2 <- subset(CRANE_IDexpt2,Substrate=="Algae")
Algae$Nuts <- algaeIDex1$NutLevel[match(Algae$SampleID,algaeIDex1$SampleID)]
Algae$Tank <- algaeIDex1$BlackTank[match(Algae$SampleID,algaeIDex1$SampleID)]
Algae$Aq_Ex1 <- algaeIDex1$Aquarium[match(Algae$SampleID,algaeIDex1$SampleID)]
Algae$Aq_Ex2 <- algaeIDex2$Aquarium[match(Algae$SampleID,algaeIDex2$SampleID)]

Algae_Wts_sheet <- gs_key("1MrX_EzFiW87ggTmqnhMU7pa5wq5TEevj47xqasqp3bA", lookup=TRUE)
Algae_Wts_raw <- gs_read(Algae_Wts_sheet)
Algae$TrayWt <- TrayWts_raw$TrayWt[match(Algae$TrayNum1,TrayWts_raw$TrayNum)]
Algae$DW <- Algae_Wts_raw$`DW with Tray`[match(Algae$TrayNum1,Algae_Wts_raw$Tray)]-Algae$TrayWt
Algae$AW <- Algae_Wts_raw$`AW with Tray`[match(Algae$TrayNum1,Algae_Wts_raw$Tray)]-Algae$TrayWt
Algae$AFDW <- Algae$DW - Algae$AW
Algae$pcAFDW <- Algae$AFDW/Algae$DW

#Algae WW are tightly related to volume; assume Gsal is a cynlinder, then SA is estimable from cross-sectional radius and volume
AlgVol_lm <- lm(CRANE_AlgalVol$`Volume of Algae`~CRANE_AlgalVol$`Algae Wet Weight (g)`-1)
AlgRadius <- mean(CRANE_AlgalSA$`Algal cross section diameters (mm)`)/2/10 #convert to cm
Algae$FinalVol <- Algae$FinalWW*coef(AlgVol_lm)
Algae$BitsVol <- Algae$BitsWW*coef(AlgVol_lm)
Algae$FinalSA <- 2*Algae$FinalVol/AlgRadius  + 2*pi*AlgRadius^2
Algae$BitsSA <- 2*Algae$BitsVol/AlgRadius  + 2*pi*AlgRadius^2

plot(DW~AW,type="n",data=Algae)
text(x=Algae$AW,y=Algae$DW,labels=Algae$SampleID,cex=0.7)

plot(AFDW~AFDW,type="n",data=Algae)
text(x=Algae$AW,y=Algae$AFDW,labels=Algae$SampleID,cex=0.7)

write.table(Algae,file="../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Algae_Rprocessed.csv",sep=",", col.names = NA)

#Sand
CRANE_sand <- gs_read(CRANE_sheet,ws="SAND")
Sand <- with(CRANE_sand,data.frame(SampleID,InitialBW,InitialTemp,PercentOutside,MeasVol))
Sand_Wts_Sheet <- gs_key("1Tvh1nzn5Bpz73yED5FsSS2tLFcyNPYNf6pdPF7orOew", lookup=TRUE)
Sand_Wts_raw <- gs_read(Sand_Wts_Sheet,ws="DW AFDW etc")
Sand$DW <- Sand_Wts_raw$`DW in Tray`[Sand_Wts_raw$TrayNum<100] - Sand_Wts_raw$`TrayWt`[Sand_Wts_raw$TrayNum<100]
Sand$SA <- pi *(8.2/2)^2  #lid of petri dish is 82mm; get SA in cm2
sandvol <- lm(Sand$MeasVol~Sand$DW-1)
plot(Sand$MeasVol ~ Sand$DW)
abline(0,coef(sandvol))
Sand$Vol <- Sand$DW*coef(sandvol)
Sand$AW <- Sand_Wts_raw$`AW in Tray`[Sand_Wts_raw$TrayNum<100]- Sand_Wts_raw$TrayWt[Sand_Wts_raw$TrayNum<100]
Sand$AFDW <- Sand$DW - Sand$AW
Sand$pcAFDW <- Sand$AFDW/Sand$DW

SandIDex1 <- subset(CRANE_IDexpt1,Substrate=="Sand")
SandIDex2 <- subset(CRANE_IDexpt2,Substrate=="Sand")
Sand$Nuts <- SandIDex1$NutLevel[match(Sand$SampleID,SandIDex1$SampleID)]
Sand$Tank <- SandIDex1$BlackTank[match(Sand$SampleID,SandIDex1$SampleID)]
Sand$Aq_Ex1 <- SandIDex1$Aquarium[match(Sand$SampleID,SandIDex1$SampleID)]
Sand$Aq_Ex2 <- SandIDex2$Aquarium[match(Sand$SampleID,SandIDex2$SampleID)]

write.table(Sand,file="../../Google Drive/CRANE shared folder/Data/Weights, Volumes & SAs/Sand_Rprocessed.csv", sep=",", col.names = NA)


# #Aquarium dataframe for Expts 1 & 2; these is the list of columns needed
# AquaEx1 <- data.frame(Aqua = c(1:36))
# AquaEx1$Nuts <- CoralSet$Nuts[match(AquaEx1$Aqua,CoralSet$Aq_Ex1)]
# a <- aggregate(CoralSet$AFDW,by=list(CoralSet$Aq_Ex1),FUN="sum",subset=(CoralSet$Species=="Montipora"))
# AquaEx1$CoralAFDW_mc <- a$x[match(CoralSet$Aq_Ex1[CoralSet$Species=="Montipora"],a$Group.1)]
# 
# AquaEx1$CoralAFDW_pc
# AquaEx1$CoralAFDW_tot
# AquaEx1$CoralSA_mc
# AquaEx1$CoralSA_pc
# AquaEx1$CoralSA_tot
# AquaEx1$CoralVol_mc
# AquaEx1$CoralVol_pc
# AquaEx1$CoralVol_tot
# AquaEx1$AlgaeAFDW
# AquaEx1$AlgaeSA
# AquaEx1$AlgaeVol
# AquaEx1$RubbleAFDW
# #AquaEx1$RubbleSA
# AquaEx1$RubbleVol
# AquaEx1$SandAFDW
# AquaEx1$SandSA <- pi*(.82/2)^2      #petri dish cover is 82mm diameter
# AquaEx1$SandVol

#Bring in Curated Nutrient Data for Plotting
Nutrients_sheet <- gs_key("1j2ZjBC0VzObUr2CzeBvUgtkD2Aw8Fb_II-6N9gpI1B0", lookup=TRUE)
Nutrients_raw <- gs_read(Nutrients_sheet)
Nutrients <- with(Nutrients_raw,data.frame(`Nominal Timepoint`,Treatment,Tank,`N+N`,Phosphate, `Substrate/Experiment`))
Nutrients <- subset(Nutrients,subset=(Nutrients$Treatment == "N0" | Nutrients$Treatment=="N2"| Nutrients$Treatment=="N4"))
Nutrients <- subset(Nutrients,subset=(Nutrients$Nominal.Timepoint <40))
Nutrients <- subset(Nutrients,subset=(Nutrients$N.N<20))  #exclude point that is at 80
revalue(Nutrients$Substrate.Experiment,c("Sediment"="Sand"))
Nutrients$Treatment<-factor(Nutrients$Treatment)
Nutrients$Tank <- factor(Nutrients$Tank)
Nutrients_CH <-subset(Nutrients,subset=(Nutrients$Substrate.Experiment=="CHAIN"))
Nutrients_CARS <- subset(Nutrients,subset=(Nutrients$Substrate.Experiment=="Algae" | Nutrients$Substrate.Experiment=="Coral" |Nutrients$Substrate.Experiment=="Rubble"|Nutrients$Substrate.Experiment=="Sediment"))
Nutrients_CH$Substrate.Experiment <- factor(Nutrients_CH$Substrate.Experiment)
Nutrients_CARS$Substrate.Experiment <- factor(Nutrients_CARS$Substrate.Experiment)

