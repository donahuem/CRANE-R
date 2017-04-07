#Data Processing for Weights, Volumes, and Surface Areas

#location for writing processed files
fileloc <- "C:/Users/Megan/Google Drive/CRANE/CRANE shared folder/Data/Weights, Volumes & SAs/"

library(googlesheets)
suppressMessages(library(dplyr))
library(plyr)
library(gdata)

#authorize access to google sheets
gs_auth(token=NULL, new_user=FALSE,key = getOption("googlesheets.client_id"),secret = getOption("googlesheets.client_secret"),
        cache = getOption("googlesheets.httr_oauth_cache"), verbose = TRUE)

#CRANE - Coral Processing
#Filename is "Coral DW & AFDW"
Coral_Wts_sheet <- gs_key("1UXNqCK_xTOqMluSi-7VdIZ5m7Dcr3CERy6txjbvMduo", lookup=TRUE)
Coral_Wts_raw <- gs_read(Coral_Wts_sheet)

#CRANE_sheet has BW for CoralSets, Rubble, Sand; normalizations for BW; CoralSet holder volumes; WW plus SA and Vol conversions for algae;Expt IDs
#Filename is "CRANE_BWs_Vols_SAs_ExptCodes"
CRANE_sheet <- gs_key("1Fks1OzSzpRkNe1X6u9Ez1W6qHmPmTgxR0scueF3OARA",lookup=TRUE)

CRANE_coralnubb <- gs_read(CRANE_sheet,ws="CoralNubbin")
CRANE_coralset <- gs_read(CRANE_sheet,ws="CoralSet")
CRANE_BWnorms <- gs_read(CRANE_sheet,ws="BW_Normalization")
Calibration <- read.csv(paste(fileloc,"buoyant weight calibration_curves.csv",sep=""),header=TRUE)

TrayWts_sheet <- gs_key("1IePBN4KmpqXHjJLy0201li2MjZDwXD3JchqznrrKtrk", lookup=TRUE)
TrayWts_raw <- gs_read(TrayWts_sheet,range = "A1:B500")

#get codes for experiments
CRANE_IDexpt1 <- gs_read(CRANE_sheet,ws="Experiment1ID")
coralIDex1 <- subset(CRANE_IDexpt1,Substrate=="Coral")
CRANE_IDexpt2 <- gs_read(CRANE_sheet,ws="Experiment2ID")
coralIDex2 <- subset(CRANE_IDexpt2,Substrate=="Coral")

#SA: get wax dip weights and Wax dipping normalizations for Coral
WaxWts_sheet <- gs_key("18bC8SBWhdq1tSiThx4HxDjzCkEyfq9zkEU2qtSs73qI")  #filename:  Coral SA_jks
WaxWts_raw <- gs_read(WaxWts_sheet,range = "A1:C300")
WaxtoSA_sheet <-gs_key("1P_IP3Csn0XCjwOeGdu1_6pXJth7B5K1w2xnPZiHpseE") #filename: SA measurements for wax dipping
WaxtoSA_raw <- gs_read(WaxtoSA_sheet)
SAmod <-lm(WaxtoSA_raw$`SA(mm2)`~ 0 + WaxtoSA_raw$`Delta Weight`) #suspect that radius column is really diameter
WaxConversionFactor <- SAmod$coefficients[1]/100  #from Jess' 6 wax dips of dowel standards; divide by 100 for cm2
  #aggregate samples with multiple pieces
WaxWts_byNubb <- aggregate(cbind(WaxWts_raw$`Initial Weight`,WaxWts_raw$`Final Weight`)~WaxWts_raw$`Tray #`,FUN=sum)
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="V1"]<-"InitWt"
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="V2"]<-"FinalWt"
names(WaxWts_byNubb)[names(WaxWts_byNubb)=="WaxWts_raw$`Tray #`"]<-"TrayNum"
WaxWts_byNubb$SA <- with(WaxWts_byNubb,(FinalWt-InitWt)*WaxConversionFactor)
WaxWts_byNubb.labels <- c(InitWt = "Weight of coral nubbin before wax dipping (including Tray Wt) (g)", 
                         FinalWt = "Weight of coral nubbin after wax dipping (including Tray Wt) (g)",
                         TrayNum = "Tray Number for coral nubbin",
                         SA = paste("Surface Area in cm2:  SA = (FinalWt-InitWt)*",round(WaxConversionFactor,3),"(cm2/g)","; coefficient is from regression of 6 wax dipped dowels of known SA, Rsq=0.99"))
label(WaxWts_byNubb) = lapply(names(WaxWts_byNubb.labels),
                              function(x) label(WaxWts_byNubb[,x]) = WaxWts_byNubb.labels[x])

#BW Calibration Calculations
source("DryWeightCalcFromBW.R")

#Create dataframe for Nubbin-scale measures
CoralNubb <- with(CRANE_coralnubb,data.frame(SampleID, Species, Clone,Volume,HPC,PercentBleached,PercentDead,TrayNum))
names(CoralNubb)[names(CoralNubb)=="Clone"] <- "Colony"  #rename Clone to Colony b/c we didn't genotype these colonies
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

CoralNubb.labels <- c(SampleID = "Sample Identifier for each set of coral nubbins",
                      Species = "Porites compressa or Montipora capitata",
                      Colony = "Colony Identifier (1-3) for each species",
                      Volume = "Volume of Coral Nubbin (cm3) measured by displacement to nearest cm3",
                      HPC = "Health Status of Nubbin: Healthy(H) - no apparent paling or lesions; Pale(P) - notable paling or bleaching by visual inspection; or Compromised (C) - diseased lesions or overgrowth by filamentous algae",
                      PercentBleached = "Percent of Nubbin Area that is bleached",
                      PercentDead = "Percent of Nubbin Area that is dead",
                      TrayNum = "Tray Identifier for tray used with this nubbin",
                      TrayWt = "Tray Weight (g) associated with this Tray Number",
                      DW = "Dry Weight (g) is the nubbin dry weight after oven-drying (60deg C for >=72 hrs)",
                      AW = "Weight (g) of nubbin after combustion at 550C  for 4 hours",
                      AFDW = "AFDW = DW - AW; Weight (g) of organic matter lost on combustion",
                      pcAFDW = "Percent Ash Free Dry Weight = AFDW/DW; this is the percent of the total dry weight that is organic matter",
                      SA = "Surface Area (cm3); surface area of nubbin based on wax dipping",
                      Check = "Flag if measurement appears to be outlier",
                      Compromised = "Flags all colonies categorized as Compromised by HPC",
                      Aqua = "Aquarium Identifier for Experiment 1; nine aquaria held coral nubbins in expt 1",
                      Tank = "Tank Identifier for Experiment 1; each aquarium was assigned to one of 3 tanks",
                      Nuts = "Nutrient level assigned to the aquarium that contained this nubbin")
label(CoralNubb) = lapply(names(CoralNubb.labels),
                              function(x) label(CoralNubb[,x]) = CoralNubb.labels[x])

#Check for outliers in nubbin data
#Trays 233 is outliers in AFDW; 177 may be an outlier
plot(CoralNubb$SA ~ CoralNubb$AFDW,col=CoralNubb$Species,type="n")
text(x=CoralNubb$AFDW,y=CoralNubb$SA,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$Vol ~ CoralNubb$AW,col=CoralNubb$Species,type="n")
text(x=CoralNubb$Vol,y=CoralNubb$AW,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$Vol ~ CoralNubb$AW,col=CoralNubb$Species,type="n")
text(jitter(CoralNubb$Vol),y=CoralNubb$AW,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

plot(CoralNubb$SA ~ CoralNubb$Vol,col=CoralNubb$Species,type="n")
text(x=CoralNubb$Vol,y=CoralNubb$SA,label=as.character(CoralNubb$TrayNum),cex=.5,col=as.numeric(CoralNubb$Species))

CoralNubb$Check[CoralNubb$TrayNum==233]<- "Outlier in AFDW"
CoralNubb$Check[CoralNubb$TrayNum==177]<- "Low outlier in SA"

write.table(CoralNubb,file=paste(fileloc,"CoralNubbins_Rprocessed.csv",sep=""),sep = " ,", col.names = NA)
write.table(CoralNubb.labels,file=paste(fileloc,"CoralNubbins_metadata.csv",sep=""),sep=" ,")

#Create dataframe for set-scale measures (coral set = 3 colonies mounted together)
CoralSet <- with(CRANE_coralset, data.frame(SampleID,Species,InitialBW,InitialTemp,InitialDate,FinalBW,FinalTemp,FinalDate))
StopperAir <- mean(CRANE_BWnorms$Weight[CRANE_BWnorms$Medium=="AIR"])
CoralSet$InitBWcorr <- BWCalc(StopperAir,CoralSet$InitialTemp, CoralSet$InitialBW)
CoralSet$FinalBWcorr <- BWCalc(StopperAir,CoralSet$FinalTemp, CoralSet$FinalBW)
CoralSet$DeltaBW <- CoralSet$FinalBWcorr - CoralSet$InitBWcorr
CoralSet$pcDeltaBW <- (CoralSet$FinalBWcorr - CoralSet$InitBWcorr)/CoralSet$InitBWcorr

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

#Metadata for CoralSet
CoralSet.labels <- c(SampleID = "Sample Identifier; single identifier for set of 3 Porites nubbins and 3 Montipora nubbins",
                     Species = "Porites compressa or Montipora capitata",
                     InitialBW = "Initial Buoyant Weight (g) of 3 nubbins",
                     InitialTemp = "Temperature (C) at time of initial buoyant weighing",
                     InitialDate = "Date of initial buoyant weighing",
                     FinalBW = "Final Buoyant Weight (g) of 3 nubbins",
                     FinalTemp = "Temperature (C) at time of final buoyant weighing",
                     FinalDate = "Date of final buoyant weighing",
                     InitBWcorr =  "Initial Dry Weight of coral set based on buoyant wt; calculated from InitialBW and InitialTemp based on Davies (1989) and a coral density of 2.93 g/cm3",
                     FinalBWcorr ="Final Dry Weight of coral set based on buoyant wt; calculated from FinalBW and FinalTemp based on Davies (1989) and a coral density of 2.93 g/cm3",
                     DeltaBW = "DeltaBW = FinalBWcorr - InitBWcorr; this is the change in dry weight over the 6 wk experiment based on buoyant weights",
                     pcDeltaBW = "pcDeltaBW = DeltaBW/InitBWcorr; this is the percent change in dry weight based on buoyant weight",
                     Volume = "Total volume (cm3) of 3 nubbins based on displacement; from dataframe CoralNubb",
                     SA = "Total surface area (cm2) of 3 nubbins based on displacement; summed from dataframe CoralNubb",
                     AW = "Total ashed weight (g) of 3 nubbins based on displacement; summed from dataframe CoralNubb",
                     DW= "Total dry weight (g) of 3 nubbins based on displacement; summed from dataframe CoralNubb",
                     AFDW = "Total ash-free dry weight (g) of 3 nubbins based on displacement; summed from dataframe CoralNubb",
                     pcAFDW = "AFDW/DW; organic matter as a percent of total dry weight",
                     Nuts = "Nutrient level assigned to the aquarium: Ambient/Medium/High",
                     Tank = "Tank Identifier; each aquarium was assigned to one of 3 tanks",
                     Aq_Ex1 = "Aquarium Identifier for Experiment 1; nine aquaria held coral nubbins in expt 1",
                     Aq_Ex2 = "Aquarium Identifier for Experiment 2; all aquaria had coral nubbins in expt 2",
                     Check = "Flag if measurement appears to be outlier")


#MC15 and PC14 are outliers in AFDW; PC22 may be low in AFDW
plot(Volume ~SA,data=CoralSet,type="n")
text(x=CoralSet$SA,y=CoralSet$Volume,labels=CoralSet$SampleID,col=as.numeric(CoralSet$Species),cex=0.7)
plot(AFDW ~SA,col=Species, data=CoralSet)
plot(Volume ~AW, col=Species, data=CoralSet)
CoralSet$Check[CoralSet$Species=="Porites" & CoralSet$SampleID==22] <- "low in AFDW?"
CoralSet$Check[CoralSet$Species=="Porites" & CoralSet$SampleID==14] <- "outlier in AFDW?"
CoralSet$Check[CoralSet$Species=="Montipora" & CoralSet$SampleID==15] <- "outlier in AFDW?"

write.table(CoralSet, file=paste(fileloc,"CoralSets_Rprocessed.csv",sep=""), sep=",", col.names = NA)
write.table(CoralSet.labels,file=paste(fileloc,"CoralSets_metadata.csv",sep=""),sep=" ,")

#Assemble Rubble Data
#Rubble BWs
#3 outliers in BWs: 324 low; 330 and 312 high;  correct from original datasheets
# probably lost or gained a small chunk during expt
#should probably exclude these three values from the BW analysis
CRANE_rubble <- gs_read(CRANE_sheet,ws="RUBBLE")
Rubble <- with(CRANE_rubble,data.frame(SampleID,InitialTemp,FinalTemp,Volume,TrayNum, deltaSAWt))
Rubble$InitBW <- BWCalc(StopperAir,Rubble$InitialTemp, CRANE_rubble$`InitialBW (g)`)
Rubble$FinalBW <- BWCalc(StopperAir,Rubble$FinalTemp, CRANE_rubble$`FinalBw (g)`)
Rubble$DeltaBW <- Rubble$FinalBW - Rubble$InitBW
Rubble$DeltaBW_all <- Rubble$DeltaBW
Rubble$DeltaBW[Rubble$SampleID == 324 | Rubble$SampleID == 330 | Rubble$SampleID == 312] <- NA
Rubble$pcDeltaBW <- Rubble$DeltaBW/Rubble$InitBW

#Rubble DW & AFDW
Rubble_Wts_sheet <- gs_key("1eD0DqrBoojarmvJxPT3r5ntcCVnfy6vEbYofk9wj3Mc", lookup=TRUE)
Rubble_Wts_raw <- gs_read(Rubble_Wts_sheet,ws="Weights")
Rubble$TrayWt <- TrayWts_raw$TrayWt[match(Rubble$TrayNum,TrayWts_raw$TrayNum)]
Rubble$DW <- Rubble_Wts_raw$`Dry Wt w Tray`[match(Rubble$TrayNum,Rubble_Wts_raw$`Tray #`)]-Rubble$TrayWt
Rubble$AW <- Rubble_Wts_raw$`Ash Wt w Tray`[match(Rubble$TrayNum,Rubble_Wts_raw$`Tray #`)]-Rubble$TrayWt
Rubble$AFDW <- Rubble$DW - Rubble$AW
Rubble$pcAFDW <- Rubble$AFDW/Rubble$DW
#Rubble SA
Rubble$SA<-Rubble$deltaSAWt*WaxConversionFactor


Rubble_WaxConv <- gs_read(Rubble_Wts_sheet,ws="WaxConversionMsmts")
SAmodR <- lm(Rubble_WaxConv$`SA (mm)` ~ 0+Rubble_WaxConv$`DeltaWax Wt`)
WaxConversionFactor_R <- SAmodR$coefficients[1]/100
Rubble$SA <- with(Rubble_Wts_raw,(deltaWt)*WaxConversionFactor_R)

plot(Rubble$SA ~ Rubble$DeltaBW,type='n')
text(Rubble$DeltaBW, Rubble$SA, label=Rubble$SampleID,cex=0.5)

rubbleIDex1 <- subset(CRANE_IDexpt1,Substrate=="Rubble")
rubbleIDex2 <- subset(CRANE_IDexpt2,Substrate=="Rubble")
Rubble$Nuts <- rubbleIDex1$NutLevel[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Tank <- rubbleIDex1$BlackTank[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Aq_Ex1 <- rubbleIDex1$Aquarium[match(Rubble$SampleID,rubbleIDex1$SampleID)]
Rubble$Aq_Ex2 <- rubbleIDex2$Aquarium[match(Rubble$SampleID,rubbleIDex2$SampleID)]

plot(DW~AW,data=Rubble)
#points(x=Rubble$AW[Rubble$TrayNum==120],y=Rubble$DW[Rubble$TrayNum==120],col="red")

#labels for Rubble data
Rubble.labels <- c(SampleID = "Sample Identifier",
                   InitialTemp = "Temperature during initial buoyant weight",
                   FinalTemp = "Temperature during final buoyant weight",
                   Volume = "Volume (cm3) based on displacement to nearest 5 cm3",
                   TrayNum = "Tray Number associated with this sample",
                   deltaSAWt = "Change in weight before and after wax dipping (g)",
                   InitBW = "Initial Dry Weight (g) based on buoyant wt; calculated from the inital buoyant weight and temperature based on Davies (1989) and a coral density of 2.93 g/cm3",
                   FinalBW = "Final Dry Weight (g) based on buoyant wt; calculated from the final buoyant weight and temperature based on Davies (1989) and a coral density of 2.93 g/cm3",
                   DeltaBW = "DeltaBW_all with three outliers removed",
                   DeltaBW_all = "DeltaBW = FinalBW - InitialBW (g); change in dry weight during experiment based on wet weight",
                   pcDeltaBW = "Percent Change in Dry Weight = DeltaBW/InitBW",
                   TrayWt = "Weight (g) of tray associate with the sample",
                   DW = "Dry Weight (g) of sample at end of experiment after oven drying at 60C for >48 hours",
                   AW = "Ashed Weight (g) of sample at end of experiment after combusting at 550C for 4 hours",
                   AFDW = "Ash Free Dry Weight (g) = DW - AW; organic matter lost during combustion",
                   pcAFDW = "Percent Ash Free Dry Weight; organic matter as a percent of dry weight",
                   SA = "Surface Area of Rubble (cm2) based on wax dipping",
                   Nuts = "Nutrient level assigned to the aquarium: Ambient/Medium/High",
                   Tank = "Tank Identifier; each aquarium was assigned to one of 3 tanks",
                   Aq_Ex1 = "Aquarium Identifier for Experiment 1; nine aquaria held coral nubbins in expt 1",
                   Aq_Ex2 = "Aquarium Identifier for Experiment 2; all aquaria had coral nubbins in expt 2")

write.table(Rubble,file=paste(fileloc,"Rubble_Rprocessed.csv",sep=""),sep=",", col.names = NA)
write.table(Rubble.labels, file=paste(fileloc,"Rubble_metatdata.csv",sep=""),sep=",", col.names = NA)

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
#Bits
Algae$TrayWtBits <- TrayWts_raw$TrayWt[match(Algae$TrayNumBits,TrayWts_raw$TrayNum)]
Algae$DWbits <- Algae_Wts_raw$`DW with Tray`[match(Algae$TrayNumBits,Algae_Wts_raw$Tray)]-Algae$TrayWtBits
Algae$AWbits <- Algae_Wts_raw$`AW with Tray`[match(Algae$TrayNumBits,Algae_Wts_raw$Tray)]-Algae$TrayWtBits
Algae$AFDWbits <- Algae$DWbits - Algae$AWbits
Algae$pcAFDWbits <- Algae$AFDWbits/Algae$DWbits

#Algae WW are tightly related to volume; assume Gsal is a cynlinder, then SA is estimable from cross-sectional radius and volume
AlgVol_lm <- lm(CRANE_AlgalVol$`Volume of Algae`~CRANE_AlgalVol$`Algae Wet Weight (g)`-1)
AlgRadius <- mean(CRANE_AlgalSA$`Algal cross section diameters (mm)`)/2/10 #convert diam to radius and mm to cm
Algae$FinalVol <- Algae$FinalWW*coef(AlgVol_lm)
Algae$BitsVol <- Algae$BitsWW*coef(AlgVol_lm)
Algae$FinalSA <- 2*Algae$FinalVol/AlgRadius
Algae$BitsSA <- 2*Algae$BitsVol/AlgRadius

#Metadata for Algae
Algae.labels <- c(SampleID = "Sample Identifier",
                  InitialWW = "Initial Wet Weight (g) of Gracilaria salicornia; weighed after drying in salad spinner",
                  FinalWW = "Final Wet Weight (g) of Gracilaria salicornia; weighted after drying in salad spinner",
                  BitsWW = "Final Wet Weight (g) of bits of algae that fell out of the individual mesh container but remained in the experimental aquarium; the bits are not included in the mass change calculations because they cannot be associated with individual samples in Expt-1; they are included in the ecosystem metabolism calculations at the aquarium scale",
                  TrayNum1 = "Tray Number associated with the algae sample",
                  TrayNumBits = "Tray Number associated with the algae bits",
                  DeltaWW = "DeltaWW = FinalWW - InitialWW; change in wet weight (g) over the 6 week experiment",
                  pcDeltaWW = "pcDeltaWW = DeltaWW/InitialWW; percent change in wet weight over 6 week experiment",
                  Nuts = "Nutrient level assigned to the aquarium: Ambient/Medium/High",
                  Tank = "Tank Identifier; each aquarium was assigned to one of 3 tanks",
                  Aq_Ex1 = "Aquarium Identifier for Experiment 1; nine aquaria held coral nubbins in expt 1",
                  Aq_Ex2 = "Aquarium Identifier for Experiment 2; all aquaria had coral nubbins in expt 2",
                  TrayWt = "Tray Weight (g) associated with the sample",
                  DW = "Dry Weight (g) of algal sample after drying at 60C for >48 hours",
                  AW = "Ash Weight (g) of algal sample after combusting at 550C for 4 hours",
                  AFDW = "AFDW = DW - AW; Weight (g) of organic matter lost on combustion",
                  pcAFDW = "Percent Ash Free Dry Weight = AFDW/DW; this is the percent of the total dry weight that is organic matter",
                  TrayWtBits = "Tray Weight Associated with algae bits",
                  DWbits = "Dry Weight (g) of algae bits after drying at 60C for >48 hours",
                  AWbits = "Ash Weights (g) of algal bits after combusting at 550C for 4 hours",
                  AFDWbits = "AFDWbits = DWbits - AWbits; Weight (g) of organic matter lost on combustion",
                  pcAFDWbits = "AFDW/DW; percent ash free dry weight for algae bits",
                  FinalVol = "Final Volume (cm3) of algae based on regression of volume vs wet weight of 6 samples, Rsq = 0.998",
                  BitsVol = "Final Volume (cm3) of algal bits based on regression of volume vs wet weight",
                  FinalSA = "Final Surface Area (cm2) of algae based on volume, average diameter of Gracilaria salicornia (2.31 mm +/- .087mm se, n=11), and the assumption that Graciliaria salicornia is made up of cylinders",
                  BitsSA = "Final Surface Area (cm2) of algae bits")   

plot(DW~AW,type="n",data=Algae)
text(x=Algae$AW,y=Algae$DW,labels=Algae$SampleID,cex=0.7)

write.table(Algae,file=paste(fileloc,"Algae_Rprocessed.csv",sep=""),sep=",", col.names = NA)
write.table(Algae.labels,file=paste(fileloc,"Algae_metadata.csv",sep=""),sep=" ,")

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

#Sand Labels
Sand.labels <- c(SampleID = "Sample Identifier",
                 InitialBW = "Buoyant Weight of Sand in petri dish at beginning of experiment",
                 InitialTemp = "Temperature during initial buoyant weight",
                 PercentOutside = "Visual estimate of the percent of sand that spilled out of petri dish",
                 MeasVol = "Final Volume of Sand (cm3) measured by displacement; only for 8 samples",
                 DW = "Final Dry Wt of Sand (g) after drying at 60C for >48 hours",
                 SA = "Surface Area (cm2) of Sand based on area of petri dish; same for all samples",
                 Vol = "Estimated Final volume (cm3) of Sand based on regression between measured volume vs dry weight (n=8,Rsq=0.999)",
                 AW = "Ashed weight (g) of Sand after combustion at 550C for 4 hours",
                 AFDW = "Ash Free Dry Weight (g) = DW - AW; weight (g) of organic matter lost on combustion",
                 pcAFDW = "Percent Ash Free Dry Weight = AFDW/DW; percent of dry weight that is organic matter",
                 Nuts = "Nutrient level assigned to the aquarium: Ambient/Medium/High",
                 Tank = "Tank Identifier; each aquarium was assigned to one of 3 tanks",
                 Aq_Ex1 = "Aquarium Identifier for Experiment 1; nine aquaria held coral nubbins in expt 1",
                 Aq_Ex2 = "Aquarium Identifier for Experiment 2; all aquaria had coral nubbins in expt 2")

write.table(Sand,file=paste(fileloc,"Sand_Rprocessed.csv",sep=""), sep=",", col.names = NA)
write.table(Sand.labels,file=paste(fileloc,"Sand_metadata.csv",sep=""), sep=",", col.names = NA)

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
