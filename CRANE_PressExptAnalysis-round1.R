# Look at changes in BW and WW over the 6 week press

#Coral PERCENT CHANGE in BW
# MC_pc <- aov(pcDeltaDens ~ Nuts + Tank, data=CoralSet,subset=(Species=="Montipora"))
# MC_pc
# summary(MC_pc)
# MC_pc_posthoc <- TukeyHSD(MC_pc,ordered=TRUE)
# plot(TukeyHSD(MC_pc,"Nuts"))
# 
# PC_pc <- aov(pcDeltaDens ~ Nuts + Tank, data=CoralSet,subset=(Species=="Porites"))
# PC_pc
# summary(PC_pc)
# PC_pc_posthoc <- TukeyHSD(PC_pc,"Nuts",ordered=FALSE)
# plot(TukeyHSD(PC_pc,"Nuts"))

# Coral_pc1 <- aov(pcDeltaDens ~ Nuts*Species + Tank, data=CoralSet)
# Coral_pc1
# summary(Coral_pc1)
# TukeyHSD(Coral_pc1,c("Nuts","Species"),ordered=FALSE)
# plot(TukeyHSD(Coral_pc1,"Nuts"))
# plot(TukeyHSD(Coral_pc1,"Species"))

# Block as fixed effect using AOV
Coral_pc2 <- aov(pcDeltaDens ~ Nuts + Species + Tank, data=CoralSet)
Coral_pc2
summary(Coral_pc2)
TukeyHSD(Coral_pc2,c("Nuts","Species"),ordered=FALSE)
plot(TukeyHSD(Coral_pc2))
Coral_output <-with(CoralSet,data.frame(SampleID,Species,Tank,Nuts,pcDeltaDens,pcOrg))
Coral_output$fitted.pc2 <- Coral_pc2$fitted.values

Coralorg <- aov(pcOrg ~ Nuts + Species + Tank, data=CoralSet)
Coralorg
summary(Coralorg)
TukeyHSD(Coralorg,c("Nuts","Species"),ordered=FALSE)
plot(TukeyHSD(Coralorg))
Coral_output$fitted.org <- Coralorg$fitted.values

#block as random effect using NLME
load(nlme)
Coral_BW_lme <- lme(pcDeltaDens ~ Nuts + Species, random=~1|Tank, data=CoralSet)
Coral_BW_lme
summary(Coral_BW_lme)

#for Org, we have data by nubbin, and colony and tank are orthogonal (not nested) random effects
#apparently, these are not available with the syntax in nlme
load(lme4)
Coral_org_lme <- lmer(pcOrg ~ Nuts + Species + (1|Tank) +(Species|Clone), data=CoralNubb)
Coral_org_lme
summary(Coral_org_lme)


##Algae PC in WW
Algae_pc <- aov(pcDeltaWW ~ Nuts + Tank, data=Algae)
Algae_pc
summary(Algae_pc)
Algae_pc_posthoc <- TukeyHSD(Algae_pc,ordered=TRUE)
Algae_pc_posthoc
plot(TukeyHSD(Algae_pc,"Nuts"))

Algae_org <- aov(pcOrg ~ Nuts + Tank, data=Algae)
Algae_org
summary(Algae_org)
Algae_org_posthoc <- TukeyHSD(Algae_org,ordered=TRUE)
Algae_org_posthoc
plot(TukeyHSD(Algae_org,"Nuts"))
Algae_output <- with(Algae,data.frame(SampleID,Tank, Nuts,pcDeltaWW,pcOrg))
Algae_output$fitted.org <-Algae_org$fitted.values
Algae_output$fitted.pcgrowth <-Algae_pc$fitted.values

Algae_pc_lme <- lme(pcDeltaWW ~ Nuts, random = ~1|Tank, data=Algae)
summary(Algae_pc_lme)
Algae_org_lme <- lme(pcOrg ~ Nuts, random = ~1|Tank, data=Algae)
summary(Algae_org_lme)

##Rubble PC in BW
Rubble_pc <- aov(pcDeltaBW ~ Nuts + Tank, data=Rubble)
Rubble_pc
summary(Rubble_pc)
Rubble_pc_posthoc <- TukeyHSD(Rubble_pc,ordered=TRUE)
Rubble_pc_posthoc
plot(TukeyHSD(Rubble_pc,"Nuts"))

Rubble_org <- aov(pcOrg ~ Nuts + Tank, data=Rubble)
Rubble_org
summary(Rubble_org)
Rubble_org_posthoc <- TukeyHSD(Rubble_org,ordered=TRUE)
Rubble_org_posthoc
plot(TukeyHSD(Rubble_org,"Nuts"))

Rubble_org_lme <- lme(pcOrg ~ Nuts, random = ~1|Tank, data=Rubble)
summary(Rubble_org_lme)

Rubble_pc_lme <- lme(pcDeltaBW ~ Nuts, random = ~1|Tank, data=Rubble)
summary(Rubble_pc_lme)

Rubble_output <- with(Rubble,data.frame(SampleID,Tank, Nuts,pcDeltaBW,pcOrg))
Rubble_output$fitted.org <-Rubble_org$fitted.values
Rubble_output$fitted.pcgrowth <-Rubble_pc$fitted.values

write.table(Coral_output,file="../CRANE shared folder/Data/Weights, Volumes & SAs/Coral_output.csv")
write.table(Algae_output,file="../CRANE shared folder/Data/Weights, Volumes & SAs/Algae_output.csv")
write.table(Rubble_output,file="../CRANE shared folder/Data/Weights, Volumes & SAs/Rubble_output.csv")

library(MASS)
library(vcd)
table(CoralNubb$Compromised[CoralNubb$Species=="Montipora"],CoralNubb$Nuts[CoralNubb$Species=="Montipora"],CoralNubb$Clone[CoralNubb$Species=="Montipora"])
xtable_MC <- xtabs(~Compromised + Nuts,data=CoralNubb,subset=(Species=="Montipora"))
xtable_PC <- xtabs(~Compromised + Nuts,data=CoralNubb,subset=(Species=="Porites"))
mosaic(xtable_MC,shade=TRUE,legend=TRUE)
mosaic(xtable_PC,shade=TRUE,legend=TRUE)

loglm(~Compromised + Nuts,xtable_MC)

# Nutrient plots
nutsIN_means <- tapply(Nutrients_CH$Phosphate,INDEX=list(Nutrients_CH$Nominal.Timepoint,Nutrients_CH$Treatment),FUN=mean)
nutsIN_ses <- sqrt(tapply(Nutrients_CH$Phosphate,INDEX=list(Nutrients_CH$Nominal.Timepoint,Nutrients_CH$Treatment),FUN=var)/3)
nutsIN_lb <- nutsIN_means-nutsIN_ses
nutsIN_ub <- nutsIN_means+nutsIN_ses
daysIN <- c(2,9,14,21,28)

plot(nutsIN_means[,3]~ daysIN,type="n",ylab="Nitrate + Nitrite, ug/L",xlab = "Day",ylim=c(0,8))
plot(nutsIN_means[,3]~ daysIN,type="n",ylab="Phosphate, ug/L",xlab = "Day",ylim=c(0,3.2))
#incoming water - N4
lines(daysIN, nutsIN_means[,3])
points(daysIN, nutsIN_means[,3],pch=16,col="black")
arrows(daysIN, nutsIN_lb[,3], daysIN, nutsIN_ub[,3], length=0.05, angle=90, code=3)
#incoming water - N2
lines(daysIN, nutsIN_means[,2],col="darkgray")
points(daysIN, nutsIN_means[,2],pch=16,col="darkgray")
arrows(daysIN, nutsIN_lb[,2], daysIN, nutsIN_ub[,2], length=0.05, angle=90, code=3,col="darkgray")
#incoming water N0
lines(daysIN, nutsIN_means[,1],col="gray")
points(daysIN, nutsIN_means[,1],pch=16,col="gray")
arrows(daysIN, nutsIN_lb[,1], daysIN, nutsIN_ub[,1], length=0.05, angle=90, code=3,col="gray")

#outgoing water
nutsOUT_means <- tapply(Nutrients_CARS$N.N,INDEX=list(Nutrients_CARS$Nominal.Timepoint,Nutrients_CARS$Treatment,Nutrients_CARS$Substrate.Experiment),FUN=mean)
nutsOUT_ses <- sqrt(tapply(Nutrients_CARS$N.N,INDEX=list(Nutrients_CARS$Nominal.Timepoint,Nutrients_CARS$Treatment,Nutrients_CARS$Substrate.Experiment),FUN=var)/3)
nutsOUT_lb <- nutsOUT_means-nutsOUT_ses
nutsOUT_ub <- nutsOUT_means+nutsOUT_ses
daysOUT <- c(2,14,28)


#plot outgoing water for Algae N0
plot(matrix(rep(days2,3),3,3),nutsOUT_means[,,3])
##close here, but too sleepy