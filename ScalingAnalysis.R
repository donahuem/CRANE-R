### CRANE Scaling Analysis
### Started Sept 5, 2017
### Last update Aug 1, 2019
### Testing whether the NEC and NEP rates are additive between single substrate and mixed substrates

source("Scaling/CRANEMetabolismAnalysis_ProcB.R")
library("sjPlot")

AllData$period <- rep(c(1:7),36*2)  #number the sampling times from 1-7 in each expt
AllData$DayNight <- as.factor(AllData$DayNight)
AllData_Ex1 <- AllData[AllData$Experiment==1,]
AllData_Ex2 <- AllData[AllData$Experiment==2,]
AllData_Ex1$Aq_Ex1 <- AllData_Ex1$Aquarium
AllData_Ex2$Aq_Ex2 <- AllData_Ex2$Aquarium

Coral.Exp1Summary <- rename(Coral.Exp1Summary,replace=c("AFDW"="AFDW.tot","DW"="DW.tot","Volume"="Vol.tot","SA"="SA.tot"))
Coral.Exp2Summary <- join(Coral.Exp2Summary,Coral.Exp1Summary, by=c("Aq_Ex1"),type="left")
Algae.Exp1Summary <- rename(Algae.Exp1Summary,replace=c("AFDW"="AFDW.tot","DW"="DW.tot","Volume"="Vol.tot","SA"="SA.tot"))
Algae.Exp2Summary <- join(Algae.Exp2Summary,Algae.Exp1Summary, by=c("Aq_Ex1"),type="left")
Rubble.Exp1Summary <- rename(Rubble.Exp1Summary,replace=c("AFDW"="AFDW.tot","DW"="DW.tot","Volume"="Vol.tot","SA"="SA.tot"))
Rubble.Exp2Summary <- join(Rubble.Exp2Summary,Rubble.Exp1Summary, by=c("Aq_Ex1"),type="left")
Sand.Exp1Summary <- rename(Sand.Exp1Summary,replace=c("AFDW"="AFDW.tot","DW"="DW.tot","Volume"="Vol.tot","SA"="SA.tot"))
Sand.Exp2Summary <- join(Sand.Exp2Summary,Sand.Exp1Summary, by=c("Aq_Ex1"),type="left")

#Add the proportions of Coral/Algae/Rubble/Sand AFDW,DW,SA,Vol from _Exp2Summary to Ex2

AllData_Ex2 <- join(AllData_Ex2,Coral.Exp2Summary,by=c("Aq_Ex2"),type="left")
AllData_Ex2$ResTime.mean.coral <- AllData_Ex1$ResTime.mean[match(AllData_Ex2$Aq_Ex1,AllData_Ex1$Aq_Ex1)]
AllData_Ex2 <- plyr::rename(AllData_Ex2,replace=c("Aq_Ex1"="Aqua.coral",
                                                  "AFDW"="AFDW.coral","DW"="DW.coral",
                                                  "SA"="SA.coral","Volume"="Vol.coral",
                                                  "AFDW.tot"="AFDW.coral.tot","DW.tot"="DW.coral.tot",
                                                  "SA.tot"="SA.coral.tot","Vol.tot"="Vol.coral.tot"))
AllData_Ex2 <- join(AllData_Ex2,Algae.Exp2Summary,by=c("Aq_Ex2"),type="left")
AllData_Ex2$ResTime.mean.algae <- AllData_Ex1$ResTime.mean[match(AllData_Ex2$Aq_Ex1,AllData_Ex1$Aq_Ex1)]
AllData_Ex2 <- plyr::rename(AllData_Ex2,replace=c("Aq_Ex1"="Aqua.algae",
                                                  "AFDW"="AFDW.algae","DW"="DW.algae",
                                                  "SA"="SA.algae","Volume"="Vol.algae",
                                                  "AFDW.tot"="AFDW.algae.tot","DW.tot"="DW.algae.tot",
                                                  "SA.tot"="SA.algae.tot","Vol.tot"="Vol.algae.tot"))
AllData_Ex2 <- join(AllData_Ex2,Rubble.Exp2Summary,by=c("Aq_Ex2"),type="left")
AllData_Ex2$ResTime.mean.rubble <- AllData_Ex1$ResTime.mean[match(AllData_Ex2$Aq_Ex1,AllData_Ex1$Aq_Ex1)]
AllData_Ex2 <- plyr::rename(AllData_Ex2,replace=c("Aq_Ex1"="Aqua.rubble",
                                                  "AFDW"="AFDW.rubble","DW"="DW.rubble",
                                                  "SA"="SA.rubble","Volume"="Vol.rubble",
                                                  "AFDW.tot"="AFDW.rubble.tot","DW.tot"="DW.rubble.tot",
                                                  "SA.tot"="SA.rubble.tot","Vol.tot"="Vol.rubble.tot"))
AllData_Ex2 <- join(AllData_Ex2,Sand.Exp2Summary,by=c("Aq_Ex2"),type="left")
AllData_Ex2$ResTime.mean.sand <- AllData_Ex1$ResTime.mean[match(AllData_Ex2$Aq_Ex1,AllData_Ex1$Aq_Ex1)]
AllData_Ex2 <- plyr::rename(AllData_Ex2,replace=c("Aq_Ex1"="Aqua.sand",
                                                  "AFDW"="AFDW.sand","DW"="DW.sand",
                                                  "SA"="SA.sand","Volume"="Vol.sand",
                                                  "AFDW.tot"="AFDW.sand.tot","DW.tot"="DW.sand.tot",
                                                  "SA.tot"="SA.sand.tot","Vol.tot"="Vol.sand.tot"))

#predicted values weighted by biology contribution (AFDW/DW/SA/Vol)
source("Scaling/ex2exp.R")  #function for calculation predicted values
AllData_Ex2$NCP.AFDW.pred <- ex2exp("NCP","AFDW")
AllData_Ex2$NCP.DW.pred <- ex2exp("NCP","DW")
AllData_Ex2$NCP.SA.pred <- ex2exp("NCP","SA")
AllData_Ex2$NCP.Vol.pred <- ex2exp("NCP","Vol")
AllData_Ex2$NEC.AFDW.pred <- ex2exp("NEC","AFDW")
AllData_Ex2$NEC.DW.pred <- ex2exp("NEC","DW")
AllData_Ex2$NEC.SA.pred <- ex2exp("NEC","SA")
AllData_Ex2$NEC.Vol.pred <- ex2exp("NEC","Vol")

#predicted values normalized by biology contribution & by residence time
AllData_Ex2$NCP.AFDW.predf <- ex2exp("NCP","AFDW",normflow=TRUE)
AllData_Ex2$NCP.DW.predf <- ex2exp("NCP","DW",normflow=TRUE)
AllData_Ex2$NCP.SA.predf <- ex2exp("NCP","SA",normflow=TRUE)
AllData_Ex2$NCP.Vol.predf <- ex2exp("NCP","Vol",normflow=TRUE)
AllData_Ex2$NEC.AFDW.predf <- ex2exp("NEC","AFDW",normflow=TRUE)
AllData_Ex2$NEC.DW.predf <- ex2exp("NEC","DW",normflow=TRUE)
AllData_Ex2$NEC.SA.predf <- ex2exp("NEC","SA",normflow=TRUE)
AllData_Ex2$NEC.Vol.predf <- ex2exp("NEC","Vol",normflow=TRUE)

#Generate observed - expected for each time & normalization
AllData_Ex2$NCP.AFDW.diff <- AllData_Ex2$NCP.AFDW - AllData_Ex2$NCP.AFDW.pred
AllData_Ex2$NCP.DW.diff <- AllData_Ex2$NCP.DW - AllData_Ex2$NCP.DW.pred
AllData_Ex2$NCP.SA.diff <- AllData_Ex2$NCP.SA - AllData_Ex2$NCP.SA.pred
AllData_Ex2$NCP.Vol.diff <- AllData_Ex2$NCP.Vol - AllData_Ex2$NCP.Vol.pred
AllData_Ex2$NEC.AFDW.diff <- AllData_Ex2$NEC.AFDW - AllData_Ex2$NEC.AFDW.pred
AllData_Ex2$NEC.DW.diff <-  AllData_Ex2$NEC.DW - AllData_Ex2$NEC.DW.pred
AllData_Ex2$NEC.SA.diff <- AllData_Ex2$NEC.SA - AllData_Ex2$NEC.SA.pred
AllData_Ex2$NEC.Vol.diff <- AllData_Ex2$NEC.Vol - AllData_Ex2$NEC.Vol.pred
AllData_Ex2$NCP.AFDW.fdiff <- AllData_Ex2$NCP.AFDW - AllData_Ex2$NCP.AFDW.predf
AllData_Ex2$NCP.DW.fdiff <- AllData_Ex2$NCP.DW - AllData_Ex2$NCP.DW.predf
AllData_Ex2$NCP.SA.fdiff <- AllData_Ex2$NCP.SA - AllData_Ex2$NCP.SA.predf
AllData_Ex2$NCP.Vol.fdiff <- AllData_Ex2$NCP.Vol - AllData_Ex2$NCP.Vol.predf
AllData_Ex2$NEC.AFDW.fdiff <- AllData_Ex2$NEC.AFDW - AllData_Ex2$NEC.AFDW.predf
AllData_Ex2$NEC.DW.fdiff <-  AllData_Ex2$NEC.DW - AllData_Ex2$NEC.DW.predf
AllData_Ex2$NEC.SA.fdiff <- AllData_Ex2$NEC.SA - AllData_Ex2$NEC.SA.predf
AllData_Ex2$NEC.Vol.fdiff <- AllData_Ex2$NEC.Vol - AllData_Ex2$NEC.Vol.predf

#Generate day/night averages

NEC_DN <- ddply(AllData_Ex2,c("DayNight","Aquarium", "Tank","NutLevel"),summarise,
                NEC.AFDW.obs.mean = mean(NEC.AFDW, na.rm=T),
                NEC.DW.obs.mean = mean(NEC.DW, na.rm=T),
                NEC.SA.obs.mean = mean(NEC.SA, na.rm=T),
                NEC.Vol.obs.mean = mean(NEC.Vol, na.rm=T),
                NEC.AFDW.pred.mean = mean(NEC.AFDW.pred,na.rm=T),
                NEC.DW.pred.mean = mean(NEC.DW.pred,na.rm=T),
                NEC.SA.pred.mean = mean(NEC.SA.pred,na.rm=T),
                NEC.Vol.pred.mean = mean(NEC.Vol.pred,na.rm=T),
                NEC.AFDW.predf.mean = mean(NEC.AFDW.predf,na.rm=T),
                NEC.DW.predf.mean = mean(NEC.DW.predf,na.rm=T),
                NEC.SA.predf.mean = mean(NEC.SA.predf,na.rm=T),
                NEC.Vol.predf.mean = mean(NEC.Vol.predf,na.rm=T),
                NEC.AFDW.diff.mean = mean(NEC.AFDW.diff,na.rm=T),
                NEC.DW.diff.mean = mean(NEC.DW.diff,na.rm=T),
                NEC.SA.diff.mean = mean(NEC.SA.diff,na.rm=T),
                NEC.Vol.diff.mean = mean(NEC.Vol.diff,na.rm=T),
                NEC.AFDW.fdiff.mean = mean(NEC.AFDW.fdiff,na.rm=T),
                NEC.DW.fdiff.mean = mean(NEC.DW.fdiff,na.rm=T),
                NEC.SA.fdiff.mean = mean(NEC.SA.fdiff,na.rm=T),
                NEC.Vol.fdiff.mean = mean(NEC.Vol.fdiff,na.rm=T),
                NEC.AFDW.mean.diff = NEC.AFDW.obs.mean-NEC.AFDW.pred.mean,
                NEC.DW.mean.diff = NEC.DW.obs.mean-NEC.DW.pred.mean,
                NEC.SA.mean.diff = NEC.SA.obs.mean-NEC.SA.pred.mean,
                NEC.Vol.mean.diff = NEC.Vol.obs.mean-NEC.Vol.pred.mean,
                NEC.AFDW.mean.fdiff = NEC.AFDW.obs.mean-NEC.AFDW.predf.mean,
                NEC.DW.mean.fdiff = NEC.DW.obs.mean-NEC.DW.predf.mean,
                NEC.SA.mean.fdiff = NEC.SA.obs.mean-NEC.SA.predf.mean,
                NEC.Vol.mean.fdiff = NEC.Vol.obs.mean-NEC.Vol.predf.mean)

NCP_DN <- ddply(AllData_Ex2,c("DayNight","Aquarium", "Tank","NutLevel"),
                summarise,
                NCP.AFDW.obs.mean = mean(NCP.AFDW, na.rm=T),
                NCP.DW.obs.mean = mean(NCP.DW, na.rm=T),
                NCP.SA.obs.mean = mean(NCP.SA, na.rm=T),
                NCP.Vol.obs.mean = mean(NCP.Vol, na.rm=T),
                NCP.AFDW.pred.mean = mean(NCP.AFDW.pred,na.rm=T),
                NCP.DW.pred.mean = mean(NCP.DW.pred,na.rm=T),
                NCP.SA.pred.mean = mean(NCP.SA.pred,na.rm=T),
                NCP.Vol.pred.mean = mean(NCP.Vol.pred,na.rm=T),
                NCP.AFDW.predf.mean = mean(NCP.AFDW.predf,na.rm=T),
                NCP.DW.predf.mean = mean(NCP.DW.predf,na.rm=T),
                NCP.SA.predf.mean = mean(NCP.SA.predf,na.rm=T),
                NCP.Vol.predf.mean = mean(NCP.Vol.predf,na.rm=T),
                NCP.AFDW.diff.mean = mean(NCP.AFDW.diff,na.rm=T),
                NCP.DW.diff.mean = mean(NCP.DW.diff,na.rm=T),
                NCP.SA.diff.mean = mean(NCP.SA.diff,na.rm=T),
                NCP.Vol.diff.mean = mean(NCP.Vol.diff,na.rm=T),
                NCP.AFDW.fdiff.mean = mean(NCP.AFDW.fdiff,na.rm=T),
                NCP.DW.fdiff.mean = mean(NCP.DW.fdiff,na.rm=T),
                NCP.SA.fdiff.mean = mean(NCP.SA.fdiff,na.rm=T),
                NCP.Vol.fdiff.mean = mean(NCP.Vol.fdiff,na.rm=T),
                NCP.AFDW.mean.diff = NCP.AFDW.obs.mean-NCP.AFDW.pred.mean,
                NCP.DW.mean.diff = NCP.DW.obs.mean-NCP.DW.pred.mean,
                NCP.SA.mean.diff = NCP.SA.obs.mean-NCP.SA.pred.mean,
                NCP.Vol.mean.diff = NCP.Vol.obs.mean-NCP.Vol.pred.mean,
                NCP.AFDW.mean.fdiff = NCP.AFDW.obs.mean-NCP.AFDW.predf.mean,
                NCP.DW.mean.fdiff = NCP.DW.obs.mean-NCP.DW.predf.mean,
                NCP.SA.mean.fdiff = NCP.SA.obs.mean-NCP.SA.predf.mean,
                NCP.Vol.mean.fdiff = NCP.Vol.obs.mean-NCP.Vol.predf.mean)

#Order the effects
NEC_DN$NutLevel <- factor(NEC_DN$NutLevel, levels = c("Ambient","Med","High" ))

#Run basic mixed model for NEC normalized to AFDW
pdf('Scaling/plots/NEC.AFDW.diff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NEC.AFDW.diff v nuts x daynight.txt')
mod1.NEC.AFDW.diff.mean <- lmer(NEC.AFDW.diff.mean ~ DayNight*NutLevel
                           + (1|Tank),data=NEC_DN)
print(anova(mod1.NEC.AFDW.diff.mean))
print(summary(mod1.NEC.AFDW.diff.mean))
plot_model(mod1.NEC.AFDW.diff.mean, type = "int")
plot_model(mod1.NEC.AFDW.diff.mean, type = "re")
#sjp.lmer(mod1.NEC.AFDW.diff.mean,type="fe")
#sjp.lmer(mod1.NEC.AFDW.diff.mean,type="re")
sink()
dev.off()


#Run basic mixed model for NCP normalized to AFDW
pdf('Scaling/plots/NCP.AFDW.diff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NCP.AFDW.diff v nuts x daynight.txt')
mod1.NCP.AFDW.diff.mean <- lmer(NCP.AFDW.diff.mean ~ DayNight*NutLevel
                                + (1|Tank),data=NCP_DN)
print(anova(mod1.NCP.AFDW.diff.mean))
print(summary(mod1.NCP.AFDW.diff.mean))
plot_model(mod1.NCP.AFDW.diff.mean, type = "int")
plot_model(mod1.NCP.AFDW.diff.mean, type = "re")

#sjp.lmer(mod1.NCP.AFDW.diff.mean,type="fe")
#sjp.lmer(mod1.NCP.AFDW.diff.mean,type="re")
sink()
dev.off()

#Run basic mixed model for NEC normalized to flow &  AFDW
pdf('Scaling/plots/NEC.AFDW.fdiff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NEC.AFDW.fdiff v nuts x daynight.txt')
mod1.NEC.AFDW.fdiff.mean <- lmer(NEC.AFDW.fdiff.mean ~ DayNight*NutLevel
                                + (1|Tank),data=NEC_DN)
print(anova(mod1.NEC.AFDW.fdiff.mean))
print(summary(mod1.NEC.AFDW.fdiff.mean))
#sjp.lmer(mod1.NEC.AFDW.fdiff.mean,type="fe")
#sjp.lmer(mod1.NEC.AFDW.fdiff.mean,type="re")
plot_model(mod1.NEC.AFDW.fdiff.mean, type = "int")
plot_model(mod1.NEC.AFDW.fdiff.mean, type = "re")

sink()
dev.off()


#Run basic mixed model for NEC normalized to flow & AFDW
pdf('Scaling/plots/NCP.AFDW.fdiff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NCP.AFDW.fdiff v nuts x daynight.txt')
mod1.NCP.AFDW.fdiff.mean <- lmer(NCP.AFDW.fdiff.mean ~ DayNight*NutLevel
                                + (1|Tank),data=NCP_DN)
print(anova(mod1.NCP.AFDW.fdiff.mean))
print(summary(mod1.NCP.AFDW.fdiff.mean))

plot_model(mod1.NCP.AFDW.fdiff.mean, type = "int")
plot_model(mod1.NCP.AFDW.fdiff.mean, type = "re")
#sjp.lmer(mod1.NCP.AFDW.fdiff.mean,type="fe")
#sjp.lmer(mod1.NCP.AFDW.fdiff.mean,type="re")
sink()
dev.off()

#***********************

#Run basic mixed model for NEC normalized to AFDW (difference before mean)
pdf('Scaling/plots/NEC.AFDW.mean.diff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NEC.AFDW.mean.diff v nuts x daynight.txt')
mod1.NEC.AFDW.mean.diff <- lmer(NEC.AFDW.mean.diff ~ DayNight*NutLevel
                                 + (1|Tank),data=NEC_DN)
print(anova(mod1.NEC.AFDW.mean.diff))
print(summary(mod1.NEC.AFDW.mean.diff))
plot_model(mod1.NEC.AFDW.mean.diff , type = "int")
plot_model(mod1.NEC.AFDW.mean.diff , type = "re")
#sjp.lmer(mod1.NEC.AFDW.mean.diff,type="fe")
#sjp.lmer(mod1.NEC.AFDW.mean.diff,type="re")
sink()
dev.off()


#Run basic mixed model for NCP normalized to AFDW (difference before mean)
pdf('Scaling/plots/NCP.AFDW.mean.diff v nuts x daynight.pdf', width = 5, height = 5, paper="letter")
sink('Scaling/stats/NCP.AFDW.mean.diff v nuts x daynight.txt')
mod1.NCP.AFDW.mean.diff <- lmer(NCP.AFDW.mean.diff ~ DayNight*NutLevel
                                + (1|Tank),data=NCP_DN)
print(anova(mod1.NCP.AFDW.mean.diff))
print(summary(mod1.NCP.AFDW.mean.diff))
plot_model(mod1.NCP.AFDW.mean.diff , type = "int")
plot_model(mod1.NCP.AFDW.mean.diff , type = "re")
#sjp.lmer(mod1.NCP.AFDW.mean.diff,type="fe")
#sjp.lmer(mod1.NCP.AFDW.mean.diff,type="re")
sink()
dev.off()

 
#********************


# 
# 
# DataEx1 <- AllData[AllData$Experiment==1,]
# colnames(DataEx1)[1] <- "Aq_Ex1"
# nr <- nrow(ScalDat)
# ScalDat <- ScalDat[rep(1:nrow(ScalDat),times=length(unique(DataEx1$DateTime))),]
# ScalDat$DateTime <- rep(unique(DataEx1$DateTime),each=nr)
# ScalDat <- plyr::join(ScalDat,DataEx1,by=c("Aq_Ex1","DateTime","Substrate"),type="left")
# 
# ScalDat <- mutate(ScalDat,NEC.AFDW.wtd = NEC.AFDW*propAFDW,NCP.AFDW.wtd = NCP.AFDW*propAFDW)
# ScalDat <- mutate(ScalDat,NEC.DW.wtd = NEC.DW*propDW,NCP.DW.wtd = NCP.DW*propDW)
# ScalDat <- mutate(ScalDat,NEC.Vol.wtd = NEC.Vol*propVol,NCP.Vol.wtd = NCP.Vol*propVol)
# ScalDat <- mutate(ScalDat,NEC.SA.wtd = NEC.SA*propSA,NCP.SA.wtd = NCP.SA*propSA)
# 
# #Now, add calculations for predicted NCP and NEC to Expt2 data to compare
# DataEx2 <- AllData[AllData$Experiment==2,]
# colnames(DataEx2)[1] <- "Aq_Ex2"
# colnames(DataEx2)[49] <- "DateTimeEx2"
# 
# ScalDat_Aqua <- ddply(ScalDat,c("Aq_Ex2","DateTime","NutLevel"),summarise,
#                                  NEC.AFDW.pred = sum(NEC.AFDW.wtd),
#                                  NEC.DW.pred = sum(NEC.DW.wtd),
#                                  NEC.Vol.pred = sum(NEC.Vol.wtd),
#                                  NEC.SA.pred = sum(NEC.SA.wtd),
#                                  NCP.AFDW.pred = sum(NCP.AFDW.wtd),
#                                  NCP.DW.pred = sum(NCP.DW.wtd),
#                                  NCP.Vol.pred = sum(NCP.Vol.wtd),
#                                  NCP.SA.pred = sum(NCP.SA.wtd))
# ScalDat_Aqua<- mutate(ScalDat_Aqua, DateTimeEx2 = DateTime+60*60*48)
# 
# DataEx2 <- join(DataEx2,ScalDat_Aqua,by=c("Aq_Ex2","DateTimeEx2","NutLevel"),"left")
# levels(DataEx2$DateTimeEx2)<- c(1:7)
# DataEx2 <- mutate(DataEx2,NEC.AFDW.diff = NEC.AFDW - NEC.AFDW.pred,
#                           NEC.DW.diff = NEC.DW - NEC.DW.pred,
#                           NEC.Vol.diff = NEC.Vol - NEC.Vol.pred,
#                           NEC.SA.diff = NEC.SA - NEC.SA.pred,
#                           NCP.AFDW.diff = NCP.AFDW - NCP.AFDW.pred,
#                           NCP.DW.diff = NCP.DW - NCP.DW.pred,
#                           NCP.Vol.diff = NCP.Vol - NCP.Vol.pred,
#                           NCP.SA.diff = NCP.SA - NCP.SA.pred)
# 
# DataEx2$DateTimeEx2<- as.factor(DataEx2$DateTimeEx2)
# DataEx2$DayNight<- as.factor(DataEx2$DayNight)
# 
# rvs <- c("NEC","NCP") #,"deltapH")
# 
# #Run models for NEC and NCP differences normalized for AFDW:
# #  rv ~ Nutrients * DayNight +(1|Tank) + (DayNight|DateTimeEx2)
# 
# #Run basic mixed model for NEC normalized to AFDW
# pdf('Scaling/plots/NEC.AFDW.diff v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
# sink('Scaling/stats/NEC.AFDW.diff v nuts x daynight.txt')
# mod1.NEC.AFDW.diff <- lmer(NEC.AFDW.diff ~ DayNight*NutLevel
#                            + (1|Tank) 
#                            + (DayNight|DateTimeEx2),data=DataEx2)
# print(anova(mod1.NEC.AFDW.diff))
# print(summary(mod1.NEC.AFDW.diff))
# sjp.lmer(mod1.NEC.AFDW.diff,type="fe")
# sjp.lmer(mod1.NEC.AFDW.diff,type="re")  
# dev.off()
# sink()
# 
# #Run basic mixed model for NCP normalized to AFDW
# pdf('Scaling/plots/NCP.AFDW.diff v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
# sink('Scaling/stats/NCP.AFDW.diff v nuts x daynight.txt')
# mod1.NCP.AFDW.diff <- lmer(NCP.AFDW.diff ~ DayNight*NutLevel
# #                           + (1|Tank) 
#                            + (DayNight|DateTimeEx2),data=DataEx2)
# print(anova(mod1.NCP.AFDW.diff))
# print(summary(mod1.NCP.AFDW.diff))
# sjp.lmer(mod1.NCP.AFDW.diff,type="fe")
# sjp.lmer(mod1.NCP.AFDW.diff,type="re")  
# dev.off()
# sink()
# 
# #SCALED: Run basic mixed model for NEC normalized to AFDW
# DataEx2$NEC.AFDW.diff.rs <- rescale(DataEx2$NEC.AFDW.diff)
# pdf('Scaling/plots/NEC.AFDW.diff.rs v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
# sink('Scaling/stats/NEC.AFDW.diff.rs v nuts x daynight.txt')
# mod1.NEC.AFDW.diff.rs <- lmer(NEC.AFDW.diff ~ DayNight*NutLevel
#                            + (1|Tank)
#                            + (DayNight|DateTimeEx2),data=DataEx2)
# print(anova(mod1.NEC.AFDW.diff.rs))
# print(summary(mod1.NEC.AFDW.diff.rs))
# sjp.lmer(mod1.NEC.AFDW.diff.rs,type="fe")
# sjp.lmer(mod1.NEC.AFDW.diff.rs,type="re")
# dev.off()
# sink()
# 
# #SCALED: Run basic mixed model for NCP normalized to AFDW
# DataEx2$NCP.AFDW.diff.rs <- rescale(DataEx2$NCP.AFDW.diff)
# pdf('Scaling/plots/NCP.AFDW.diff.rs v nuts x daynight.pdf', width = 8, height = 10.5, paper="letter")
# sink('Scaling/stats/NCP.AFDW.diff.rs v nuts x daynight.txt')
# mod2.NCP.AFDW.diff.rs <- lmer(NCP.AFDW.diff.rs ~ DayNight*NutLevel
#                            + (1|Tank)
#                            + (DayNight|DateTimeEx2),data=DataEx2)
# print(anova(mod2.NCP.AFDW.diff.rs))
# print(summary(mod2.NCP.AFDW.diff.rs))
# sjp.lmer(mod2.NCP.AFDW.diff.rs,type="fe")
# sjp.lmer(mod2.NCP.AFDW.diff.rs,type="re")
# dev.off()
# sink()
# 
# RV.diff.means<-ddply(DataEx2, c("DayNight","NutLevel"), summarise,
#                    Mean.NCP.AFDW.diff=mean(NCP.AFDW.diff, na.rm=T),
#                    Mean.NEC.AFDW.diff=mean(NEC.AFDW.diff, na.rm=T),
#                    reps=12,
#                    SE.NCP.AFDW.diff=sd(NCP.AFDW.diff)/sqrt(reps),
#                    SE.NEC.AFDW.diff=sd(NEC.AFDW.diff)/sqrt(reps)
# )
# 
# 
# #NEC plots net
#   x<-barplot(matrix(RV.diff.means$Mean.NCP.AFDW.diff,nrow=3,ncol=2),beside=TRUE,
#              ylab=("Exp(NCP) - Obs(NCP), AFDW normalized")
#   errorbars(x,NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]],0,NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==sub[i]])
#   axis(1, at=x, labels=c("Ambient","Medium","High"))
#   lines(x,c(0,0,0))
# }
# 
# 
# ##Basic Plotting 
# for (i in (1:length(rvs))){
#   par(mfrow=c(2,2))
#   for (j in (1:length(sandvarlist))){
#     nmx <- paste0(rvs[i],".",sandvarlist[j])
#     nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
#     plot(DataEx2[[nmx]],DataEx2[[nmy]],col=as.numeric(unique(DataEx2$NutLevel)),
#          xlab=nmx,ylab=nmy)
#     legend("topleft",legend=levels(DataEx2$NutLevel),col=as.numeric(unique(DataEx2$NutLevel)),pch=1,bty="n")
#     abline(0,1)
#   }
# }
# 
# for (i in (1:length(rvs))){
#   par(mfrow=c(2,2))
#   for (j in (1:length(sandvarlist))){
#     nmx <- paste0(rvs[i],".",sandvarlist[j])
#     nmy <- paste0(rvs[i],".",sandvarlist[j],".pred")
#     plot(DataEx2[[nmx]],DataEx2[[nmy]],col=as.numeric(unique(DataEx2$DayNight)),
#          xlab=nmx,ylab=nmy)
#     legend("topleft",legend=unique(DataEx2$Time),col=as.numeric(unique(DataEx2$DayNight)),pch=1,bty="n")
#     abline(0,1)
#     
#   }
# }
# 
# #Plotting Deviation by nutrients and by time
# par(mfrow=c(2,2))
# for (i in (1:length(rvs))){
#   for (j in (1:length(sandvarlist))){
#     nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
#     print(nmy)
#     plot(DataEx2[[nmy]]~DataEx2$NutLevel, ylab=nmy)
#     abline(0,0,col="blue")
#   }
# }
# 
# #Plotting Deviation by nutrients and by time
# par(mfrow=c(2,2))
# for (i in (1:length(rvs))){
#   for (j in (1:length(sandvarlist))){
#     nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
#     print(nmy)
#     plot(DataEx2[[nmy]]~as.factor(DataEx2$DayNight), ylab=nmy)
#     abline(0,0,col="blue")
#   }
# }
# 
# 
# 
# #Histograms
# for (i in (1:length(rvs))){
#   for (j in (1:length(sandvarlist))){
#     nmy <- paste0(rvs[i],".",sandvarlist[j],".diff")
#     hist(DataEx2[[nmy]], xlab=paste0(nmy),main="")
#   }
# }
# 
