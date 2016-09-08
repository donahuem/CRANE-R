#Review SLAB data 05312016

#location for writing processed files
fileloc <- "C:/Users/Sorte/Google Drive/CRANE/CRANE shared folder/Data/Weights, Volumes & SAs/"

library(googlesheets)
suppressMessages(library(dplyr))
library(plyr)
library(gdata)

#authorize access to google sheets
gs_auth(token=NULL, new_user=FALSE,key = getOption("googlesheets.client_id"),secret = getOption("googlesheets.client_secret"),
        cache = getOption("googlesheets.httr_oauth_cache"), verbose = TRUE)

#Nutrient data from 5/31/16 run at SLAB
Bottles_sheet <- gs_key("1IObpNN5Sszo92QhafCKSD9C0Nu4IIs5xFFf7rar8kxE",lookup=TRUE)
Bottles_raw <- gs_read(Bottles_sheet,sheet="20160123_corrected")

CRANE_sheet <- gs_key("1Fks1OzSzpRkNe1X6u9Ez1W6qHmPmTgxR0scueF3OARA",lookup=TRUE)
CRANE_IDexpt1 <- gs_read(CRANE_sheet,ws="Experiment1ID")
CRANE_IDexpt2 <- gs_read(CRANE_sheet,ws="Experiment2ID")

Nuts2_sheet <- gs_key("1pGB3NX6jwz9gI8AyXEwokkqS0wzHr2YSEPvs5xjmuEI",lookup=TRUE)
Nuts2_raw <- gs_read(Nuts2_sheet)

Nuts2<- with(Nuts2_raw,data.frame(SampleID,Phosphate,Silicate,`N+N`,Ammonia))
Nuts2$NutLevel <- as.factor(Bottles_raw$`Nut Level`[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)])
Nuts2$NutLevel <- factor(Nuts2$NutLevel,levels(Nuts2$NutLevel)[c(1,3,2)])
Nuts2$Aquarium <- as.factor(Bottles_raw$Aquarium[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)])
Nuts2$Tank <- as.factor(Bottles_raw$`Black Tank`[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)])
Nuts2$Expt <- Bottles_raw$Experiment[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)]
Nuts2$Date <- Bottles_raw$Date[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)]
Nuts2$SamplingTime <- Bottles_raw$`Sampling Time`[match(Nuts2$SampleID,Bottles_raw$`Nutrient bottle`)]
Nuts2$Substrate <- CRANE_IDexpt1$Substrate[match(Nuts2$Aquarium,CRANE_IDexpt1$Aquarium)]
Nuts2$Substrate[Nuts2$Aquarium=="Gray"|Nuts2$Aquarium=="Pink"|Nuts2$Aquarium=="Red"] <- "Header"
Nuts2$Substrate[is.na(Nuts2$Substrate)==TRUE] <- "Mixed"
Nuts2$Substrate <- as.factor(Nuts2$Substrate)
Nuts2$Substrate <- factor(Nuts2$Substrate,levels(Nuts2$Substrate)[c(3,2,1,5,6,4)])

#Create a variable that orders samples from 1-7 for each experiment
Nuts2$SamplingOrder <- NA
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1000 & Nuts2$Date == 20151203] <-1
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1400 & Nuts2$Date == 20151203] <-2
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1800 & Nuts2$Date == 20151203] <-3
Nuts2$SamplingOrder[Nuts2$SamplingTime == 2200 & Nuts2$Date == 20151203] <-4
Nuts2$SamplingOrder[Nuts2$SamplingTime == 0200 & Nuts2$Date == 20151204] <-5
Nuts2$SamplingOrder[Nuts2$SamplingTime == 0600 & Nuts2$Date == 20151204] <-6
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1000 & Nuts2$Date == 20151204] <-7
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1000 & Nuts2$Date == 20151205] <-8
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1400 & Nuts2$Date == 20151205] <-9
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1800 & Nuts2$Date == 20151205] <-10
Nuts2$SamplingOrder[Nuts2$SamplingTime == 2200 & Nuts2$Date == 20151205] <-11
Nuts2$SamplingOrder[Nuts2$SamplingTime == 0200 & Nuts2$Date == 20151206] <-12
Nuts2$SamplingOrder[Nuts2$SamplingTime == 0600 & Nuts2$Date == 20151206] <-13
Nuts2$SamplingOrder[Nuts2$SamplingTime == 1000 & Nuts2$Date == 20151206] <-14


#Plotting (exclude headers)
plot(Nuts2$Phosphate[Nuts2$Substrate !="Header"]~Nuts2$N.N[Nuts2$Substrate !="Header"],type="n")
text(Nuts2$N.N[Nuts2$Substrate !="Header"],Nuts2$Phosphate[Nuts2$Substrate !="Header"],label=Nuts2$SampleID[Nuts2$Substrate !="Header"],col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"]),cex=0.5)
plot(Nuts2$Phosphate[Nuts2$Substrate !="Header"]~Nuts2$N.N[Nuts2$Substrate !="Header"],type="n")
text(Nuts2$N.N[Nuts2$Substrate !="Header"],Nuts2$Phosphate[Nuts2$Substrate !="Header"],label=Nuts2$Substrate[Nuts2$Substrate !="Header"],col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"]),cex=0.5)

plot(Nuts2$Phosphate~Nuts2$SamplingOrder,col=as.numeric(Nuts2$NutLevel)+1,pch=as.numeric(Nuts2$Substrate))
legend(x=10,y=3,legend=c("Ambient","Medium","High","Coral","Algae","Rubble","Sand","Mixed","Header"),
       pch=c(15,15,15,2,3,4,5,6,1),col=c(2,3,4,1,1,1,1,1,1),cex=0.6)
plot(Nuts2$N.N~Nuts2$SamplingOrder,col=as.numeric(Nuts2$NutLevel)+1,pch=as.numeric(Nuts2$Substrate))
legend(x=10,y=10,legend=c("Ambient","Medium","High","Coral","Algae","Rubble","Sand","Mixed","Header"),
       pch=c(15,15,15,2,3,4,5,6,1),col=c(2,3,4,1,1,1,1,1,1),cex=0.6)
plot(Nuts2$Ammonia~Nuts2$SamplingOrder,col=as.numeric(Nuts2$NutLevel)+1,pch=as.numeric(Nuts2$Substrate))
legend(x=10,y=160,legend=c("Ambient","Medium","High","Coral","Algae","Rubble","Sand","Mixed","Header"),
       pch=c(15,15,15,2,3,4,5,6,1),col=c(2,3,4,1,1,1,1,1,1),cex=0.6)

plot(Nuts2$N.N~Nuts2$SamplingOrder,col=as.numeric(Nuts2$NutLevel)+1,pch=as.numeric(Nuts2$Substrate))
legend(x=10,y=10,legend=c("Ambient","Medium","High","Coral","Algae","Rubble","Sand","Mixed","Header"),
       pch=c(15,15,15,2,1,5,6,3,4),col=c(2,3,4,1,1,1,1,1,1),cex=0.6)
plot(Nuts2$Phosphate~Nuts2$SamplingOrder,col=as.numeric(Nuts2$NutLevel)+1,pch=as.numeric(Nuts2$Substrate))
legend(x=10,y=3.5,legend=c("Ambient","Medium","High","Coral","Algae","Rubble","Sand","Mixed","Header"),
       pch=c(15,15,15,2,1,5,6,3,4),col=c(2,3,4,1,1,1,1,1,1),cex=0.6)

plot(Nuts2$Ammonia[Nuts2$Substrate !="Header"]~Nuts2$N.N[Nuts2$Substrate !="Header"],type="n")
text(Nuts2$N.N[Nuts2$Substrate !="Header"],Nuts2$Ammonia[Nuts2$Substrate !="Header"],label=Nuts2$SampleID[Nuts2$Substrate !="Header"],col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"]),cex=0.5)
plot(Nuts2$Ammonia[Nuts2$Substrate !="Header"]~Nuts2$N.N[Nuts2$Substrate !="Header"],type="n")
text(Nuts2$N.N[Nuts2$Substrate !="Header"],Nuts2$Ammonia[Nuts2$Substrate !="Header"],label=Nuts2$SamplingTime[Nuts2$Substrate !="Header"],col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"]),cex=0.5)
plot(Nuts2$Ammonia[Nuts2$Substrate !="Header"]~Nuts2$N.N[Nuts2$Substrate !="Header"],type="n")
text(Nuts2$N.N[Nuts2$Substrate !="Header"],Nuts2$Ammonia[Nuts2$Substrate !="Header"],label=Nuts2$Substrate[Nuts2$Substrate !="Header"],col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"]),cex=0.5)

plot(Nuts2$Ammonia~Nuts2$NutLevel,subset=(Nuts2$Substrate !="Header"))
hist(Nuts2$Ammonia,100)

#Are outliers in NvP relationship related to Ammonia (i.e., does ammonia relate to unusual values in NvP relationship?  No.)
NvP <- lm(Nuts2$N.N[Nuts2$Substrate !="Header"] ~ Nuts2$Phosphate[Nuts2$Substrate !="Header"])
residNPvAmm <- lm(resid(NvP)~Nuts2$Ammonia[Nuts2$Substrate !="Header"])
plot(resid(NvP)~Nuts2$Ammonia[Nuts2$Substrate !="Header"],
     col=as.numeric(Nuts2$NutLevel[Nuts2$Substrate !="Header"])+1,
     pch=as.numeric(Nuts2$Substrate[Nuts2$Substrate !="Header"]))
legend("topright",legend=c("Amb","Med","High","Coral","Algae","Rubble","Sand","Mixed"),
       col=c(2,3,4,1,1,1,1),pch=c(15,15,15,2,3,4,5,6),cex=0.6,bty="n")

#Compare with press data (code repeated from CRANE-DataProcessing.R, but added Ammonia)
Nutrients_sheet <- gs_key("1j2ZjBC0VzObUr2CzeBvUgtkD2Aw8Fb_II-6N9gpI1B0", lookup=TRUE)
Nutrients_raw <- gs_read(Nutrients_sheet)
Nutrients <- with(Nutrients_raw,data.frame(`Nominal Timepoint`,Treatment,Tank,`N+N`,Phosphate, Ammonia,`Substrate/Experiment`))
#Exclude: CHAIN treatments N1 and N3
Nutrients <- subset(Nutrients,subset=(Nutrients$Treatment == "N0" | Nutrients$Treatment=="N2"| Nutrients$Treatment=="N4"))
#Exclude: timepooint 40 where nutrient application went awry for a day
Nutrients <- subset(Nutrients,subset=(Nutrients$Nominal.Timepoint <40))
#Exclude crazy high outlier 
Nutrients <- subset(Nutrients,subset=(Nutrients$N.N<20))  #exclude point that is at 80
#Exclude Source Water and Fank 
Nutrients <- subset(Nutrients,subset=(Nutrients$Tank!="Source"))
Nutrients <- subset(Nutrients,subset=(Nutrients$Tank!="Fank"))

Nutrients$Substrate.Experiment <- revalue(Nutrients$Substrate.Experiment,c("Sediment"="Sand"))
Nutrients$Substrate.Experiment <- revalue(Nutrients$Substrate.Experiment,c("CHAIN"="Header"))
Nutrients$Substrate.Experiment <- droplevels(Nutrients$Substrate.Experiment)
Nutrients$Substrate.Experiment <- factor(Nutrients$Substrate.Experiment,levels(Nutrients$Substrate.Experiment)[c(2,3,1,4,5)])

Nutrients$Treatment<-factor(Nutrients$Treatment)
Nutrients$Tank <- factor(Nutrients$Tank)
#Nutrients_CH <-subset(Nutrients,subset=(Nutrients$Substrate.Experiment=="CHAIN"))
#Nutrients_CARS <- subset(Nutrients,subset=(Nutrients$Substrate.Experiment=="Algae" | Nutrients$Substrate.Experiment=="Coral" |Nutrients$Substrate.Experiment=="Rubble"|Nutrients$Substrate.Experiment=="Sediment"))
#Nutrients_CH$Substrate.Experiment <- factor(Nutrients_CH$Substrate.Experiment)
#Nutrients_CARS$Substrate.Experiment <- factor(Nutrients_CARS$Substrate.Experiment)

#Headers
plot(Nutrients$N.N[Nutrients$Substrate.Experiment!="Header"] ~ Nutrients$Nominal.Timepoint[Nutrients$Substrate.Experiment!="Header"],col=Nutrients$Treatment[Nutrients$Substrate.Experiment!="Header"])
plot(Nutrients$Phosphate[Nutrients$Substrate.Experiment!="Header"] ~ Nutrients$Nominal.Timepoint[Nutrients$Substrate.Experiment!="Header"],col=Nutrients$Treatment[Nutrients$Substrate.Experiment!="Header"])

#with Substrates
plot(log(Nutrients$N.N) ~ Nutrients$Nominal.Timepoint,col=Nutrients$Treatment,pch=as.numeric(Nutrients$Substrate.Experiment))
plot(Nutrients$Phosphate ~ Nutrients$Nominal.Timepoint,col=Nutrients$Treatment,pch=as.numeric(Nutrients$Substrate.Experiment))

#Want to plot:  mean + 95CI of N.N,PO4,and NH4 for 14,21,28 day timepoints from press and Expt 1 by substrate+header
#Header means + 95CI for press
#Phosphate
nutsPress_means <- tapply(Nutrients$Phosphate[Nutrients$Nominal.Timepoint>13],
                          INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=mean)
nutsPress_ses <- sqrt(tapply(log(Nutrients$Phosphate[Nutrients$Nominal.Timepoint>13]),
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                        Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var)/
                        tapply(log(Nutrients$Phosphate[Nutrients$Nominal.Timepoint>13]),
                               INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                          Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=length))
nutsPress_sds <- sqrt(tapply(Nutrients$Phosphate[Nutrients$Nominal.Timepoint>13],
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var))
nutsPress_lb <- nutsPress_means-1.96*nutsPress_ses
nutsPress_ub <- nutsPress_means+1.96*nutsPress_ses
nutsPress_lbsd <- nutsPress_means-1.96*nutsPress_sds
nutsPress_ubsd <- nutsPress_means+1.96*nutsPress_sds

#Header+Substrate means for Expt1
nutsExpt_means <- tapply(Nuts2$Phosphate,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=mean)
nutsExpt_ses <- sqrt(tapply(Nuts2$Phosphate,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var)/
                       tapply(Nuts2$Phosphate,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=length))
nutsExpt_sds <- sqrt(tapply(Nuts2$Phosphate,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var))
nutsExpt_lb <- nutsExpt_means-1.96*nutsExpt_ses
nutsExpt_ub <- nutsExpt_means+1.96*nutsExpt_ses
nutsExpt_lbsd <- nutsExpt_means-1.96*nutsExpt_sds
nutsExpt_ubsd <- nutsExpt_means+1.96*nutsExpt_sds

plot(c(nutsPress_means),c(nutsExpt_means[,1:5]),col=matrix(rep(c(2:4),5),3,5),pch=t(matrix(rep(c(1,15,20,17,18),3),5,3)),
     xlim=c(floor(min(nutsPress_lb)),ceiling(max(nutsPress_ub))),ylim=c(floor(min(nutsExpt_lb)),ceiling(max(nutsExpt_ub))))
legend("bottomright",legend=c("Amb","Med","High","Header","Coral","Algae","Rubble","Sand"),col=c(2,3,4,1,1,1,1,1),pch=c(1,1,1,1,15,20,17,18),cex=0.8)
abline(0,1)

arrows(x0=c(nutsPress_lb),x1=c(nutsPress_ub),y0=c(nutsExpt_means[,1:5]),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
arrows(y0=c(nutsExpt_lb[,1:5]),y1=c(nutsExpt_ub[,1:5]),x0=c(nutsPress_means),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
title("Phosphate")

#N+N
# nutsPress_means <- tapply(Nutrients$N.N[Nutrients$Nominal.Timepoint>13],
#                           INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=mean)
# nutsPress_ses <- sqrt(tapply(Nutrients$N.N[Nutrients$Nominal.Timepoint>13],
#                              INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
#                                         Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var)/
#                         tapply(Nutrients$N.N[Nutrients$Nominal.Timepoint>13],
#                                INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
#                                           Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=length))
# nutsPress_sds <- sqrt(tapply(Nutrients$N.N[Nutrients$Nominal.Timepoint>13],
#                              INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var))
nutsPress_means <- tapply(log(Nutrients$N.N[Nutrients$Nominal.Timepoint>13]),
                          INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=mean)
nutsPress_ses <- sqrt(tapply(log(Nutrients$N.N[Nutrients$Nominal.Timepoint>13]),
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                        Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var)/
                        tapply(log(Nutrients$N.N[Nutrients$Nominal.Timepoint>13]),
                               INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                          Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=length))
nutsPress_sds <- sqrt(tapply(log(Nutrients$N.N[Nutrients$Nominal.Timepoint>13]),
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var))
nutsPress_lb <- nutsPress_means-1.96*nutsPress_ses
nutsPress_ub <- nutsPress_means+1.96*nutsPress_ses
nutsPress_lbsd <- nutsPress_means-1.96*nutsPress_sds
nutsPress_ubsd <- nutsPress_means+1.96*nutsPress_sds

#Header+Substrate means for Expt1
# nutsExpt_means <- tapply(Nuts2$N.N,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=mean)
# nutsExpt_ses <- sqrt(tapply(Nuts2$N.N,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var)/
#                        tapply(Nuts2$N.N,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=length))
# nutsExpt_sds <- sqrt(tapply(Nuts2$N.N,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var))
nutsExpt_means <- tapply(log(Nuts2$N.N),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=mean)
nutsExpt_ses <- sqrt(tapply(log(Nuts2$N.N),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var)/
                       tapply(log(Nuts2$N.N),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=length))
nutsExpt_sds <- sqrt(tapply(log(Nuts2$N.N),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var))
nutsExpt_lb <- nutsExpt_means-1.96*nutsExpt_ses
nutsExpt_ub <- nutsExpt_means+1.96*nutsExpt_ses
nutsExpt_lbsd <- nutsExpt_means-1.96*nutsExpt_sds
nutsExpt_ubsd <- nutsExpt_means+1.96*nutsExpt_sds

plot(c(nutsPress_means),c(nutsExpt_means[,1:5]),col=matrix(rep(c(2:4),5),3,5),pch=t(matrix(rep(c(1,15,20,17,18),3),5,3)),
     xlim=c(floor(min(nutsPress_lb)),ceiling(max(nutsPress_ub))),ylim=c(floor(min(nutsExpt_lb)),ceiling(max(nutsExpt_ub))))
legend("bottomright",legend=c("Amb","Med","High","Header","Coral","Algae","Rubble","Sand"),col=c(2,3,4,1,1,1,1,1),pch=c(1,1,1,1,15,20,17,18),cex=0.8)
abline(0,1)

arrows(x0=c(nutsPress_lb),x1=c(nutsPress_ub),y0=c(nutsExpt_means[,1:5]),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
arrows(y0=c(nutsExpt_lb[,1:5]),y1=c(nutsExpt_ub[,1:5]),x0=c(nutsPress_means),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
#title("N+N")
title("log(N+N)")

#Ammonia
# nutsPress_means <- tapply(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13],
#                           INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=mean)
# nutsPress_ses <- sqrt(tapply(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13],
#                              INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
#                                         Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var)/
#                         tapply(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13],
#                                INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
#                                           Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=length))
# nutsPress_sds <- sqrt(tapply(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13],
#                              INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var))
nutsPress_means <- tapply(log(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13]),
                          INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=mean)
nutsPress_ses <- sqrt(tapply(log(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13]),
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                        Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var)/
                        tapply(log(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13]),
                               INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],
                                          Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=length))
nutsPress_sds <- sqrt(tapply(log(Nutrients$Ammonia[Nutrients$Nominal.Timepoint>13]),
                             INDEX=list(Nutrients$Treatment[Nutrients$Nominal.Timepoint>13],Nutrients$Substrate.Experiment[Nutrients$Nominal.Timepoint>13]),FUN=var))
nutsPress_lb <- nutsPress_means-1.96*nutsPress_ses
nutsPress_ub <- nutsPress_means+1.96*nutsPress_ses
nutsPress_lbsd <- nutsPress_means-1.96*nutsPress_sds
nutsPress_ubsd <- nutsPress_means+1.96*nutsPress_sds

#Header+Substrate means for Expt1
# nutsExpt_means <- tapply(Nuts2$Ammonia,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=mean)
# nutsExpt_ses <- sqrt(tapply(Nuts2$Ammonia,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var)/
#                        tapply(Nuts2$Ammonia,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=length))
# nutsExpt_sds <- sqrt(tapply(Nuts2$Ammonia,INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var))
nutsExpt_means <- tapply(log(Nuts2$Ammonia),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=mean)
nutsExpt_ses <- sqrt(tapply(log(Nuts2$Ammonia),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var)/
                       tapply(log(Nuts2$Ammonia),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=length))
nutsExpt_sds <- sqrt(tapply(log(Nuts2$Ammonia),INDEX=list(Nuts2$NutLevel,Nuts2$Substrate),FUN=var))
nutsExpt_lb <- nutsExpt_means-1.96*nutsExpt_ses
nutsExpt_ub <- nutsExpt_means+1.96*nutsExpt_ses
nutsExpt_lbsd <- nutsExpt_means-1.96*nutsExpt_sds
nutsExpt_ubsd <- nutsExpt_means+1.96*nutsExpt_sds

plot(c(nutsPress_means),c(nutsExpt_means[,1:5]),col=matrix(rep(c(2:4),5),3,5),pch=t(matrix(rep(c(1,15,20,17,18),3),5,3)),
     xlim=c(floor(min(nutsPress_lb)),ceiling(max(nutsPress_ub))),ylim=c(floor(min(nutsExpt_lb)),ceiling(max(nutsExpt_ub))))
legend("bottomright",legend=c("Amb","Med","High","Header","Coral","Algae","Rubble","Sand"),col=c(2,3,4,1,1,1,1,1),pch=c(1,1,1,1,15,20,17,18),cex=0.8)
abline(0,1)

arrows(x0=c(nutsPress_lb),x1=c(nutsPress_ub),y0=c(nutsExpt_means[,1:5]),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
arrows(y0=c(nutsExpt_lb[,1:5]),y1=c(nutsExpt_ub[,1:5]),x0=c(nutsPress_means),length=0,col=c(matrix(rep(c(2:4),5),3,5)))
# title("Ammonia")
title("log(Ammonia)")
