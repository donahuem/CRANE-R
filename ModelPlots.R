#Algae
model.NECNet.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' ,]) 
model.NECDay.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Day',]) 
model.NECNight.algae<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Night',]) 
model.NCPNet.algae<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' ,]) 
model.R.algae<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Night',]) 
model.GCP.algae<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Algae' & AllData$DayNight=='Day',]) 

#coral
model.NECNet.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank) +(1|DateTime), data=AllData[AllData$Substrate=='Coral' ,]) 
model.NECDay.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Day',]) 
model.NECNight.Coral<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Night',]) 
model.NCPNet.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' ,]) 
model.R.Coral<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Night',]) 
model.GCP.Coral<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Coral' & AllData$DayNight=='Day',]) 

#rubble
model.NECNet.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' ,]) 
model.NECDay.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Day',]) 
model.NECNight.Rubble<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Night',]) 
model.NCPNet.Rubble<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' ,]) 
model.R.Rubble<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Night',]) 
model.GCP.Rubble<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Rubble' & AllData$DayNight=='Day',]) 

#Sand
model.NECNet.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' ,]) 
model.NECDay.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Day',]) 
model.NECNight.Sand<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Night',]) 
model.NCPNet.Sand<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' ,]) 
model.R.Sand<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Night',]) 
model.GCP.Sand<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Sand' & AllData$DayNight=='Day',]) 


#Mixed
model.NECNet.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' ,]) 
model.NECDay.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Day',]) 
model.NECNight.Mixed<-lmer(NEC.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Night',]) 
model.NCPNet.Mixed<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' ,]) 
model.R.Mixed<-lmer(NCP.AFDW~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Night',]) 
model.GCP.Mixed<-lmer(GPP~NutLevel +(1|Tank)+(1|DateTime), data=AllData[AllData$Substrate=='Mixed' & AllData$DayNight=='Day',]) 

#NEC plots net
### Respiration figure-------------
substrate<-c('Algae','Coral','Rubble','Sediment','Mixed')
#change layout so that mixed is centered
#boxcol=c('green','red','purple','yellow','white')
#png("RespirationModel.png", width=3800, height=4000, res=300)
#par(mfrow=c(3,2))
#layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,
 #               3,3,3,3,3,0,4,4,4,4,4,
  #              0,0,0,5,5,5,5,5,0,0,0), 3, 11, byrow = TRUE))
j<-2
par(bg=NA) 

## massive plot of all metabolic rates by substrate
par(mfrow=c(5,5))
par(oma=c(.1,.1,.1,.1)); 
par(mar=c(2,5,4,2)); 
#net NEC
#coral net NEC
y<-summary(model.NECNet.Coral)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Coral', ylim=c(0,max(abs(y))+5),
           ylab=expression(paste("Mean NEC")),
            cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
axis(2, cex.axis=1)

#algae
y<-summary(model.NECNet.algae)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Algae', ylim=c(-2,max(abs(y))+1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#rubble
y<-summary(model.NECNet.Rubble)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Rubble', ylim=c(-2,max(abs(y))),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.NECNet.Sand)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Sand', ylim=c(-4,max(abs(y))),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

#mixed
y<-summary(model.NECNet.Mixed)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Mixed', ylim=c(-1,max(abs(y))+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

##-- 

#Day NEC
#coral 
y<-summary(model.NECDay.Coral)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,max(abs(y))+6),
           ylab=expression(paste("mean Daytime NEC ")),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
axis(2, cex.axis=1)

#algae
y<-summary(model.NECDay.algae)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,max(abs(y))),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#rubble
y<-summary(model.NECDay.Rubble)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-2,max(abs(y))+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.NECDay.Sand)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-4,max(abs(y))+1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

#mixed
y<-summary(model.NECDay.Mixed)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-1,max(abs(y))+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

##--- 
#Night NEC
#coral 
y<-summary(model.NECNight.Coral)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,max(abs(y))+4),
           ylab=expression(paste("mean Nighttime NEC ")),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
axis(2, cex.axis=1)

#algae
y<-summary(model.NECNight.algae)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,max(abs(y))+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#rubble
y<-summary(model.NECNight.Rubble)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,0),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.NECNight.Sand)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-4,1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

#mixed
y<-summary(model.NECNight.Mixed)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,max(abs(y))),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

##--- 
#GPP
#coral 
y<-summary(model.GCP.Coral)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,60),
           ylab=expression(paste("mean GCP ")),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
axis(2, cex.axis=1)

#algae
y<-summary(model.GCP.algae)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,40),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#rubble
y<-summary(model.GCP.Rubble)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.GCP.Sand)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,25),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

#mixed
y<-summary(model.GCP.Mixed)$coefficients[,1:2]
x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,30),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
##--
#R
#coral 
y<-summary(model.R.Coral)$coefficients[,1:2]
x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,20),
           ylab=expression(paste("mean R ")),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
axis(2, cex.axis=1)

#algae
y<-summary(model.R.algae)$coefficients[,1:2]
x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#rubble
y<-summary(model.R.Rubble)$coefficients[,1:2]
x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.R.Sand)$coefficients[,1:2]
x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)

#mixed
y<-summary(model.R.Mixed)$coefficients[,1:2]
x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
          col='black', lwd=3)
text(x = x, par("usr")[3] + 2, par("usr")[1] - 1.5, labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE, srt=45)
#dev.off()