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


pdf("plots/MSplots/MeanRates.pdf", width=8, height=8)
j<-2
par(bg=NA) 
par(pty="m")
## massive plot of all metabolic rates by substrate
par(mfrow=c(3,5))
par(oma=c(.1,.1,.1,.1)); 
par(mar=c(1,4,3,1)); 
#net NEC
#coral net NEC
y<-summary(model.NECNet.Coral)$coefficients[,1:2]
a<-anova(model.NECNet.Coral)

#calculates using SE from the model... not quite right
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Coral', ylim=c(0,par('usr')[4]+5),
 #          ylab=expression(paste("Mean NEC")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

#calculate 95%CI using effects
ef<-as.data.frame(effect("NutLevel", model.NECNet.Coral))
par(lwd = 2) 
SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Coral']
SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Coral']
x<-barplot(ef$fit, main='Coral', 
           ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Coral'],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Coral'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #         col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
      #  legend("top",'*', cex=2, bty="n")
        legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)

#algae
y<-summary(model.NECNet.algae)$coefficients[,1:2]
a<-anova(model.NECNet.algae)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Algae', ylim=c(-2,par('usr')[4]+1),
 #          #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNet.algae))
SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Algae']
SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Algae']
x<-barplot(ef$fit, main='Algae', 
           ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Algae'],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Algae'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#rubble
y<-summary(model.NECNet.Rubble)$coefficients[,1:2]
a<-anova(model.NECNet.Rubble)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Rubble', ylim=c(-1.5,1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNet.Rubble))
SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Rubble']
SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Rubble']
x<-barplot(ef$fit, main='Rubble', 
           ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Rubble'],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Rubble'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
##axis(2, cex.axis=1)


#sand
y<-summary(model.NECNet.Sand)$coefficients[,1:2]
a<-anova(model.NECNet.Sand)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Sand', ylim=c(-4,par('usr')[4]),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNet.Sand))
SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Sand']
SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Sand']
x<-barplot(ef$fit, main='Sand', 
           ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Sand'],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Sand'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)


#mixed
y<-summary(model.NECNet.Mixed)$coefficients[,1:2]
a<-anova(model.NECNet.Mixed)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]), main='Mixed', ylim=c(-1,par('usr')[4]+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNet.Mixed))
SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Mixed']
SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Mixed']
x<-barplot(ef$fit, main='Mixed', ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
         #  ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
         yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Mixed'],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate=='Mixed'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)


##-- 
par(mar=c(2,4,1,1)); 
#Day NEC
#coral 
y<-summary(model.NECDay.Coral)$coefficients[,1:2]
a<-anova(model.NECDay.Coral)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,par('usr')[4]+6),
 #          ylab=expression(paste("Mean Daytime NEC ")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECDay.Mixed))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Day']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Day']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("Day NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Day'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Day'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=1)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
##axis(2, cex.axis=1)

#algae
y<-summary(model.NECDay.algae)$coefficients[,1:2]
a<-anova(model.NECDay.algae)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,par('usr')[4]),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECDay.algae))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Day']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Day']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
          # ylab=expression(paste("Day NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Day'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Day'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)

#rubble
y<-summary(model.NECDay.Rubble)$coefficients[,1:2]
a<-anova(model.NECDay.Rubble)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-2,par('usr')[4]+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECDay.Rubble))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Day']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Day']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
          # ylab=expression(paste("Mean Daytime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Day'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Day'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)

#sand
y<-summary(model.NECDay.Sand)$coefficients[,1:2]
a<-anova(model.NECDay.Sand)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-4,par('usr')[4]+1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECDay.Sand))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Day']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Day']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           # ylab=expression(paste("Mean Daytime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Day'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Day'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)

#mixed
y<-summary(model.NECDay.Mixed)$coefficients[,1:2]
a<-anova(model.NECDay.Mixed)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-1,par('usr')[4]+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECDay.Mixed))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Day']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Day']

x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           # ylab=expression(paste("Mean Daytime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
  #arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Day'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Day'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(2, cex.axis=1)
##--- 
#Night NEC
par(mar=c(6,4,1,1)); 
#coral 
y<-summary(model.NECNight.Coral)$coefficients[,1:2]
a<-anova(model.NECNight.Coral)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,par('usr')[4]+4),
 #          ylab=expression(paste("Mean Nighttime NEC ")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNight.Coral))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Night']
           
x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper))), 5),
            ylab=expression(paste("Night NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Coral' & NEC.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=1)
text(x = x-0.5, par("usr")[3] - 1,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#algae
y<-summary(model.NECNight.algae)$coefficients[,1:2]
a<-anova(model.NECNight.algae)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,par('usr')[4]+2),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNight.algae))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Night']

x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper))), 5),
          # ylab=expression(paste("Mean Nighttime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Algae' & NEC.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
text(x = x-0.5, par("usr")[3] - 0.5,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
text(x = x-0.5, par("usr")[3] - 0.5,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#rubble
y<-summary(model.NECNight.Rubble)$coefficients[,1:2]
a<-anova(model.NECNight.Rubble)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,0),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNight.Rubble))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Night']

x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper))), 5),
            # ylab=expression(paste("Mean Nighttime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Rubble' & NEC.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

text(x = x-0.5, par("usr")[3] - 0.25,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)

#sand
y<-summary(model.NECNight.Sand)$coefficients[,1:2]
a<-anova(model.NECNight.Sand)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-4,1),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNight.Sand))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Night']

x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper))), 5),
           # ylab=expression(paste("Mean Nighttime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Sand' & NEC.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
text(x = x-0.5, par("usr")[3] -0.25,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#mixed
y<-summary(model.NECNight.Mixed)$coefficients[,1:2]
a<-anova(model.NECNight.Mixed)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-3,par('usr')[4]),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NECNight.Mixed))
SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           # ylab=expression(paste("Mean Nighttime NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate=='Mixed' & NEC.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
text(x = x-0.5, par("usr")[3] - 0.25,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
##--- 
par(mfrow=c(3,5))
par(mar=c(1,4,3,1)); 
#NCP
#coral 
y<-summary(model.NCPNet.Coral)$coefficients[,1:2]
a<-anova(model.NCPNet.Coral)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,20), main = 'Coral',
 #          ylab=expression(paste("Mean NCP ")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NCPNet.Coral))
SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Coral']
SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Coral']

x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey', main='Coral')
arrows(x, ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Coral'],
       x, ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Coral'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=1)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#algae
y<-summary(model.NCPNet.algae)$coefficients[,1:2]
a<-anova(model.NCPNet.algae)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,20),main = 'Algae',
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NCPNet.algae))
SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Algae']
SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Algae']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),main='Algae',
         #  ylab=expression(paste("Mean NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Algae'],
       x, ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Algae'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#rubble
y<-summary(model.NCPNet.Rubble)$coefficients[,1:2]
a<-anova(model.NCPNet.Rubble)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-5,5),main = 'Rubble',
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NCPNet.Rubble))
SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Rubble']
SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Rubble']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper)))),
           yaxp=c(c(ifelse(min(ef$lower)>0,0,floor(min(ef$lower))),ceiling(max(ef$upper))), 5),main='Rubble',
           #ylab=expression(paste("Mean NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Rubble'],
       x, ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Rubble'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)

#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#sand
y<-summary(model.NCPNet.Sand)$coefficients[,1:2]
a<-anova(model.NCPNet.Sand)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-5,6),main = 'Sand',
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NCPNet.Sand))
SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Sand']
SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Sand']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           main='Sand',
           #ylab=expression(paste("Mean NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Sand'],
       x, ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Sand'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
#mixed
y<-summary(model.NCPNet.Mixed)$coefficients[,1:2]
a<-anova(model.NCPNet.Mixed)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),main = 'Mixed',
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
#           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.NCPNet.Mixed))
SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Mixed']
SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Mixed']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),main='Mixed',
           #ylab=expression(paste("Mean NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Mixed'],
       x, ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate=='Mixed'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
##--

##--- 
#GPP
#coral 
par(mar=c(2,4,2,1)); 
y<-summary(model.GCP.Coral)$coefficients[,1:2]
a<-anova(model.GCP.Coral)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,60),
 #          ylab=expression(paste("Mean GCP ")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.GCP.Coral))
SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Coral']
SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Coral']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Coral'],
       x, ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Coral'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
       #col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=1)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#algae
y<-summary(model.GCP.algae)$coefficients[,1:2]
a<-anova(model.GCP.algae)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,40),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.GCP.algae))
SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Algae']
SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Algae']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           #ylab=expression(paste("Mean GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Algae'],
       x, ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Algae'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#rubble
y<-summary(model.GCP.Rubble)$coefficients[,1:2]
a<-anova(model.GCP.Rubble)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,15),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.GCP.Rubble))
SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Rubble']
SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Rubble']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           #ylab=expression(paste("Mean GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Rubble'],
       x, ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Rubble'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#sand
y<-summary(model.GCP.Sand)$coefficients[,1:2]
a<-anova(model.GCP.Sand)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,25),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.GCP.Sand))
SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Sand']
SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Sand']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           #ylab=expression(paste("Mean GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Sand'],
       x, ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Sand'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}

#mixed
y<-summary(model.GCP.Mixed)$coefficients[,1:2]
a<-anova(model.GCP.Mixed)
#x<-barplot(c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,30),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.GCP.Mixed))
SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Mixed']
SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Mixed']  
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
           #ylab=expression(paste("Mean GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Mixed'],
       x, ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate=='Mixed'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
##--
#R
par(mar=c(4,4,2,1)); 
#coral 
y<-summary(model.R.Coral)$coefficients[,1:2]
a<-anova(model.R.Coral)
#x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,25),
 #          ylab=expression(paste("Mean R ")),
  #         cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)

ef<-as.data.frame(effect("NutLevel", model.R.Coral))
SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Coral' & NCP.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Coral' & NCP.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0)),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0), 5),
           ylab=expression(paste("R ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Coral' & NCP.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Coral'& NCP.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(2, cex.axis=1)
text(x = x-0.5, par("usr")[3] - 2,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("bottom",'*', cex=2, bty="n")
}

#algae
y<-summary(model.R.algae)$coefficients[,1:2]
a<-anova(model.R.algae)
#x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,12),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.R.algae))
SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Algae' & NCP.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Algae' & NCP.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0)),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0), 5),
           #ylab=expression(paste("Mean R ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Algae' & NCP.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Algae'& NCP.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
text(x = x-0.5, par("usr")[3] - 1,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("bottom",'*', cex=2, bty="n")
}

#rubble
y<-summary(model.R.Rubble)$coefficients[,1:2]
a<-anova(model.R.Rubble)
#x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,12),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.R.Rubble))
SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Rubble' & NCP.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Rubble' & NCP.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0)),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0), 5),
           #ylab=expression(paste("Mean R ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Rubble' & NCP.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Rubble'& NCP.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
#axis(2, cex.axis=2)
text(x = x-0.5, par("usr")[3] - 1,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("bottom",'*', cex=2, bty="n")
}

#sand
y<-summary(model.R.Sand)$coefficients[,1:2]
a<-anova(model.R.Sand)
#x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(-2,12),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.R.Sand))
SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Sand' & NCP.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Sand' & NCP.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0)),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0), 5),
           #ylab=expression(paste("Mean R ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Sand' & NCP.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Sand'& NCP.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

text(x = x-0.5, par("usr")[3] - 1,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("bottom",'*', cex=2, bty="n")
}

#mixed
y<-summary(model.R.Mixed)$coefficients[,1:2]
a<-anova(model.R.Mixed)
#x<-barplot(-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),  ylim=c(0,12),
           #ylab=expression(paste("mean NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
 #          cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
#errorbars(x,-1*c(y[1,1], y[1,1]+y[2,1], y[1,1]+y[3,1]),0,-1*c(y[1,2], y[2,2], y[3,2]),
 #         col='black', lwd=2)

ef<-as.data.frame(effect("NutLevel", model.R.Mixed))
SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Mixed' & NCP.mean.DayNight$DayNight=='Night']
SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Mixed' & NCP.mean.DayNight$DayNight=='Night']
x<-barplot(ef$fit, ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0)),
           yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),0), 5),
           #ylab=expression(paste("Mean R ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
           cex.main=2, cex.axis=1, cex.lab=1,  col='grey')
arrows(x, ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Mixed' & NCP.mean.DayNight$DayNight=='Night'],
       x, ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate=='Mixed'& NCP.mean.DayNight$DayNight=='Night'], length=0.05, angle=90, code=3,
       col='black', lwd=2)
abline(h=0)
#arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
 #      col='black', lwd=2)

text(x = x-0.5, par("usr")[3] - 1,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
#axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE, srt=45)
if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
   legend("top",'*', cex=2, bty="n")
}
dev.off()