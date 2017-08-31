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


pdf("plots/MSplots/MeanRates_NEC.pdf", width=8, height=7)
j<-2
par(bg=NA) 
par(pty="m")
## massive plot of all metabolic rates by substrate
par(mfrow=c(3,5))
par(oma=c(.1,.1,.1,.1)); 
par(mar=c(1,4,3,1));
par(lwd = 2) 

#net NEC
#coral net NEC
y<-summary(model.NECNet.Coral)$coefficients[,1:2]
a<-anova(model.NECNet.Coral)

#calculate 95%CI using effects
ef<-as.data.frame(effect("NutLevel", model.NECNet.Coral))


# Make a function so I can minimize errors
Nutplot.NEC<-function(species, Main = TRUE, YLAB = TRUE){
  #calculate 95%CI using effects
  SE.upper<-ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species]
  SE.lower<-ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species]
  
  # If I want to go back to boxplots
  # x<-barplot(ef$fit, main=species, 
  #            ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
  #            yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
  #            ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
  #            cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette)
  # arrows(x, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species],x, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species], length=0.05, angle=90, code=3,
  #        col='black', lwd=2)
  
  plot(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
       ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB==TRUE, expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), NA),
       cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette, xaxt='n', xlim=c(0,4), cex=1.5)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, ef$fit+NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species],1:3, ef$fit-NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==species], length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2)
  #arrows(x,ef$lower,x,ef$upper, length=0.05, angle=90, code=3,
  #         col='black', lwd=2)
  
  #axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
  if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
    #  legend("top",'*', cex=2, bty="n")
    legend("top",'*', cex=2, bty="n")
  }
  #axis(2, cex.axis=1)
}

# plot it 
Nutplot.NEC('Coral')

#algae
ef<-as.data.frame(effect("NutLevel", model.NECNet.algae))
y<-summary(model.NECNet.algae)$coefficients[,1:2]
a<-anova(model.NECNet.algae)
Nutplot.NEC('Algae',TRUE,FALSE)

#rubble
ef<-as.data.frame(effect("NutLevel", model.NECNet.Rubble))
y<-summary(model.NECNet.Rubble)$coefficients[,1:2]
a<-anova(model.NECNet.Rubble)
Nutplot.NEC('Rubble',TRUE,FALSE)

#sand
ef<-as.data.frame(effect("NutLevel", model.NECNet.Sand))
y<-summary(model.NECNet.Sand)$coefficients[,1:2]
a<-anova(model.NECNet.Sand)
Nutplot.NEC('Sand',TRUE,FALSE)

#mixed
y<-summary(model.NECNet.Mixed)$coefficients[,1:2]
a<-anova(model.NECNet.Mixed)
ef<-as.data.frame(effect("NutLevel", model.NECNet.Mixed))
Nutplot.NEC('Mixed',TRUE,FALSE)


##-- 
par(mar=c(2,4,1,1)); 
#Day NEC
# new function for day night
Nutplot.NECDN<-function(species, Main = TRUE, YLAB = TRUE,DN, tx = FALSE){
  #calculate 95%CI using effects
  SE.upper<-ef$fit+NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species & NEC.mean.DayNight$DayNight==DN]
  SE.lower<-ef$fit-NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==species & NEC.mean.DayNight$DayNight==DN]
  
  plot(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
       ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB==TRUE, expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), NA),
       cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette, xaxt='n', xlim=c(0,4), xlab="", cex=1.5)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2)
   if(tx == TRUE){ # add x labels
     text(x = 1:3-0.75, par("usr")[3]-0.5 ,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
        }
  #axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
  if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
    #  legend("top",'*', cex=2, bty="n")
    legend("top",'*', cex=2, bty="n")
  }
  #axis(2, cex.axis=1)
}

#coral 
y<-summary(model.NECDay.Coral)$coefficients[,1:2]
a<-anova(model.NECDay.Coral)
ef<-as.data.frame(effect("NutLevel", model.NECDay.Mixed))

Nutplot.NECDN('Coral', FALSE, TRUE,'Day')
#algae
y<-summary(model.NECDay.algae)$coefficients[,1:2]
a<-anova(model.NECDay.algae)
ef<-as.data.frame(effect("NutLevel", model.NECDay.algae))

Nutplot.NECDN('Algae', FALSE, FALSE,'Day')

#rubble
y<-summary(model.NECDay.Rubble)$coefficients[,1:2]
a<-anova(model.NECDay.Rubble)
ef<-as.data.frame(effect("NutLevel", model.NECDay.Rubble))
Nutplot.NECDN('Rubble', FALSE, FALSE,'Day')


#sand
y<-summary(model.NECDay.Sand)$coefficients[,1:2]
a<-anova(model.NECDay.Sand)
ef<-as.data.frame(effect("NutLevel", model.NECDay.Sand))

Nutplot.NECDN('Sand', FALSE, FALSE,'Day')

#mixed
y<-summary(model.NECDay.Mixed)$coefficients[,1:2]
a<-anova(model.NECDay.Mixed)
ef<-as.data.frame(effect("NutLevel", model.NECDay.Mixed))

Nutplot.NECDN('Mixed', FALSE, FALSE,'Day')

##--- 
#Night NEC
par(mar=c(3,4,1,1)); #6
#coral 
y<-summary(model.NECNight.Coral)$coefficients[,1:2]
a<-anova(model.NECNight.Coral)
ef<-as.data.frame(effect("NutLevel", model.NECNight.Coral))
Nutplot.NECDN('Coral', FALSE, TRUE,'Night','TRUE')

#algae
y<-summary(model.NECNight.algae)$coefficients[,1:2]
a<-anova(model.NECNight.algae)
ef<-as.data.frame(effect("NutLevel", model.NECNight.algae))

Nutplot.NECDN('Algae', FALSE, FALSE,'Night','TRUE')


#rubble
y<-summary(model.NECNight.Rubble)$coefficients[,1:2]
a<-anova(model.NECNight.Rubble)
ef<-as.data.frame(effect("NutLevel", model.NECNight.Rubble))

Nutplot.NECDN('Rubble', FALSE, FALSE,'Night','TRUE')


#sand
y<-summary(model.NECNight.Sand)$coefficients[,1:2]
a<-anova(model.NECNight.Sand)
ef<-as.data.frame(effect("NutLevel", model.NECNight.Sand))
Nutplot.NECDN('Sand', FALSE, FALSE,'Night','TRUE')

#mixed
y<-summary(model.NECNight.Mixed)$coefficients[,1:2]
a<-anova(model.NECNight.Mixed)
ef<-as.data.frame(effect("NutLevel", model.NECNight.Mixed))
Nutplot.NECDN('Mixed', FALSE, FALSE,'Night','TRUE')
dev.off()
#---
## NCP ---------------------------------------------------------------------- 

pdf("plots/MSplots/MeanRates_NCP.pdf", width=8, height=7)

par(mfrow=c(3,5))
par(mar=c(1,4,3,1)); 
par(bg=NA) 
par(pty="m")
## massive plot of all metabolic rates by substrate
par(mfrow=c(3,5))
par(oma=c(.1,.1,.1,.1)); 
par(lwd = 2) 

#NCP
#coral 
y<-summary(model.NCPNet.Coral)$coefficients[,1:2]
a<-anova(model.NCPNet.Coral)
ef<-as.data.frame(effect("NutLevel", model.NCPNet.Coral))


# Make a function so I can minimize errors
Nutplot.NCP<-function(species, Main = TRUE, YLAB = TRUE){
  #calculate 95%CI using effects
  SE.upper<-ef$fit+NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==species]
  SE.lower<-ef$fit-NCP.mean.Net$SE.AFDW2[NCP.mean.Net$Substrate==species]
  
    plot(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
       ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB==TRUE, expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})), NA),
       cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette, xaxt='n', xlim=c(0,4), cex=1.5)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2)
    if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
    #  legend("top",'*', cex=2, bty="n")
    legend("top",'*', cex=2, bty="n")
  }
 }
Nutplot.NCP('Coral', TRUE,TRUE)

#algae
y<-summary(model.NCPNet.algae)$coefficients[,1:2]
a<-anova(model.NCPNet.algae)
ef<-as.data.frame(effect("NutLevel", model.NCPNet.algae))
Nutplot.NCP('Algae', TRUE,FALSE)


#rubble
y<-summary(model.NCPNet.Rubble)$coefficients[,1:2]
a<-anova(model.NCPNet.Rubble)
ef<-as.data.frame(effect("NutLevel", model.NCPNet.Rubble))
Nutplot.NCP('Rubble', TRUE,FALSE)


#sand
y<-summary(model.NCPNet.Sand)$coefficients[,1:2]
a<-anova(model.NCPNet.Sand)
ef<-as.data.frame(effect("NutLevel", model.NCPNet.Sand))
Nutplot.NCP('Sand', TRUE,FALSE)

#mixed
y<-summary(model.NCPNet.Mixed)$coefficients[,1:2]
a<-anova(model.NCPNet.Mixed)
ef<-as.data.frame(effect("NutLevel", model.NCPNet.Mixed))
Nutplot.NCP('Mixed', TRUE,FALSE)


##--

##--- 
#GPP
#coral 
#par(mar=c(2,4,2,1));
par(mar=c(2,4,1,1)); 
y<-summary(model.GCP.Coral)$coefficients[,1:2]
a<-anova(model.GCP.Coral)
ef<-as.data.frame(effect("NutLevel", model.GCP.Coral))

Nutplot.GPP<-function(species, Main = TRUE, YLAB = TRUE){
  #calculate 95%CI using effects
  SE.upper<-ef$fit+NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate==species]
  SE.lower<-ef$fit-NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate==species]
  
  plot(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
       ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB==TRUE, expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})), NA),
       cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette, xaxt='n', xlim=c(0,4), cex=1.5)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2)
  if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
    #  legend("top",'*', cex=2, bty="n")
    legend("top",'*', cex=2, bty="n")
  }
}

Nutplot.GPP('Coral', FALSE,TRUE)


#algae
y<-summary(model.GCP.algae)$coefficients[,1:2]
a<-anova(model.GCP.algae)
ef<-as.data.frame(effect("NutLevel", model.GCP.algae))
Nutplot.GPP('Algae', FALSE,FALSE)


#rubble
y<-summary(model.GCP.Rubble)$coefficients[,1:2]
a<-anova(model.GCP.Rubble)
ef<-as.data.frame(effect("NutLevel", model.GCP.Rubble))
Nutplot.GPP('Rubble', FALSE,FALSE)


#sand
y<-summary(model.GCP.Sand)$coefficients[,1:2]
a<-anova(model.GCP.Sand)
ef<-as.data.frame(effect("NutLevel", model.GCP.Sand))
Nutplot.GPP('Sand', FALSE,FALSE)

#mixed
y<-summary(model.GCP.Mixed)$coefficients[,1:2]
a<-anova(model.GCP.Mixed)
ef<-as.data.frame(effect("NutLevel", model.GCP.Mixed))
Nutplot.GPP('Mixed', FALSE,FALSE)


##--
#R
#par(mar=c(4,4,2,1));
par(mar=c(3,4,1,1)); #6


# new function for respiration
Nutplot.R<-function(species, Main = TRUE, YLAB = TRUE, tx = FALSE){
  #calculate 95%CI using effects
  SE.upper<-ef$fit+NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species & NCP.mean.DayNight$DayNight=='Night']
  SE.lower<-ef$fit-NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==species & NCP.mean.DayNight$DayNight=='Night']
  
  plot(1:3,ef$fit, main=ifelse(Main ==TRUE, species,NA), pch=19,
       ylim=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper)))),
       yaxp=c(c(ifelse(min(SE.lower)>0,0,floor(min(SE.lower))),ceiling(max(SE.upper))), 5),
       ylab=ifelse(YLAB==TRUE, expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})), NA),
       cex.main=2, cex.axis=1, cex.lab=1,  col=mypalette, xaxt='n', xlim=c(0,4), xlab="", cex=1.5)
  lines(1:3,ef$fit, col = 'black', type = 'c' )
  
  arrows(1:3, SE.upper,1:3, SE.lower, length=0.05, angle=90, code=3,
         col=mypalette, lwd=2)
  
  abline(h=0, lty=2)
  if(tx == TRUE){ # add x labels
    text(x = 1:3-0.75, par("usr")[3]-0.5 ,  labels = c("Ambient","Medium","High"), srt = 45, pos = 1, xpd = TRUE)
  }
  #axis(1, at=x, labels=c("Ambient","Medium","High"), cex.axis=2, tick=FALSE)
  if(a$`Pr(>F)`<=0.055){ #add a star to the graph if it is statistically significant
    #  legend("top",'*', cex=2, bty="n")
    legend("top",'*', cex=2, bty="n")
  }
  
}

#coral 
y<-summary(model.R.Coral)$coefficients[,1:2]
a<-anova(model.R.Coral)
ef<-as.data.frame(effect("NutLevel", model.R.Coral))

Nutplot.R('Coral', FALSE,TRUE,TRUE)


#algae
y<-summary(model.R.algae)$coefficients[,1:2]
a<-anova(model.R.algae)
ef<-as.data.frame(effect("NutLevel", model.R.algae))
Nutplot.R('Algae', FALSE,FALSE,TRUE)

#rubble
y<-summary(model.R.Rubble)$coefficients[,1:2]
a<-anova(model.R.Rubble)
ef<-as.data.frame(effect("NutLevel", model.R.Rubble))
Nutplot.R('Rubble', FALSE,FALSE,TRUE)


#sand
y<-summary(model.R.Sand)$coefficients[,1:2]
a<-anova(model.R.Sand)

ef<-as.data.frame(effect("NutLevel", model.R.Sand))
Nutplot.R('Sand', FALSE,FALSE,TRUE)



#mixed
y<-summary(model.R.Mixed)$coefficients[,1:2]
a<-anova(model.R.Mixed)

ef<-as.data.frame(effect("NutLevel", model.R.Mixed))
Nutplot.R('Mixed', FALSE,FALSE,TRUE)

dev.off()