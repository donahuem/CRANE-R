DOC.means <- ddply(AllDOC, c("Substrate","NutLevel", "Days"), summarize,
                   #Mean.AFDW2.p = mean(NCP.AFDW[NCP.AFDW>0], na.rm=T),
                   N=sum(!is.na(DOC)),
                   r =mean(Mean.AFDW2.r, na.rm = T),
                   r.SE=sd(Mean.AFDW2.r)/sqrt(N),
                   NCP = mean(Mean.AFDW2.NCP, na.rm=T),
                   NCP.SE=sd(Mean.AFDW2.NCP)/sqrt(N),
                   GPP.mean=mean(GPP, na.rm=T),
                   GPP.SE=sd(GPP)/sqrt(N),
                   PR=mean(PR.AFDW2, na.rm=T),
                   PR.SE=sd(PR.AFDW2)/sqrt(N),        
                   NEC.Day = mean(Mean.AFDW2.Day, na.rm=T), #day NEC
                   NEC.Day.SE=sd(Mean.AFDW2.Day)/sqrt(N),        
                   NEC.Night = mean(Mean.AFDW2.Night, na.rm=T), #Night NEC
                   NEC.Night.SE=sd(Mean.AFDW2.Night)/sqrt(N),
                   NEC.Net = mean(Mean.AFDW2.NEC, na.rm=T), #Night NEC
                   NEC.Net.SE=sd(Mean.AFDW2.NEC)/sqrt(N),
                   NEC.NCP.mean= mean(Slope, na.rm=T),
                   NEC.NCP.SE=sd(Slope)/sqrt(N),
                   DOC.mean = mean(DOC, na.rm=T),
                   DOC.SE=sd(DOC)/sqrt(N),
                   ExcessDOC.mean = mean(excessDOC),
                   EDOC.SE=sd(excessDOC)/sqrt(N)
                   
)

coralX<-c(-20,50)
coralY<-c(0,0.2)

AlgaeX<-c(-10,10)
AlgaeY<-c(-0.1,0.1)

RubbleX<-c(-20,20)
RubbleY<-c(-0.2,0.5)

SandX<-c(-20,40)
SandY<-c(-.05,0.2)

#plot DOC versus NEC:NCP ratios
par(mfrow=c(2,2))
plot(0,type='n', ylim=c(-0.1,0.1), xlab='DOC', ylab='NEC:NCP', main ='Algae', xlim=AlgaeX)
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
  y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         x - DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=coralX, ylim=c(0,0.2), xlab='DOC', ylab='NEC:NCP', main='Coral')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
  y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         x - DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         angle=90, code=3, length = 0.1)
}
legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')


plot(0,type='n', xlim=RubbleX, ylim=c(-0.2,0.5), xlab='DOC', ylab='NEC:NCP', main='Rubble')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
  y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         x - DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=SandX, ylim=c(-.05,0.2), xlab='DOC', ylab='NEC:NCP', main='Sand')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
  y=DOC.means$NEC.NCP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         x, y - DOC.means$NEC.NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         x - DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         angle=90, code=3, length = 0.1)
}

###---------------
#Same plot versus GPP
#plot DOC versus NEC:NCP ratios
par(mfrow=c(2,2))
plot(0,type='n', xlim=AlgaeX, ylim=c(0,50), xlab='DOC', ylab='GPP', main ='Algae')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
  y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         x - DOC.means$EDOC.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=coralX, ylim=c(0,60), xlab='DOC', ylab='GPP', main='Coral')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
  y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         angle=90, code=3, length = 0.1)
}
legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')


plot(0,type='n', xlim=RubbleX, ylim=c(0,15), xlab='DOC', ylab='GPP', main='Rubble')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
  y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=SandX, ylim=c(0,30), xlab='DOC', ylab='GPP', main='Sand')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
  y=DOC.means$GPP.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         x, y - DOC.means$GPP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         angle=90, code=3, length = 0.1)
}

#Same plot versus R--------------------

par(mfrow=c(2,2))
plot(0,type='n', xlim=AlgaeX, ylim=c(0,15), xlab='DOC', ylab='R', main ='Algae')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
  y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=coralX, ylim=c(0,30), xlab='DOC', ylab='R', main='Coral')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
  y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         angle=90, code=3, length = 0.1)
}
legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')


plot(0,type='n', xlim=RubbleX, ylim=c(0,10), xlab='DOC', ylab='R', main='Rubble')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
  y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=SandX, ylim=c(0,15), xlab='DOC', ylab='R', main='Sand')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
  y=DOC.means$r[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         x, y - DOC.means$r.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         angle=90, code=3, length = 0.1)
}

#-------------
#Same plot versus NCP--------------------

par(mfrow=c(2,2))
plot(0,type='n', xlim=AlgaeX, ylim=c(0,20), xlab='DOC', ylab='NCP', main ='Algae')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
  y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=coralX, ylim=c(0,10), xlab='DOC', ylab='NCP', main='Coral')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
  y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         angle=90, code=3, length = 0.1)
}
legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')


plot(0,type='n', xlim=RubbleX, ylim=c(-5,1), xlab='DOC', ylab='NCP', main='Rubble')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
  y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=SandX, ylim=c(-2,6), xlab='DOC', ylab='NCP', main='Sand')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
  y=DOC.means$NCP[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         x, y - DOC.means$NCP.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         angle=90, code=3, length = 0.1)
}

#-

#-------------
#Same plot versus NEC--------------------

par(mfrow=c(2,2))
plot(0,type='n', xlim=AlgaeX, ylim=c(-3,2), xlab='DOC', ylab='NEC', main ='Algae')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae']
  y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Algae'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Algae'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=coralX, ylim=c(0,15), xlab='DOC', ylab='NEC', main='Coral')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral']
  y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Coral'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Coral'], y, 
         angle=90, code=3, length = 0.1)
}
legend('topright', legend=c('Ambient','Medium','High'), col = cols, pch=19, bty = 'n')


plot(0,type='n', xlim=RubbleX, ylim=c(-2,0), xlab='DOC', ylab='NEC', main='Rubble')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble']
  y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Rubble'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Rubble'], y, 
         angle=90, code=3, length = 0.1)
}

plot(0,type='n', xlim=SandX, ylim=c(-4,2), xlab='DOC', ylab='NEC', main='Sand')
for (j in 1:3){ #nutrients
  #par(new = TRUE)
  x=DOC.means$ExcessDOC.mean[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand']
  y=DOC.means$NEC.Net[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand']
  points(x,y,type = 'p', col = cols[j], pch=19)
  arrows(x, y + DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         x, y - DOC.means$NEC.Net.SE[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j]& DOC.means$Substrate=='Sand'], 
         angle=90, code=3, length = 0.1)
  arrows(x + DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         x - DOC.means$EDOC[DOC.means$Days=='14' & DOC.means$NutLevel==Nuts[j] & DOC.means$Substrate=='Sand'], y, 
         angle=90, code=3, length = 0.1)
}


#Plot NEC versus NCP-----------------------
b<-matrix(nrow=5,ncol=3)
for(i in 1:5){
  plot(0,type='n', xlim=c(-20,60), ylim=c(-10,20), xlab='NCP', ylab='NEC',main=sub[i])
  for (j in 1:3){
    #par(new = TRUE)
    points(AllData$NCP.AFDW[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]],AllData$NEC.AFDW[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]], main=sub[i], 
           type = 'p', col = cols[j], pch=19)
    
    
    modelTA.DIC<-lm(AllData$NEC.AFDW[AllData$NutLevel==Nuts[j]& AllData$Substrate==sub[i]]  ~AllData$NCP.AFDW[AllData$NutLevel==Nuts[j] & AllData$Substrate==sub[i]])
    b0[i,j]<-modelTA.DIC$coefficients[1]
    b[i,j]<-modelTA.DIC$coefficients[2]
    r2[i,j]<-summary(modelTA.DIC)$r.squared
    substrate[i,j]<-sub[i]
    Nut[i,j]<-Nuts[j]
    x<-seq(from=-20, to=60,by=0.5)
    y<-b[i,j]*x+b0[i,j]
    lines(x,y, col=cols[j], lwd=3)
    
    
    
  }
  legend('topleft',legend=c('Ambient',"Medium","High"), col=cols, pch=19, bty = 'n')
}