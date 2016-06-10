### Figures for ICRS

## NEC versus Omega made for a black background
png("NECvsOmegaICRS.png", width=2800, height=3200, res=300)
par(bg=NA) 
par(mfrow=c(3,2))
cols <- c(239,117,254)
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
for (i in 1:length(sub)){
  plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
        main = sub[i], xlim=c(1.5,6), col.main="white", col.axis = 'white',
       col.lab='white', cex.lab=3, cex.main=3, cex.axis=1.5,ylab="")
      # ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  
  abline(h=0, lty=2, col='white')
  axis(1,  col.tick= "white", col.axis="white", cex.axis=1.5)
  
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], cex.axis=1.5,
         y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = cols[j],col.lab = 'white', 
         pch=21, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), col.axis = 'white')
    
    model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
    lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3,col.lab = 'white',
          xaxt='n')
   axis(2,  col.tick = "white", col.axis="white", cex.axis=1.5)
    box(col = 'white')
    }
}
#legend('top', horiz = TRUE, legend=unique(NEC.mean$NutLevel), col=c(239,117,254), pch=19, bty = 'n', text.col = 'white')

dev.off()


### now just coral to use as an example adding one nutrient at a time

# only ambient
png("CoralOmegaICRS1.png", width=2800, height=3200, res=300)
par(bg=NA) 
par(mfrow=c(1,1))
cols <- c(239,117,254)
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
i=2
plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
     main = sub[i], xlim=c(1.5,6), col.main="white", col.axis = 'white',
     col.lab='white',  cex.main=3, cex.axis=1.5,ylab="", cex.lab=3)
# ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))

abline(h=0, lty=2, col='white')
axis(1,  col.tick= "white", col.axis="white", cex.axis=1.5)

for (j in 1){
  par(new = TRUE)
  plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], cex.axis=1.5,cex=2,
       y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = cols[j],col.lab = 'white', 
       pch=21, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), col.axis = 'white')
  
  model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
  lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3,col.lab = 'white',
        xaxt='n')
  axis(2,  col.tick = "white", col.axis="white", cex.axis=1.5)
  box(col = 'white')
  
}
#legend('top', horiz = TRUE, legend=unique(NEC.mean$NutLevel), col=c(239,117,254), pch=19, bty = 'n', text.col = 'white')

dev.off()


## NEC versus Omega made for a black background
png("CoralOmegaICRS2.png", width=2800, height=3200, res=300)
par(bg=NA) 
par(mfrow=c(1,1))
cols <- c(239,117,254)
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
i=2
plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
     main = sub[i], xlim=c(1.5,6), col.main="white", col.axis = 'white',
     col.lab='white',  cex.main=3, cex.axis=1.5,ylab="", cex.lab=3)
# ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))

abline(h=0, lty=2, col='white')
axis(1,  col.tick= "white", col.axis="white", cex.axis=1.5)

for (j in 1:2){
  par(new = TRUE)
  plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], cex.axis=1.5,cex=2,
       y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = cols[j],col.lab = 'white', 
       pch=21, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), col.axis = 'white')
  
  model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
  lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3,col.lab = 'white',
        xaxt='n')
  axis(2,  col.tick = "white", col.axis="white", cex.axis=1.5)
  box(col = 'white')
  
}
#legend('top', horiz = TRUE, legend=unique(NEC.mean$NutLevel), col=c(239,117,254), pch=19, bty = 'n', text.col = 'white')

dev.off()


## NEC versus Omega made for a black background
png("CoralOmegaICRS3.png", width=2800, height=3200, res=300)
par(bg=NA) 
par(mfrow=c(1,1))
cols <- c(239,117,254)
y<-AllData$NEC.AFDW
yse<-NEC.mean$SE.AFDW
i=2
  plot(NA, xlab=expression(paste(Omega)[arag]),ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), 
       main = sub[i], xlim=c(1.5,6), col.main="white", col.axis = 'white',
       col.lab='white',  cex.main=3, cex.axis=1.5,ylab="", cex.lab=3)
  # ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})))
  
  abline(h=0, lty=2, col='white')
  axis(1,  col.tick= "white", col.axis="white", cex.axis=1.5)
  
  for (j in 1:length(Nuts)){
    par(new = TRUE)
    plot(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], cex.axis=1.5,cex=2,
         y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], col = cols[j],col.lab = 'white', 
         pch=21, type="p", xaxt='n', xlim=c(1.5,6), ylab='', xlab='',ylim=c(min(y[AllData$Substrate==sub[i]]), max(y[AllData$Substrate==sub[i]])), col.axis = 'white')
    
    model<-lm(y[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]]~AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]])
    lines(AllData$TankOmegaArag[AllData$Substrate==sub[i] & AllData$NutLevel==Nuts[j]], model$fitted.values, col=cols[j], lwd=3,col.lab = 'white',
          xaxt='n')
    axis(2,  col.tick = "white", col.axis="white", cex.axis=1.5)
    box(col = 'white')
  
}
#legend('top', horiz = TRUE, legend=unique(NEC.mean$NutLevel), col=c(239,117,254), pch=19, bty = 'n', text.col = 'white')

dev.off()

png("Nutrients.png", width=3800, height=3200, res=300)
#Nutrient figures
par(mfrow=c(1,2)) #average nitrate for experiment
par(mar=c(5,6,4,2)+0.1 )
par(bg=NA)  #remove background
x<-barplot(meanNuts$meanNN, main=expression(paste('NO'[3]^{'2-'},'+ NO'[2]^{'-'})), 
           ylab=expression(paste(mu,'M')), ylim=c(0,10),col=c('lightblue','orange','hotpink'),
           col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white', border='white')
errorbars(x,meanNuts$meanNN,0,meanNuts$SENN, col='white')
axis(1, at=x, labels=c("Ambeint","Medium","High"),col.tick= "white", col.axis="white", cex.axis=2)
axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
box(col = 'white')

#average phosphate for experiment
x<-barplot(meanNuts$meanP, main=expression(paste('PO'[4]^{'3-'})),ylab='', ylim=c(0,2.5), border='white',
           col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3,col=c('lightblue','orange','hotpink'), col.main='white')
errorbars(x,meanNuts$meanP,0,meanNuts$SEP, col='white')
axis(1, at=x, labels=c("Ambeint","Medium","High"),col.tick= "white", col.axis="white", cex.axis=2)
axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
box(col = 'white')
dev.off()

### GPP figure-------------
substrate=c('Algae','Coral','Rubble','Sediment','Mixed')
#change layout so that mixed is centered
boxcol=c('green','red','purple','yellow','white')
png("GPP.png", width=3800, height=4000, res=300)
par(mfrow=c(3,2))
layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,
                3,3,3,3,3,0,4,4,4,4,4,
                0,0,0,5,5,5,5,5,0,0,0), 3, 11, byrow = TRUE))

par(bg=NA) 
for (i in 1:length(sub)){
  x<-barplot(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]], main=substrate[i], ylim=c(0,max(NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]])+15),
             #ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
             col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white', col='lightblue', border='white')
  errorbars(x,NCP.mean.PR$Mean.GPP[NCP.mean.PR$Substrate==sub[i]],0,NCP.mean.PR$SE.GPP[NCP.mean.PR$Substrate==sub[i]],
            col='white', lwd=3)
  
  axis(1, at=x, labels=c("Ambeint","Medium","High"),col.tick= "white", col.axis="white", cex.axis=2, tick=FALSE)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = boxcol[i])
}
dev.off()



### Respiration figure-------------
substrate<-c('Algae','Coral','Rubble','Sediment','Mixed')
#change layout so that mixed is centered
boxcol=c('green','red','purple','yellow','white')
png("Respiration.png", width=3800, height=4000, res=300)
par(mfrow=c(3,2))
layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,
                3,3,3,3,3,0,4,4,4,4,4,
                0,0,0,5,5,5,5,5,0,0,0), 3, 11, byrow = TRUE))

par(bg=NA) 
for (i in 1:length(sub)){
  y=NCP.mean.DayNight$Mean.AFDW2[NCP.mean.DayNight$Substrate==sub[i] & NCP.mean.DayNight$DayNight==DN[j]]
  
  x<-barplot(abs(y), main=substrate[i], ylim=c(0,max(abs(y))+15),
             #ylab=expression(paste("GCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
             col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white', col='lightblue', border='white')
  errorbars(x,abs(y),0,NCP.mean.DayNight$SE.AFDW2[NCP.mean.DayNight$Substrate==sub[i]& NCP.mean.DayNight$DayNight==DN[j]],
            col='white', lwd=3)
  
  axis(1, at=x, labels=c("Ambeint","Medium","High"),col.tick= "white", col.axis="white", cex.axis=2, tick=FALSE)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = boxcol[i])
}
dev.off()

## pH vs NCP----------------------------------
png("pH_NCP.png", width=2800, height=2500, res=300)
par(mfrow=c(1,1))
par(bg=NA) 
par(mar=c(6,6,4,2)+0.1 )
plot(AllData$NCP.AFDW, AllData$TankpH, col='hotpink', xlab='', ylab='pH',
     col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white', pch=19)
abline(v=0, lty=2, col='white')
axis(1, col.tick= "white", col.axis="white", cex.axis=2)
axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
box(col = 'white')

#model for NCP vs pH with nutrient level as a random effect
#modelpHNCP<-lmer(AllData$TankpH~AllData$NCP.AFDW+(1|AllData$NutLevel))
modelpHNCP<-lm(AllData$TankpH~AllData$NCP.AFDW)
#calculate rsquared
#r.squaredGLMM(modelpHNCP)
d<-order(AllData$NCP.AFDW)
y.new<-predict(modelpHNCP, interval='confidence')
lines(AllData$NCP.AFDW[d], y.new[d,2],col='red', lwd=1)
lines(AllData$NCP.AFDW[d], y.new[d,1],col='red', lwd=3)
lines(AllData$NCP.AFDW[d], y.new[d,3],col='red', lwd=1)
#move xlabel dowm
title(xlab=expression(paste("NCP ",mu,"mol g AFDW"^{-1}," hr"^{-1})), line=5, cex.lab=3, col.lab='white')
dev.off()

## pH time series
--------------------------
#show the differences across substrates for ambient and also across nutrients for sand as example
#Delta pH across time
  png("pH_Time.png", width=3400, height=2000, res=300)

  par(mfrow=c(1,2))
  par(mar=c(5,6,4,2)+0.1 )
par(bg=NA)
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


  plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white')
  
  abline(h=0, lty=2, col='white')
  par(new = TRUE)
  cols <- c('green','red','purple','orange','white')
  #
  # for (j in 1:length(Nutlevels)){
  for (i in 1:5){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
   }
  #shaded area for night
  rect(unique(deltapHMeans.time $DateTime)[a]+3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
  legend('top',legend=substrate, col=cols, pch=19, bty = 'n',text.col='white')
  



#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = sub[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white')
  
  abline(h=0, lty=2, col='white')
  par(new = TRUE)
  cols <- c('lightblue','orange','hotpink')

  for (j in 1:length(Nuts)){
    par(new = TRUE)
    
    plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
         y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
         col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white')
    
    arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
           + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
           unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
           - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1, col='white')
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
    b<-ifelse(i<=4,6,13)
    axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
         labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
         col.tick = "white", col.axis="white", cex.axis=2)
    axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
    box(col = 'white')
     }
 rect(unique(deltapHMeans.time $DateTime)[a]+3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4, alpha=0.7), border = NA)
 
    legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
           text.col='white')
    
    
 # }
  dev.off()
  
  ### Calcification  figure-------------
  boxcol=c('green','red','purple','yellow','white') 
  png("NetCalcification.png", width=3800, height=4000, res=300)
  par(mfrow=c(3,2))
  par(bg=NA)
  layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,
                  3,3,3,3,3,0,4,4,4,4,4,
                  0,0,0,5,5,5,5,5,0,0,0), 3, 11, byrow = TRUE))
  
  y1<-c(-0.5,0,-1.2,-1.5,0)
  y2<-c(4,15,1.5,3,3)
  for (i in 1:length(sub)){
    x<-barplot(NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]], main=sub[i],ylim=c(y1[i],y2[i]), 
               #ylab=expression(paste("Net NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1})),
               col.lab='white',  cex.main=3, cex.axis=2, cex.lab=3, col.main='white', col='lightblue', border='white')
    errorbars(x,NEC.mean.Net$Mean.AFDW2[NEC.mean.Net$Substrate==sub[i]],0,NEC.mean.Net$SE.AFDW2[NEC.mean.Net$Substrate==sub[i]],
              col='white', lwd=3)
    #axis(1, at=x, labels=c("Ambeint","Medium","High"))
    lines(x,c(0,0,0), col='white')
    axis(1, at=x, labels=c("Ambeint","Medium","High"),col.tick= "white", col.axis="white", cex.axis=2, tick=FALSE)
    axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
    box(col = boxcol[i])
  }
  dev.off()
  
  #Day and night Calcification
  #NEC plots by day
  y2<-c(5,20,5,5,5,10)
  y3<-c(-2,0,-1,-2,0)
  DN<-c('Day','Night')
 # for (j in 1:2){
  j=1
    par(mfrow=c(3,2))
    for (i in 1:length(sub)){
      x<-barplot(NEC.mean.DayNight$Mean.AFDW2[NEC.mean.DayNight$Substrate==sub[i] & NEC.mean.DayNight$DayNight==DN[j]], main=sub[i], ylim=c(y3[i],y2[i]), 
                 #ylab=expression(paste("NEC ",mu,"mol g AFDW"^{-1}," hr"^{-1}))
                  )
      errorbars(x,NEC.mean.DayNight$Mean.AFDW2[NEC.mean.DayNight$Substrate==sub[i]& NEC.mean.DayNight$DayNight==DN[j]],0,NEC.mean.DayNight$SE.AFDW2[NEC.mean.DayNight$Substrate==sub[i]& NEC.mean.DayNight$DayNight==DN[j]])
      axis(1, at=x, labels=c("Ambeint","Medium","High"))
      lines(x,c(0,0,0))
    }  
  #}