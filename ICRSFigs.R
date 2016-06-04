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
