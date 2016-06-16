png("pH_Time.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
for (i in 1:5){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,5,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,6,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
#shaded area for night
rect(unique(deltapHMeans.time $DateTime)[a]+3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top',legend=substrate, col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()

## MAKE FIGURE ONE BY ONE
png("pH_Time1.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
for (i in 1){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
#shaded area for night
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top',legend=substrate[1:i], col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()

#2---
png("pH_Time2.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
for (i in 1:2){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
#shaded area for night
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top',legend=substrate[1:i], col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()

#3---
png("pH_Time3.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
for (i in 1:3){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
#shaded area for night
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top',legend=substrate[1:i], col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()

#4
png("pH_Time4.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
for (i in 1:4){
  par(new = TRUE)
  
  
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], col = cols[i],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2,0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         + yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         unique(deltapHMeans.time$DateTime), y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]]
         - yse[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[1]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
#shaded area for night
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top',legend=substrate[1:i], col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()

##0
png("pH_Time0.png", width=3600, height=2000, res=300)

###PH by time PDF
#pdf("pH_Time.pdf", width=14, height=8)

par(mfrow=c(1,2))
par(mar=c(5,6,4,2)+0.1 )
par(bg='Black')
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE


plot(NA, ylim=c(-0.2,0.3), xaxt="n", xlab="Time", ylab=expression(paste(Delta,"pH")),col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
for (i in 1:1){
par(new = TRUE)
cols <- c('green','red','purple','yellow','white')
#
# for (j in 1:length(Nutlevels)){
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  # a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  #b<-ifelse(i<=4,6,13)
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = 'black',
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.3),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
   
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

#shaded area for night

#legend('top',legend=substrate, col=cols, pch=19, bty = 'n',text.col='white', cex=1.5)


#--------------------------
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
i=4
#for (i in 1:length(sub)){
plot(NA, xaxt='n', xlab="Time",ylim=c(-0.2, 0.2), ylab="", main = substrate[i],col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')

abline(h=0, lty=2, col='white')
par(new = TRUE)
cols <- c('lightblue','orange','hotpink')

for (j in 1:length(Nuts)){
  par(new = TRUE)
  
  plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
       y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
       pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(-0.2, 0.2),
       col.lab='white',  cex.main=3, cex.axis=2, cex.lab=2, col.main='white')
  
  arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
         + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
         - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
         angle=90, code=3, length = 0.1, col='white')
  start<-ifelse(i<=4,c(1),c(8))
  stops<-ifelse(i<=4,c(7),c(14))
  a<-ifelse(i<=4,1,7) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,3,10)
  
  c<-ifelse(i<=4,6,13) #because mixed has different dates than the rest of the substrats
  d<-ifelse(i<=4,7,14)
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], 
       labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"),
       col.tick = "white", col.axis="white", cex.axis=2)
  axis(2,  col.tick = "white", col.axis="white", cex.axis=2)
  box(col = 'white')
}
rect(unique(deltapHMeans.time $DateTime)[a]-3600,min(y),unique(deltapHMeans.time $DateTime)[b]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)
rect(unique(deltapHMeans.time $DateTime)[c]+3600,min(y),unique(deltapHMeans.time $DateTime)[d]+3600,max(y)+.5,col = rgb(0.5,0.5,0.5,1/4), border = NA)

legend('top', legend=unique(deltapHMeans.time $NutLevel),  col = c('lightblue','orange','hotpink'), pch=19, bty = 'n',
       text.col='white', cex=1.5)


# }
dev.off()