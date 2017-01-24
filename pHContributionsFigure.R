#Delta pH across time
par(mfrow=c(5,3))
rm(y)
rm(yse)
y<-deltapHMeans.time$pHMean
yse<-deltapHMeans.time$pHSE
for (i in 1:length(sub)){
  plot(NA, xaxt='n', xlab="Time",ylim=c(min(y), max(y)+0.1), ylab=expression(paste(Delta,"pH")), main = sub[i])
  
  abline(h=0)
  par(new = TRUE)
  #cols <- unique(NCP.mean$NutLevel)
  #
  for (j in 1:length(Nuts)){
  
    
    plot(as.numeric(deltapHMeans.time$DateTime [deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]),
         y[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = cols[j],
         pch=19, type="b", xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.1))
    
    arrows(unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
           + yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
           unique(deltapHMeans.time $DateTime), y[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
           - yse[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
           angle=90, code=3, length = 0.1)
  #bar plot for NEC
    start<-ifelse(i<=4,c(1),c(8))
    stops<-ifelse(i<=4,c(7),c(14))
    
  par(new = TRUE)
    
      bp<-barplot(deltapHMeans.time$pH.NEC[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = 'black',
               xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.1), yaxt='n')
      arrows(bp, deltapHMeans.time$pH.NEC[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
             + deltapHMeans.time$pH.NECSE[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             bp, deltapHMeans.time$pH.NEC[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
             - deltapHMeans.time$pH.NECSE[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
             angle=90, code=3, length = 0.1)
      
  #bar plot for NCP
          
     par(new=TRUE)
     bp<-barplot(deltapHMeans.time$pH.NCP[deltapHMeans.time$Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]], col = 'grey',
                            xaxt='n', ylab='', xlab='',ylim=c(min(y), max(y)+.1), yaxt='n')
                 
       arrows(bp, deltapHMeans.time$pH.NCP[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]]
            + deltapHMeans.time$pH.NCPSE[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
            bp, deltapHMeans.time$pH.NCP[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time$NutLevel==Nuts[j]]
            - deltapHMeans.time$pH.NCPSE[deltapHMeans.time $Substrate==sub[i] & deltapHMeans.time $NutLevel==Nuts[j]], 
            angle=90, code=3, length = 0.1)
     
      
 # }
     
  axis(1, at=unique(deltapHMeans.time $DateTime)[start:stops], labels=c('10:00',"14:00","18:00","22:00","02:00","06:00","10:00"))
 
 
  
  #shaded area for night
  a<-ifelse(i<=4,3,10) #because mixed has different dates than the rest of the substrats
  b<-ifelse(i<=4,6,13)
  rect(unique(deltapHMeans.time $DateTime)[a],min(y),unique(deltapHMeans.time $DateTime)[b],max(y)+.1,col = rgb(0.5,0.5,0.5,1/4), border = NA)
  }
}
#legend('topright', legend=unique(deltapHMeans.time $NutLevel), col=unique(deltapHMeans.time $NutLevel), pch=19, bty = 'n')
