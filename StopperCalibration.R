#######################################################################
### Script to calibrate glass stoppers to different temps during BW ###
### Created by Nyssa Silbiger #########################################
### Created on 12/31/2105 #############################################

### This script regresses weight of glass stoppers in fresh water and ##
### salt water measured at several temperatures with temperature. ######
### The coefficients from these regressions can be used to correct #####
### the bouyant weights of corals measured at different temps. #########
### Glass stopper weights in fresh and salt water were taken at many ###
### temperatures for this analysis by Mike Fox. ########################
########################################################################

# Clear Workspace------------------------------------------------------
rm(list=ls())

# load libraries ----------------------------------------------------
library('plyr')

# load data --------------------------------------------------------
CalData<-read.csv('calibration_curves.csv')
#Data with stopper weights in grams in freshwater, saltwater, and the temp
#that they were measured at

# Analysis---------------------------------------------------------
#Weights were taken 3 times at each temp
#calculate average weight per temp
AvgWeights<- ddply(CalData, c("Temp"), summarise,
                   StopperSalt = mean(StopperSalt),
                   StopperFresh = mean(StopperFresh)
)

#plot relationship between temp and stopper in fresh and salt water
plot(AvgWeights$Temp, AvgWeights$StopperSalt , col = 'red', ylab = 'Salt Weight (g)', xlab = 'Temp C')
par(new=TRUE)
plot(AvgWeights$Temp, AvgWeights$StopperFresh, col = 'blue',xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("'Fresh Weight (g)'",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Salt","Fresh"), bty = "n")

#linear regression between temp and stopper
#Salt
StopperSaltModel <- lm(AvgWeights$StopperSalt~AvgWeights$Temp)
summary(StopperSaltModel) #r2 = 0.96
# temp calibration salt
# y = 21.009843 + 0.005491*x where x is a temp and y is the weight of the stopper in salt water

#Fresh
StopperFreshModel <- lm(AvgWeights$StopperFresh~AvgWeights$Temp)
summary(StopperFreshModel) #r2 = 0.96
# temp calibration fresh
# y = 21.455346 + 0.005372 * x

