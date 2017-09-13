#######################################################################
### Function to calculate dry weight of corals from buoyant weights ###
### Created by Nyssa Silbiger #########################################
### Created on 12/31/2105 #############################################

### This function uses calculations from Davies (1989) Marine Biology ##
### to calculate dry weight of corals by correcting for the density ####
### of seawater using a known standard (glass stopper). ################
### With this calculation the temperature needs to be constant. Here,###
### we use a calibration curve to account for changes in temperature. ##
### Glass stopper weights in fresh and salt water were taken at many ###
### temperatures for this analysis by Mike Fox. ########################
########################################################################

BWCalc <- function(StopperAir= 39.109, Temp, BW, CoralDensity = 2.93){
  #Parameters---------------------------------------------------------------
  # StopperAir is the weight of the stopper in air (Default set at 39.109 grams)
  # Temp is the temp of the water during the coral measurement
  # BW is the buoywant weight of the coral
  # CoralDensity <- 2.93 # This is the density of aragonite and it is an assumed value in
  # this calculation based on Jokiel 1978.
  # Hollie tested this assumption with several different densities and it made no difference.
  # But you can change the density to lit values for the specific coral species of interest in the function. 
  # 2.93 is the default
  
  #Calculation------------------------------------------------------------
  #Step 1: correct the stopper weights for temperature
  # StopperFresh is the weight of the stopper in  fresh water at temp x
  # StopperSalt is the weight of the stopper in salt water at temp x
  
  # This is based on a calibration curve for stoppers weighed in fresh and salt water at many temps
  StopperFresh <- 21.455346 + 0.005372 *Temp
  StopperSalt <- 21.009843 + 0.005491*Temp
  
  # Step 2: Use weight in air and freshwater of the glass stopper to calculate
  # the density of the stopper (Davies eq. 4)
  FreshDensity <- 1 #Fresh water has a density of 1 g/cm3
  StopperDensity <- FreshDensity/(1-(StopperFresh/StopperAir)) 
  
  # Step 3: Calculate the density of seawater using the density of the stopper
  # (Davies eq. 3)
  SWDensity <- StopperDensity*(1-(StopperSalt/StopperAir))
  
  # Step 4: Calculate the dry weight of the coral (Davies eq. 1)
  CoralWeight <- BW/(1-(SWDensity/CoralDensity))
  
  return(CoralWeight) #returns coral dry weights in g
}
