#write function ex2exp to find expected value of experiment 2 basd on expt 1
ex2exp <- function(process,norms,normflow=FALSE) {
  #process = NEC or NCP; net community calcification or net community production 
  #norms = AFDW, DW, Vol, SA; normalization factor for process
  #normflow; TRUE if normalized to flow rates bt Ex1 and Ex2; FALSE if flow rates are ignored
 # process="NEC"
#  norms="AFDW"
  nm <- paste0(process,".",norms)
  process.coral <- NA
  process.algae <- NA
  process.rubble <- NA
  process.sand <- NA
  for (i in c(1:nrow(AllData_Ex2))){
    process.coral[i] <- AllData_Ex1[((AllData_Ex1$Aquarium==AllData_Ex2$Aqua.coral[i])
                                     & (AllData_Ex1$period == AllData_Ex2$period[i])),nm]
    process.algae[i] <- AllData_Ex1[((AllData_Ex1$Aquarium==AllData_Ex2$Aqua.algae[i])
                                     & (AllData_Ex1$period == AllData_Ex2$period[i])),nm]
    process.rubble[i] <- AllData_Ex1[((AllData_Ex1$Aquarium==AllData_Ex2$Aqua.rubble[i])
                                      & (AllData_Ex1$period == AllData_Ex2$period[i])),nm]
    process.sand[i] <- AllData_Ex1[((AllData_Ex1$Aquarium==AllData_Ex2$Aqua.sand[i])
                                    & (AllData_Ex1$period == AllData_Ex2$period[i])),nm]
  }
  ftot <- AllData_Ex2$ResTime.mean.coral + AllData_Ex2$ResTime.mean.algae + 
    AllData_Ex2$ResTime.mean.rubble + AllData_Ex2$ResTime.mean.sand
  favg <- ftot/4
  
  if (!normflow) {
    pred <- (process.coral*AllData_Ex2$AFDW.coral/AllData_Ex2$AFDW.coral.tot +
            process.algae*AllData_Ex2$AFDW.algae/AllData_Ex2$AFDW.algae.tot + 
            process.rubble*AllData_Ex2$AFDW.rubble/AllData_Ex2$AFDW.rubble.tot +
            process.sand*AllData_Ex2$AFDW.sand/AllData_Ex2$AFDW.sand.tot)
  } else {
    pred <- (process.coral*(AllData_Ex2$AFDW.coral/AllData_Ex2$AFDW.coral.tot)
             *(AllData_Ex2$ResTime.mean.coral/ftot)
             + process.algae*(AllData_Ex2$AFDW.algae/AllData_Ex2$AFDW.algae.tot)
             *(AllData_Ex2$ResTime.mean.coral/ftot)
             + process.rubble*(AllData_Ex2$AFDW.rubble/AllData_Ex2$AFDW.rubble.tot)
             *(AllData_Ex2$ResTime.mean.coral/ftot)
             + process.sand*(AllData_Ex2$AFDW.sand/AllData_Ex2$AFDW.sand.tot)
             *(AllData_Ex2$ResTime.mean.coral/ftot))*AllData_Ex2$ResTime.mean/favg
  }
  return(pred)
}