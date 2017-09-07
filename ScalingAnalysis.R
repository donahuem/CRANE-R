### CRANE Scaling Analysis
### Started Sept 5, 2017
### Testing whether the NEC and NEP rates are additive between single substrate and mixed substrates


source("CRANEMetabolismAnalysis.R")

#Add columns to substrate data frames that calculate each sample's proportion of the total (AFDW, DW, Vol, SA) per aquarium

propAqua <- function(datfram,x,Aqua) {
  #sum x within each aquarium
  a <- aggregate(datfram[x],datfram[Aqua],sum)
  #sum x within each SampleID (necessary for coral bc two species) and bring along Aq_Ex1 identification
  b <- cbind(aggregate(datfram[x],datfram["SampleID"],sum),aggregate(datfram[Aqua],datfram["SampleID"],mean))
  #calculate sample x as a proportion of aquarium x for each sample
  y <- cbind(b[Aqua],b["SampleID"],b[x]/a[[x]][match(b[[Aqua]],a[[Aqua]])])
  return(y)
}

#datframs = list(Coral=Coral,Algae=Algae,Rubble=Rubble,Sand=Sand)
datframs = c("Coral","Algae","Rubble","Sand")  #list of data frames to loop over
varlist = c("AFDW","DW","Volume","SA")         #list of variables in each dataframe that function propAqua will be used on
algvarlist = c("AFDW","DW","FinalVol","FinalSA")  #custom list of vars for algae
sandvarlist = c("AFDW","DW","Vol","SA")           #custom list of vars for sand
newvarlist = c("propAFDW","propDW","propVol","propSA")  #list of names for newly calculated variables

#loop over dataframes and add a newly calculated variable
#create new dataframe of weights: ScalDat
ScalDat <- data.frame()
for (i in c(1:length(datframs))){
  #get correct variable list for this dataframe
  vr = varlist
  if (datframs[i]=="Algae") vr=algvarlist
  if (datframs[i]=="Sand") vr=sandvarlist
  #loop over variables in the data frame
  for (j in c(1:length(vr))){
    a <- propAqua(get(datframs[i]),vr[j],"Aq_Ex1")  #call propAqua to calculate the proportion of vr[j] that each sample comprises in its aquarium from Expt 1
    colnames(a) <- c("Aquarium","SampleID",newvarlist[j])   #rename the returned variable from the newvarname list
    #create a temporary matrix with the proportions based on 4 measures for each substrate
    if (j==1) {
      aa <- cbind(matrix(datframs[i],nrow = nrow(a),ncol=1,dimnames=list(NULL,"Substrate")),a)
      }
    else aa <- cbind(aa,a[newvarlist[j]])
    #assign(datframs[i],cbind(get(datframs[i]),a))   #use assign to create a new dataframe with the same name and assign it the value of the old dataframe (using get) cbind'ed with the new variable
  }
  if (i==1) ScalDat <- aa else ScalDat <- rbind(ScalDat,aa)  #concatenate the rows together into a single dataframe
}

DataEx1 <- AllData[AllData$Experiment==1,]
nr <- nrow(ScalDat)
ScalDat <- ScalDat[rep(1:nrow(ScalDat),times=length(unique(DataEx1$DateTime))),]
ScalDat$DateTime <- rep(unique(DataEx1$DateTime),each=nr)

ScalDat <- join(ScalDat,DataEx1,by=c("Aquarium","DateTime","Substrate"),type="left")
rvs <- c("NEC", "NCP")
varlist <- sandvarlist
for (j in c(1:length(rvs))) {
  for (i in c(1:length(varlist))){
    nm <- paste0(rvs[j],".", varlist[i])
    nm2 <- paste0(nm,".wtd")
    w <- ScalDat[newvarlist[i]]*ScalDat[nm]
    colnames(w)<- nm2
    if (i==1 && j==1) wtdvars <- w else wtdvars <- cbind(wtdvars,w)
  }
}
ScalDat <- cbind(ScalDat,wtdvars)  #add AFDW,DW,Vol, and SA-wtd NEC and NEP to ScalDat based on Expt 1

#Now, add calculations for predicted NCP and NEC to Expt2 data to compare
DataEx2 <- AllData[AllData$Experiment==2,]
##Next Steps:
#1 - add Aq_Ex2 to ScalDat (i.e., associate each sample with its Expt 2 aquarium)
#2 - summarize ScalDat by Aq_Ex2 to get ScalDat_Aqua
#3 - pull ScalDat_Aqua data into DataEx2
#4 - compare NEC and NEP pred and obs for DataEx2



