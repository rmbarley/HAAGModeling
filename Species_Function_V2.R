## SET FUNCTION FOR SPECIES

Species_Function <- function(body_mass=0, WaterEconomyIndex=0, printinfo=FALSE, changeConstant = FALSE) {
  
  ## 0. PREPPING DATAFRAME FOR OUTPUTS ===========================================  
  # Width = number of variables
  # Length = number of combination of results
  DF_outputs <- matrix(data = 0, nrow = length(body_mass)*length(WaterEconomyIndex), ncol = 15)
  colnames(DF_outputs) <- c("Bodymass", "EnergyExp", "WEI", "TranscutaneousH2OLoss", "WVSkin", "MolesO2Air", "O2FluxLungs",
                            "H2OOral", "H2ONasal", "WVNose", "TotalH2OTurnover", "UrinaryH2OLoss", "WVUrine", "Urea", "d18Oairtakenup")
  DF_outputs <- as.data.frame(DF_outputs)  
  
  #FILLING DATAFRAME WITH ARGUMENTS VALUES
  if(sum(body_mass)==0)
  {
    stop("Enter bodymass value")
  }
  bodymass <- body_mass
  DF_outputs$Bodymass <- body_mass
  
  
  if(sum(WaterEconomyIndex)==0)
  {
    stop("Enter Water Economy Index value")
  }
  
  WEI <- WaterEconomyIndex
  DF_outputs_WEI_temp <- c()
  for(i in 1:length(WEI)){
  DF_outputs_WEI_temp <- c(DF_outputs_WEI_temp, rep(WEI[i], nrow(DF_outputs)/length(WEI)))}
  DF_outputs$WEI <- DF_outputs_WEI_temp
    
  ## 1. CONSTANTS ================================================================
  #oxygen utilization factor
  ocf <- 0.2
  #Z-factor
  Zfactor <- 10.5
  #Urea
  Urea <- 0.2
  message("OCF, Zfactor, d8Oairtakenup, and Urea are standardized constants, 
         but can be modified by user by inputing changeConstant == TRUE")
  
  if(changeConstant == TRUE){
    ocf_temp <- readline("Please enter a SINGLE value for oxygen utilization factor, 0.2 is the default value : ")
    ocf <- as.numeric(ocf_temp)
    Zfactor_temp <- readline("Please enter a SINGLE value for Z-factor, 10.5 is the default value : ")
    Zfactor <- as.numeric(Zfactor_temp)
    Urea_temp <- readline("Please enter a SINGLE value for Urea, 0.2 is the default value : ")
    Urea <- as.numeric(Urea_temp)
  }
  DF_outputs$Urea <- Urea
  
  
  #d18Oairtakenup = 23.2 - Zfactor * (1-oxygen utilization factor)
  d18Oairtakenup <- 23.2 - Zfactor * (1-ocf)
  DF_outputs$d18Oairtakenup <- d18Oairtakenup
    
  # 2. BODY MASS (kg) ============================================================

  #Energy Expenditure = 900 x BodyMass^0.73
  for(i in 1:nrow(DF_outputs)){
  DF_outputs$EnergyExp[i] <- 900 * (DF_outputs$Bodymass[i]^0.73)}

  #TranscutaneousH2OLoss <- 1.44 * bodymass * 0.667
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$TranscutaneousH2OLoss[i] <- 1.44 * (DF_outputs$Bodymass[i]^0.667)}

  #Skin = Transcutaneous Water Loss /2
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$WVSkin[i] <- DF_outputs$TranscutaneousH2OLoss[i]/2}

  #MolesO2Air <- EnergyExp * 0.00216
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$MolesO2Air[i] <- DF_outputs$EnergyExp[i] * 0.00216}
  
  #O2FluxLungs <- (22.4 * MolesO2Air)/(0.2*0.21)
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$O2FluxLungs[i] <- (22.4 * DF_outputs$MolesO2Air[i])/(0.2*0.21)}
  
  #H2O exhaled Orally <- O2FluxLungs * 0.5 * 0.003
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$H2OOral[i] <- (DF_outputs$O2FluxLungs[i] * 0.5 * 0.003)}
  
  #H2O exhaled nasally <- O2FluxLungs * 0.5 * 0.5 * 0.003
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$H2ONasal[i] <- (DF_outputs$O2FluxLungs[i] * 0.5 * 0.5 * 0.003)}
  
  #Nose = Water Exhaled Nasally/2
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$WVNose[i] <- (DF_outputs$H2ONasal[i]/2)}
  
  # 3. WATER ECONOMY INDEX (WEI) ====================================================
  
  #TotalH2OTurnover = WEI * (EnergyExp/18)
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$TotalH2OTurnover[i] <- (DF_outputs$WEI[i] * (DF_outputs$EnergyExp[i]/18))}
  
  #Urinary H2O Loss <- WEI loss as urine (0.25) * TotalH2OTurnover
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$UrinaryH2OLoss[i] <- ((0.25) * DF_outputs$TotalH2OTurnover[i])}
  
  #Urine = Urinary H2O loss /2
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$WVUrine[i] <- (DF_outputs$UrinaryH2OLoss[i]/2)}
  
  return(DF_outputs)
}

OS <- Species_Function(body_mass=c(600, 1000,1200,1400), WaterEconomyIndex=c(0.4, 0.5), printinfo = TRUE, changeConstant = FALSE)
