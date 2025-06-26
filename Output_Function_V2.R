###SET OUTPUT FUNCTION

Outputs_Function <- function(Inputs= 0, SweatingSpecies = FALSE)
{
  message("Default is Water Vapor Loss of Sweat = 0; if species sweat, change arguments SweatingSpecies to TRUE")
  
  ## 0. PREPPING DATAFRAME FOR OUTPUTS ===========================================  
  # Width = number of variables from inputs dataframe + OUTPUTS Variables computed in this function
  # Length = number of rows from Inputs dataframe
  DF_outputs <- Inputs
  DF_outputs_temp <- matrix(data = 0, nrow = nrow(Inputs), ncol = 9)
  colnames(DF_outputs_temp) <- c("DryFecalOutput", "FecalH20Loss", "WVFecal",
                                 "WaterHeatLoss", "WVSweat", "Panting", 
                                 "WVMouth", "UreaProduced", "WVCO2")
  DF_outputs_temp <- as.data.frame(DF_outputs_temp)
  DF_outputs <- cbind(DF_outputs, DF_outputs_temp)
  
  ## 1. COMPUTING OUTPUTS VARIABLES
  
  #Dry Fecal Output = FoodMassIngested * (1-Digestibility)
  for(i in 1:nrow(DF_outputs)){
  DF_outputs$DryFecalOutput[i] = DF_outputs$FoodMassIngested[i] * (1-DF_outputs$Digestibility[i])

  #Fecal H20 Loss = DryFecalOutput * 55.56 * 0.6/(1-0.6)
  DF_outputs$FecalH20Loss[i] <- DF_outputs$DryFecalOutput[i] * 55.56 * 0.6/(1-0.6)

  #Fecal = Fecal H2O Loss /2
  DF_outputs$WVFecal[i] <- DF_outputs$FecalH20Loss[i]/2

  #Water Used for Heat Loss <- TotalH2OTurnover - H2OOral - H2ONasal - TrancutaneousH2OLoss - UrinaryH2OLoss - FecalH20Loss
  DF_outputs$WaterHeatLoss[i] <- DF_outputs$TotalH2OTurnover[i] - DF_outputs$H2OOral[i] - DF_outputs$H2ONasal[i] - DF_outputs$TranscutaneousH2OLoss[i] - DF_outputs$UrinaryH2OLoss[i] - DF_outputs$FecalH20Loss[i]

  #Sweat = Sweating/2 = (0.75xWater Heat Loss)/2
  Sweating = 0.75 * DF_outputs$WaterHeatLoss[i]
  DF_outputs$WVSweat[i] <- 0
  if(SweatingSpecies == TRUE){DF_outputs$WVSweat[i] <- Sweating
  message(paste("Default is Water Vapor Loss of Sweat = 0; because species sweat, its changed to 0.75 * WaterHeatLoss : ", Sweating))}
  
  #Panting <- 0.5 * WaterHeatLoss
  DF_outputs$Panting[i] <- 0.5 * DF_outputs$WaterHeatLoss[i]

  #Mouth = Water Exhaled Orally/2 + Panting/2 
  DF_outputs$WVMouth[i] <- DF_outputs$H2OOral[i]/2 + DF_outputs$Panting[i]/2

  DF_outputs$UreaProduced[i] = DF_outputs$Oprotein[i] * DF_outputs$foodproteincontent[i] * DF_outputs$EEE[i] * DF_outputs$Digestibility[i] * DF_outputs$FoodMassIngested[i]

  #CO2 = moles O2 air - Urea Produced - (Dry H influx/2 - Dry O Influx)
  DF_outputs$WVCO2[i] <- DF_outputs$MolesO2Air[i] - DF_outputs$UreaProduced[i] - (DF_outputs$dryHinflux[i]/2 - DF_outputs$dryOinflux[i])
  }
  
  return(DF_outputs)
}
