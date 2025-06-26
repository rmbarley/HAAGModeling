
#' Calculate Oxygen Isotope Ratios in Body Water
#'
#' This function calculates the oxygen isotope composition (δ18O) of body water,
#' phosphate, and carbonate based on species physiology, food intake, environmental
#' conditions, and water balance.
#'
#' @param Species Output from Species_Function containing physiological parameters
#'   including oxygen consumption, water turnover rates, and respiratory water losses.
#' @param Food Output from Food_Function containing food composition and
#'   digestibility parameters.
#' @param Environment Output from Environment_Function containing environmental
#'   isotope information and water vapor data.
#' @param Inputs Output from Input_Function containing calculated input fluxes
#'   of water and nutrients.
#' @param Outputs Output from Output_Function containing calculated output
#'   fluxes of water through various physiological processes.
#'
#' @return The function will return the following list:
#' \describe{
#'   \item{d18Obw}{δ18O value of body water}
#'   \item{d18Ophos}{δ18O value of phosphate}
#'   \item{d18Ocarb}{δ18O value of carbonate}
#' }
#'

###SET d18OBODYWATER FUNCTION
d18OBW_Function <- function(Outputs=0)
{

  ## 0. PREPPING DATAFRAME FOR OUTPUTS ===========================================
  # Width = number of variables from  OUTPUTS dataframe + the three d180 computed here
  # Length = number of rows from OUTPUTS dataframe
  DF_outputs <- Outputs
  DF_outputs_temp <- matrix(data = 0, nrow = nrow(Outputs), ncol = 3)
  colnames(DF_outputs_temp) <- c("d18Obw", "d18Ophos", "d18Ocarb")
  DF_outputs_temp <- as.data.frame(DF_outputs_temp)
  DF_outputs <- cbind(DF_outputs, DF_outputs_temp)


#d18Obw <- ((MolesO2Air * d18Oairtakenup + WV * dairH2OSW + dfoodO2SW * dryOinflux + dfoodH2Osw * WaterinFood + DrinkingWater * 0) - (WVCO2 * 38.6 + WVMouth * -8.2 + WVNose * -17 + WVSkin * -18 + WVSweat * 0 + WVUrine * 0 + WVFecal * 0)) / (((WVCO2 + WVMouth + WVNose + WVSkin + WVSweat + WVUrine + WVFecal + Urea)+(DrinkingWater *d18Osw + WV * d18Osw + WaterinFood * d18Osw + dryOinflux * d18Osw))/(WVCO2 + WVMouth + WVNose + WVSkin + WVSweat + WVUrine + WVFecal + 0.2))
for(i in 1:nrow(DF_outputs)){
DF_outputs$d18Obw[i] <- ((DF_outputs$MolesO2Air[i] * DF_outputs$d18Oairtakenup[i] + DF_outputs$WV[i] * DF_outputs$dairH2OSW[i] + DF_outputs$dfoodO2SW[i] * DF_outputs$dryOinflux[i] + DF_outputs$dfoodH2Osw[i] * DF_outputs$WaterinFood[i] + DF_outputs$DrinkingWater[i] * 0) - (DF_outputs$WVCO2[i] * 38.6 + DF_outputs$WVMouth[i] * -8.2 + DF_outputs$WVNose[i] * -17 + DF_outputs$WVSkin[i] * -18 + DF_outputs$WVSweat[i] * 0 + DF_outputs$WVUrine[i] * 0 + DF_outputs$WVFecal[i] * 0)) / (DF_outputs$WVCO2[i] + DF_outputs$WVMouth[i] + DF_outputs$WVNose[i] + DF_outputs$WVSkin[i] + DF_outputs$WVSweat[i] + DF_outputs$WVUrine[i] + DF_outputs$WVFecal[i] + DF_outputs$Urea[i]) + (DF_outputs$DrinkingWater[i] *  DF_outputs$d18Osw[i] + DF_outputs$WV[i] *  DF_outputs$d18Osw[i] + DF_outputs$WaterinFood[i] *  DF_outputs$d18Osw[i] + DF_outputs$dryOinflux[i] *  DF_outputs$d18Osw[i]) / (DF_outputs$WVCO2[i] + DF_outputs$WVMouth[i] + DF_outputs$WVNose[i] + DF_outputs$WVSkin[i] + DF_outputs$WVSweat[i] + DF_outputs$WVUrine[i] + DF_outputs$WVFecal[i] + DF_outputs$Urea[i])

#d18Ophosphate = d18Obw + 25.9 - 37 / 4.38
DF_outputs$d18Ophos[i] <- DF_outputs$d18Obw[i] + 25.9 - 37 / 4.38

#d18Ocarbonate = d18Ophosphate + 8.5
DF_outputs$d18Ocarb[i] <- DF_outputs$d18Ophos[i] + 8.5

}

message("d18O body water, d180 phosphate, d180 carbonate : ")
print(DF_outputs[,c("d18Obw", "d18Ophos", "d18Ocarb")])

return(DF_outputs)
}

#d18OBW_Function(Species=OS,Food=OI,Environment=OE,Inputs=OI,Outputs=OO)
