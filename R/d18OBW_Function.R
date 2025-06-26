### SET d18OBODYWATER FUNCTION
d18OBW_Function <- function(Species = 0, Food = 0, Environment = 0, Inputs = 0, Outputs = 0) {
  # d18Obw <- ((MolesO2Air * d18Oairtakenup + WV * dairH2OSW + dfoodO2SW * dryOinflux + dfoodH2Osw * WaterinFood + DrinkingWater * 0) - (WVCO2 * 38.6 + WVMouth * -8.2 + WVNose * -17 + WVSkin * -18 + WVSweat * 0 + WVUrine * 0 + WVFecal * 0)) / (((WVCO2 + WVMouth + WVNose + WVSkin + WVSweat + WVUrine + WVFecal + Urea)+(DrinkingWater *d18Osw + WV * d18Osw + WaterinFood * d18Osw + dryOinflux * d18Osw))/(WVCO2 + WVMouth + WVNose + WVSkin + WVSweat + WVUrine + WVFecal + 0.2))
  # d18Obw <- (2861.06301 + 4174.77875 - 2667.44642) / 1036.56431 + (681.648289 * -9.9 + 1.82509491 * -9.9 + 363.511953 * -9.9 + 60.8971554 * -9.9) / 1036.56431
  d18Obw <- ((Species$MolesO2Air * Species$d18Oairtakenup + Environment$WV * Environment$dairH2OSW + Environment$dfoodO2SW * Inputs$dryOinflux + Environment$dfoodH2Osw * Inputs$WaterinFood + Inputs$DrinkingWater * 0) - (Outputs$WVCO2 * 38.6 + Outputs$WVMouth * -8.2 + Species$WVNose * -17 + Species$WVSkin * -18 + Outputs$WVSweat * 0 + Species$WVUrine * 0 + Outputs$WVFecal * 0)) / (Outputs$WVCO2 + Outputs$WVMouth + Species$WVNose + Species$WVSkin + Outputs$WVSweat + Species$WVUrine + Outputs$WVFecal + Species$Urea) + (Inputs$DrinkingWater * Environment$d18Osw + Environment$WV * Environment$d18Osw + Inputs$WaterinFood * Environment$d18Osw + Inputs$dryOinflux * Environment$d18Osw) / (Outputs$WVCO2 + Outputs$WVMouth + Species$WVNose + Species$WVSkin + Outputs$WVSweat + Species$WVUrine + Outputs$WVFecal + Species$Urea)

  # d18Ophosphate = d18Obw + 25.9 - 37 / 4.38
  d18Ophos <- d18Obw + 25.9 - 37 / 4.38

  # d18Ocarbonate = d18Ophosphate + 8.5
  d18Ocarb <- d18Ophos + 8.5

  Output_d18OBW_Function <- list(d18Obw = d18Obw, d18Ophos = d18Ophos, d18Ocarb = d18Ocarb)
  return(Output_d18OBW_Function)
}

# d18OBW_Function(Species=OS,Food=OI,Environment=OE,Inputs=OI,Outputs=OO)
