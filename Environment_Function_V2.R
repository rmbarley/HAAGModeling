###SET FUNCTION FOR ENVIRONMENT

Environment_Function <- function(air_temperature=0,Relative_Humidity=0, d18O_surfacewater=0) {
  
  ## 0. PREPPING DATAFRAME FOR OUTPUTS ===========================================  
  # Width = number of variables
  # Length = number of combination of results
  DF_outputs <- matrix(data = 0, nrow = length(air_temperature)*length(Relative_Humidity)*length(d18O_surfacewater), ncol = 12)
  colnames(DF_outputs) <- c("airtemp", "MAT", "Humidity", "WVinLungs", "WV", "d18Osw", "dairH2O",
                            "dairH2OSW", "d18OleafH2O", "d18Oleafcellulose", "dfoodO2SW", "dfoodH2Osw")
  DF_outputs <- as.data.frame(DF_outputs)   
  
  #FILLING DATAFRAME WITH ARGUMENTS VALUES
  
  # air temperature (C)
  if(sum(air_temperature)==0)
  {
    stop("Enter Air Temperature value")
  }
  airtemp <- air_temperature
  DF_outputs$airtemp <- airtemp
  
  # mean annual temperature (K)OE
  MAT <- airtemp +273
  DF_outputs$MAT <- MAT
  
  #Humidity
  if(sum(Relative_Humidity)==0)
  {
    stop("Enter Relative Humidity value")
  }
  Humidity <- Relative_Humidity
  DF_outputs_Humidity_temp <- c()
  for(i in 1:length(Humidity)){
    DF_outputs_Humidity_temp <- c(DF_outputs_Humidity_temp, rep(Humidity[i], nrow(DF_outputs)/length(Humidity)))} ## the dataframe is split in X part for X values of humidity
  DF_outputs$Humidity <- DF_outputs_Humidity_temp
  
  # d18Osurfacewater
  if(sum(d18O_surfacewater)==0)
  {
    stop("Enter d18Osw value")
  }
  d18Osw <- d18O_surfacewater
  DF_outputs_d18Osw_temp <- c()
  for(i in 1:length(d18Osw)){
    DF_outputs_d18Osw_temp <- c(DF_outputs_d18Osw_temp, rep(d18Osw[i], nrow(DF_outputs)/length(Humidity)/length(d18Osw)))}
  DF_outputs_d18Osw_temp <- rep(DF_outputs_d18Osw_temp, length(Humidity))
  DF_outputs$d18Osw <- DF_outputs_d18Osw_temp
  
  
  ### 1. CALCULATION WITH HUMIDITY AND AIR TEMPERATURE =========================
  
  #Water Vapor Taken in Lungs <- Humidity * 10^(0.686+0.027*airtemp) * 12400/(760*22.4)
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$WVinLungs[i] <- DF_outputs$Humidity[i] * (10^(0.686+0.027*DF_outputs$airtemp[i])) * 12400/(760*22.4)}
  
  #Water Vapor <- Water Vapor in Lungs / 2
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$WV[i] <- DF_outputs$WVinLungs[i]/2}
  
  ### 2. ADDING D18 SURFACE WATER TO CALCULATION ===============================
  
  #dairH2O <- d18Osw - 2.644 + 3206/MAT - 1.534 * 10^6 / MAT^2
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$dairH2O[i] <- DF_outputs$d18Osw[i] - 2.644 + 3206/DF_outputs$MAT[i] - 1.534 * (10^6) / (DF_outputs$MAT[i]^2)}
  
  #dairH2OSW <- dairH2O -d18Osw
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$dairH2OSW[i] <- DF_outputs$dairH2O[i] - DF_outputs$d18Osw[i]}
  
  #d18OleafH2O <-d18Osw + (1-Humidity) * (d18Osw - dairH2O + 16)
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$d18OleafH2O[i] <- DF_outputs$d18Osw[i] + (1-DF_outputs$Humidity[i]) * (DF_outputs$d18Osw[i] - DF_outputs$dairH2O[i] + 16)}
  
  #d18Oleafcellulose <- d18OleafH2O + 27
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$d18Oleafcellulose[i] <- DF_outputs$d18OleafH2O[i] + 27}
  
  #dfoodO2SW <-d18Oleafcellulose - d18Osw
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$dfoodO2SW[i] <- DF_outputs$d18Oleafcellulose[i] -  DF_outputs$d18Osw[i]}
  
  #dfoodH2Osw  <- (0.5 * d18OstemH2O + 0.5 * d18OleafH2O) -d18Osw
  #d18OstemH2O = d18Osw
  for(i in 1:nrow(DF_outputs)){
    DF_outputs$dfoodH2Osw[i] <- (0.5 *  DF_outputs$d18Osw[i] + 0.5 * DF_outputs$d18OleafH2O[i]) - DF_outputs$d18Osw[i]}
  
  return(DF_outputs)
}

OE <- Environment_Function(air_temperature=c(10, 20),Relative_Humidity=c(0.5,0.75), d18O_surfacewater=c(-2, -5))
