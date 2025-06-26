### SET FUNCTION FOR ENVIRONMENT
Environment_Function <- function(air_temperature = 0, Relative_Humidity = 0, d18O_surfacewater = 0,
                                 printinfo = FALSE) {
  # air temperature (C)
  if (sum(air_temperature) == 0) {
    stop("Enter Air Temperature value")
  }
  airtemp <- air_temperature

  # mean annual temperature (K)
  MAT <- airtemp + 273

  # Humidity
  if (sum(Relative_Humidity) == 0) {
    stop("Enter Relative Humidity value")
  }
  Humidity <- Relative_Humidity


  # Water Vapor Taken in Lungs <- Humidity * 10^(0.686+0.027*airtemp) * 12400/(760*22.4)
  WVinLungs <- Humidity * (10^(0.686 + 0.027 * airtemp)) * 12400 / (760 * 22.4)
  if (printinfo == TRUE) {
    print(paste("Water Vapor in Lungs ", round(WVinLungs)))
  }

  # Water Vapor <- Water Vapor in Lungs / 2
  WV <- WVinLungs / 2
  if (printinfo == TRUE) {
    print(paste("Water Vapor ", round(WV)))
  }


  # d18Osurfacewater
  if (sum(d18O_surfacewater) == 0) {
    stop("Enter d18Osw value")
  }
  d18Osw <- d18O_surfacewater

  # dairH2O <- d18Osw - 2.644 + 3206/MAT - 1.534 * 10^6 / MAT^2
  dairH2O <- d18Osw - 2.644 + 3206 / MAT - 1.534 * (10^6) / (MAT^2)
  if (printinfo == TRUE) {
    print(paste("d18Oairwater ", round(dairH2O)))
  }

  # dairH2OSW <- dairH2O -d18Osw
  dairH2OSW <- dairH2O - d18Osw
  if (printinfo == TRUE) {
    print(paste("d18Oairwater-d18Osw ", round(dairH2OSW)))
  }

  # d18OleafH2O <-d18Osw + (1-Humidity) * (d18Osw - dairH2O + 16)
  d18OleafH2O <- d18Osw + (1 - Humidity) * (d18Osw - dairH2O + 16)
  if (printinfo == TRUE) {
    print(paste("d18Oleafwater ", round(d18OleafH2O)))
  }

  # d18Oleafcellulose <- d18OleafH2O + 27
  d18Oleafcellulose <- d18OleafH2O + 27
  if (printinfo == TRUE) {
    print(paste("d18Oleafcelluslose ", round(d18Oleafcellulose)))
  }

  # dfoodO2SW <-d18Oleafcellulose - d18Osw
  dfoodO2SW <- d18Oleafcellulose - d18Osw
  if (printinfo == TRUE) {
    print(paste("d18OFoodO2 - d18Osw ", round(dfoodO2SW)))
  }

  # dfoodH2Osw  <- (0.5 * d18OstemH2O + 0.5 * d18OleafH2O) -d18Osw
  # d18OstemH2O = d18Osw
  dfoodH2Osw <- (0.5 * d18Osw + 0.5 * d18OleafH2O) - d18Osw
  if (printinfo == TRUE) {
    print(paste("d18Ofoodwater - d18Osw ", round(dfoodH2Osw)))
  }

  ## Outputs of Environment_Function
  Output_Environment_Function <- list(
    airtemp = airtemp, MAT = MAT, Humidity = Humidity,
    WVinLungs = WVinLungs, WV = WV, d18Osw = d18Osw,
    dairH2O = dairH2O, dairH2OSW = dairH2OSW, d18OleafH2O = d18OleafH2O,
    d18Oleafcellulose = d18Oleafcellulose, dfoodO2SW = dfoodO2SW,
    dfoodH2Osw = dfoodH2Osw
  )
  return(Output_Environment_Function)
}

OE <- Environment_Function(air_temperature = 10, Relative_Humidity = 0.5, d18O_surfacewater = -2)
Environment <- OE
