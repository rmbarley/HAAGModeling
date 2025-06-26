## SET FUNCTION FOR SPECIES
Species_Function <- function(body_mass = 0, WaterEconomyIndex = 0, printinfo = FALSE) {
  ## CONSTANTS
  # oxygen utilization factor
  ocf <- 0.2
  # Z-factor
  Zfactor <- 10.5
  # d18Oairtakenup = 23.2 - Zfactor * (1-oxygen utilization factor)
  d18Oairtakenup <- 23.2 - Zfactor * (1 - ocf)

  # Urea
  Urea <- 0.2
  message(" OCF, Zfactor, d8Oairtakenup, and Urea are standardized constants,
         but can be modified within code by user")

  # body mass (kg)
  if (sum(body_mass) == 0) {
    stop("Enter bodymass value")
  }
  bodymass <- body_mass

  # Energy Expenditure = 900 x BodyMass^0.73
  message("To view output for individual variables, set printinfo = TRUE ")
  EnergyExp <- 900 * (bodymass^0.73)
  if (printinfo == TRUE) {
    print(paste("Energy_Expenditure ", round(EnergyExp)))
  }

  # TranscutaneousH2OLoss <- 1.44 * bodymass * 0.667
  TranscutaneousH2OLoss <- 1.44 * (bodymass^0.667)
  if (printinfo == TRUE) {
    print(paste("Transcutaneous Water Loss ", round(TranscutaneousH2OLoss)))
  }

  # Skin = Transcutaneous Water Loss /2
  WVSkin <- TranscutaneousH2OLoss / 2
  if (printinfo == TRUE) {
    print(paste("Water Vapor from Skin ", round(WVSkin)))
  }

  # MolesO2Air <- EnergyExp * 0.00216
  MolesO2Air <- EnergyExp * 0.00216
  if (printinfo == TRUE) {
    print(paste("Moles of O2 Respired from Air ", round(MolesO2Air)))
  }

  # O2FluxLungs <- (22.4 * MolesO2Air)/(0.2*0.21)
  O2FluxLungs <- (22.4 * MolesO2Air) / (0.2 * 0.21)
  if (printinfo == TRUE) {
    print(paste("O2 Fluxed Through Lungs ", round(O2FluxLungs)))
  }

  # H2O exhaled Orally <- O2FluxLungs * 0.5 * 0.003
  H2OOral <- O2FluxLungs * 0.5 * 0.003
  if (printinfo == TRUE) {
    print(paste("Water Exhaled Orally ", round(H2OOral)))
  }

  # H2O exhaled nasally <- O2FluxLungs * 0.5 * 0.5 * 0.003
  H2ONasal <- O2FluxLungs * 0.5 * 0.5 * 0.003
  if (printinfo == TRUE) {
    print(paste("Water Exhally Nasally ", round(H2ONasal)))
  }

  # Nose = Water Exhaled Nasally/2
  WVNose <- H2ONasal / 2
  if (printinfo == TRUE) {
    print(paste("Water Vapor Output from Nose ", round(WVNose)))
  }

  # water economy index (WEI)
  if (sum(WaterEconomyIndex) == 0) {
    stop("Enter Water Economy Index value")
  }

  WEI <- WaterEconomyIndex

  # TotalH2OTurnover = WEI * (EnergyExp/18)
  TotalH2OTurnover <- WEI * (EnergyExp / 18)
  if (printinfo == TRUE) {
    print(paste("Total Water Loss ", round(TotalH2OTurnover)))
  }

  # Urinary H2O Loss <- WEI loss as urine (0.25) * TotalH2OTurnover
  UrinaryH2OLoss <- (0.25) * TotalH2OTurnover
  if (printinfo == TRUE) {
    print(paste("Urinary Water Loss ", round(UrinaryH2OLoss)))
  }

  # Urine = Urinary H2O loss /2
  WVUrine <- UrinaryH2OLoss / 2
  if (printinfo == TRUE) {
    print(paste("Water Vapor Output from Urine ", round(WVUrine)))
  }

  ## Outputs of Species_Function
  Output_Species_Function <- list(
    d18Oairtakenup = d18Oairtakenup, bodymass = bodymass, EnergyExp = EnergyExp, WEI = WEI,
    TranscutaneousH2OLoss = TranscutaneousH2OLoss, WVSkin = WVSkin,
    MolesO2Air = MolesO2Air, O2FluxLungs = O2FluxLungs, H2OOral = H2OOral,
    H2ONasal = H2ONasal, WVNose = WVNose, TotalH2OTurnover = TotalH2OTurnover,
    UrinaryH2OLoss = UrinaryH2OLoss, WVUrine = WVUrine, Urea = Urea
  )
  return(Output_Species_Function)
}

OS <- Species_Function(body_mass = 600, WaterEconomyIndex = 0.4)
Species <- OS
