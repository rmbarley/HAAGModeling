#' Calculate Outputs for Animal
#'
#' This function computes various physiological outputs related to water balance,
#' heat regulation, and metabolic processes in animals. It calculates fecal
#' outputs, water vapor losses through different routes, and metabolic
#' byproducts based on input parameters describing food intake, digestibility,
#' and water turnover.
#'
#' @param inputs A data frame containing the following required columns:
#'   \describe{
#'     \item{FoodMassIngested}{Numeric. Mass of food consumed (grams)}
#'     \item{Digestibility}{Numeric. Fraction of food digested (0-1)}
#'     \item{TotalH2OTurnover}{Numeric. Total daily water turnover (ml)}
#'     \item{H2OOral}{Numeric. Oral water loss (ml)}
#'     \item{H2ONasal}{Numeric. Nasal water loss (ml)}
#'     \item{TranscutaneousH2OLoss}{Numeric. Transcutaneous water loss (ml)}
#'     \item{UrinaryH2OLoss}{Numeric. Urinary water loss (ml)}
#'     \item{Oprotein}{Numeric. Oxygen consumption for protein metabolism}
#'     \item{foodproteincontent}{Numeric. Protein content of food (fraction)}
#'     \item{EEE}{Numeric. Energy extraction efficiency (0-1)}
#'     \item{MolesO2Air}{Numeric. Moles of oxygen from air}
#'     \item{dryHinflux}{Numeric. Dry hydrogen influx}
#'     \item{dryOinflux}{Numeric. Dry oxygen influx}
#'   }
#' @param sweating_species Logical. Indicates whether the species is capable of
#'   sweating. If TRUE, water vapor loss from sweating is calculated as 75% of
#'   water heat loss. If FALSE (default), sweating water vapor loss is set to
#'   zero.
#'
#' @return A data frame containing all original input columns plus the following
#'   calculated output columns:
#'   \describe{
#'     \item{DryFecalOutput}{Numeric. Dry mass of fecal output (grams)}
#'     \item{FecalH20Loss}{Numeric. Water loss through feces (ml)}
#'     \item{WVFecal}{Numeric. Water vapor from fecal loss (ml)}
#'     \item{WaterHeatLoss}{Numeric. Water used for thermoregulation (ml)}
#'     \item{WVSweat}{Numeric. Water vapor from sweating (ml, species-dependent)}
#'     \item{Panting}{Numeric. Water loss through panting (ml)}
#'     \item{WVMouth}{Numeric. Water vapor from oral routes (ml)}
#'     \item{UreaProduced}{Numeric. Urea production from protein metabolism}
#'     \item{WVCO2}{Numeric. Water vapor associated with CO2 exchange}
#'   }
#'
#' @details
#' The function performs calculations in a specific sequence to handle dependencies:
#' \enumerate{
#'   \item Calculates dry fecal output based on food intake and digestibility
#'   \item Computes fecal water loss using standard conversion factors
#'   \item Determines water available for heat loss by subtracting all other losses
#'   \item Calculates sweating-related outputs (species-dependent)
#'   \item Computes respiratory water losses
#'   \item Determines metabolic outputs including urea production
#' }
#'
#' @note
#' All water-related inputs and outputs should use consistent units (typically ml).
#' Digestibility and efficiency values should be between 0 and 1.
#' For non-sweating species, set sweating_species = FALSE (default).
#'
#' @examples
#' \dontrun{
#' # Example for a small mammal
#' mouse_data <- data.frame(
#'   FoodMassIngested = 5.2,
#'   Digestibility = 0.85,
#'   TotalH2OTurnover = 8.5,
#'   H2OOral = 1.2,
#'   H2ONasal = 0.8,
#'   TranscutaneousH2OLoss = 2.1,
#'   UrinaryH2OLoss = 3.2,
#'   Oprotein = 0.12,
#'   foodproteincontent = 0.18,
#'   EEE = 0.88,
#'   MolesO2Air = 0.42,
#'   dryHinflux = 0.15,
#'   dryOinflux = 0.08
#' )
#'
#' # Calculate outputs for non-sweating species (default)
#' mouse_outputs <- outputs_function(mouse_data)
#'
#' # Calculate outputs for sweating species with debug info
#' human_outputs <- outputs_function(human_data, sweating_species = TRUE, printinfo = TRUE)
#'
#' # Multiple animals
#' multi_animal_data <- data.frame(
#'   FoodMassIngested = c(100, 150, 200),
#'   Digestibility = c(0.8, 0.75, 0.85),
#'   TotalH2OTurnover = c(1000, 1200, 1400),
#'   H2OOral = c(50, 60, 70),
#'   H2ONasal = c(20, 25, 30),
#'   TranscutaneousH2OLoss = c(80, 90, 100),
#'   UrinaryH2OLoss = c(150, 180, 200),
#'   Oprotein = c(0.1, 0.12, 0.08),
#'   foodproteincontent = c(0.2, 0.18, 0.22),
#'   EEE = c(0.9, 0.85, 0.92),
#'   MolesO2Air = c(10, 12, 8),
#'   dryHinflux = c(4, 5, 3),
#'   dryOinflux = c(2, 2.5, 1.5)
#' )
#' multi_outputs <- outputs_function(multi_animal_data, sweating_species = FALSE)
#' }
#'
#' @export
outputs_function <- function(inputs = 0, sweating_species = FALSE) {
  if (!sweating_species) {
    message("Using non-sweating species model (WVSweat = 0)")
  }

  validate_inputs(inputs)

  sweat_function <- ifelse(sweating_species,
    calculate_wv_sweating,
    calculate_wv_not_sweating
  )

  prepare_outputs_dataframe(inputs) |>
    with_column("DryFecalOutput", calculate_dry_fecal_output) |>
    with_column("FecalH20Loss", calculate_fecal_water_loss) |>
    with_column("WVFecal", calculate_wv_fecal) |>
    with_column("WaterHeatLoss", calculate_water_heat_loss) |>
    with_column("WVSweat", sweat_function) |>
    with_column("Panting", calculate_panting) |>
    with_column("WVMouth", calculate_wv_mouth) |>
    with_column("UreaProduced", calculate_urea_produced) |>
    with_column("WVCO2", calculate_wv_co2)
}

#' Validate required input columns and data types
#' @param inputs Input data frame to validate
#' @keywords internal
validate_inputs <- function(inputs) {
  required_cols <- c(
    "FoodMassIngested", "Digestibility", "TotalH2OTurnover",
    "H2OOral", "H2ONasal", "TranscutaneousH2OLoss", "UrinaryH2OLoss",
    "Oprotein", "foodproteincontent", "EEE", "MolesO2Air",
    "dryHinflux", "dryOinflux"
  )

  missing_cols <- setdiff(required_cols, names(inputs))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(inputs) == 0) {
    stop("Input data frame is empty")
  }

  # Check for negative values where they don't make biological sense
  non_negative_cols <- c(
    "FoodMassIngested", "TotalH2OTurnover", "H2OOral",
    "H2ONasal", "TranscutaneousH2OLoss", "UrinaryH2OLoss"
  )

  for (col in non_negative_cols) {
    if (any(inputs[[col]] < 0, na.rm = TRUE)) {
      warning(sprintf("Negative values found in %s column", col))
    }
  }

  # Check digestibility is between 0 and 1
  if (any(inputs$Digestibility < 0 | inputs$Digestibility > 1, na.rm = TRUE)) {
    warning("Digestibility values should be between 0 and 1")
  }
}

FECAL_WATER_MULTIPLIER <- 55.56
FECAL_WATER_CONTENT <- 0.6
WATER_VAPOR_FRACTION <- 0.5
SWEAT_HEAT_FRACTION <- 0.75
PANTING_HEAT_FRACTION <- 0.5

#' Calculate dry fecal output
#' @param df Dataframe with FoodMassIngested and Digestibility columns
#' @return Numeric vector of dry fecal output values
#' @keywords internal
calculate_dry_fecal_output <- function(df) {
  df$FoodMassIngested * (1 - df$Digestibility)
}

#' Calculate fecal water loss
#' @param df Dataframe with DryFecalOutput column
#' @return Numeric vector of fecal water loss values
#' @keywords internal
calculate_fecal_water_loss <- function(df) {
  fecal_water_ratio <- FECAL_WATER_CONTENT / (1 - FECAL_WATER_CONTENT)
  df$DryFecalOutput * FECAL_WATER_MULTIPLIER * fecal_water_ratio
}

#' Calculate water vapor from fecal loss
#' @param df Dataframe with FecalH20Loss column
#' @return Numeric vector of fecal water vapor values
#' @keywords internal
calculate_wv_fecal <- function(df) {
  df$FecalH20Loss / 2
}

#' Calculate water used for heat loss
#' @param df Dataframe with water turnover and loss columns
#' @return Numeric vector of water heat loss values
#' @keywords internal
calculate_water_heat_loss <- function(df) {
  df$TotalH2OTurnover - df$H2OOral - df$H2ONasal -
    df$TranscutaneousH2OLoss - df$UrinaryH2OLoss - df$FecalH20Loss
}

#' Calculate panting water loss
#' @param df Dataframe with WaterHeatLoss column
#' @return Numeric vector of panting values
#' @keywords internal
calculate_panting <- function(df) {
  PANTING_HEAT_FRACTION * df$WaterHeatLoss
}

#' Calculate water vapor from mouth
#' @param df Dataframe with H2OOral and Panting columns
#' @return Numeric vector of mouth water vapor values
#' @keywords internal
calculate_wv_mouth <- function(df) {
  df$H2OOral * WATER_VAPOR_FRACTION +
    df$Panting * WATER_VAPOR_FRACTION
}

#' Calculate urea production
#' @param df Dataframe with protein metabolism columns
#' @return Numeric vector of urea production values
#' @keywords internal
calculate_urea_produced <- function(df) {
  df$Oprotein * df$foodproteincontent * df$EEE *
    df$Digestibility * df$FoodMassIngested
}

#' Calculate water vapor from CO2
#' @param df Dataframe with O2, urea, and influx columns
#' @return Numeric vector of CO2 water vapor values
#' @keywords internal
calculate_wv_co2 <- function(df) {
  df$MolesO2Air - df$UreaProduced -
    (df$dryHinflux * WATER_VAPOR_FRACTION - df$dryOinflux)
}

#' Calculate water vapor from sweating
#' @param df Dataframe with WaterHeatLoss column
#' @return Numeric vector of sweat water vapor values
#' @keywords internal
calculate_wv_sweating <- function(df) {
  SWEAT_HEAT_FRACTION * df$WaterHeatLoss
}

#' Calculate water vapor for non-sweating species
#' @param df Dataframe (parameter ignored)
#' @return Numeric vector of zeros
#' @keywords internal
calculate_wv_not_sweating <- function(df) {
  rep(0, nrow(df))
}


#' Prepare outputs dataframe with initialized calculation columns
#'
#' @param inputs A dataframe containing inputs for calculations
#' @return A dataframe with original inputs and new columns. Initialized to 0
#' @keywords internal
prepare_outputs_dataframe <- function(inputs) {
  data.frame(
    inputs,
    DryFecalOutput = 0,
    FecalH20Loss = 0,
    WVFecal = 0,
    WaterHeatLoss = 0,
    WVSweat = 0,
    Panting = 0,
    WVMouth = 0,
    UreaProduced = 0,
    WVCO2 = 0,
    row.names = NULL
  )
}
