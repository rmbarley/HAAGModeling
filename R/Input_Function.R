#' Calculate input variables for model
#'
#' This function combines species, food, and environment data to calculate
#' input variables for model. This includes food mass ingested,
#' dry matter influx, and water balance components.
#'
#' @param species Data frame containing species physiological parameters.
#'   Must include: EnergyExp, TotalH2OTurnover, WVinLungs
#' @param food Data frame containing food composition data.
#'   Must include: Digestibility, EEE, food macronutrient contents and energies,
#'   elemental compositions (O and H ratios), and freeH20food
#' @param environment Data frame containing environmental conditions
#' @return Data frame with all input combinations and calculated physiological
#'         variables:
#'   \itemize{
#'     \item FoodMassIngested - Mass of food consumed
#'     \item dryOinflux - Dry oxygen influx from food
#'     \item dryHinflux - Dry hydrogen influx from food
#'     \item FreeH2Oinfood - Free water content in food
#'     \item WaterinFood - Water vapor from food (half of free water)
#'     \item DrinkingH2OIngested - Additional water intake required
#'     \item DrinkingWater - Drinking water vapor (half of ingested)
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Create example data
#' species_data <- data.frame(
#'   EnergyExp = 5000,
#'   TotalH2OTurnover = 1000,
#'   WVinLungs = 50
#' )
#'
#' food_data <- data.frame(
#'   Digestibility = 0.85,
#'   EEE = 0.95,
#'   foodcarbcontent = 0.6, foodcarbenergy = 17,
#'   foodproteincontent = 0.25, foodproteinenergy = 23,
#'   foodfatcontent = 0.05, foodfatenergy = 39,
#'   Ocarb = 1.33, Oprotein = 1.41, Ofat = 2.9,
#'   Hcarb = 1.51, Hprotein = 1.29, Hfat = 1.94,
#'   freeH20food = 0.75
#' )
#'
#' environment_data <- data.frame(
#'   Temperature = 25,
#'   Humidity = 60
#' )
#'
#' result <- Input_Function(
#'   species = species_data,
#'   food = food_data,
#'   environment = environment_data
#' )
#' }
Input_Function <- function(species = 0, food = 0, environment = 0) {
  if (identical(species, 0) || identical(food, 0) || identical(environment, 0)) {
    stop("All three arguments (Species, Food, Environment) must be provided as data frames")
  }

  if (!is.data.frame(species) || !is.data.frame(food) || !is.data.frame(environment)) {
    stop("Species, Food, and Environment must be data frames")
  }

  if (nrow(species) == 0 || nrow(food) == 0 || nrow(environment) == 0) {
    stop("Arguments cannot be empty")
  }

  combine_inputs(species, food, environment) |>
    with_column("FoodMassIngested", calculate_food_mass) |>
    with_column("dryOinflux", calculate_dry_oxygen_influx) |>
    with_column("dryHinflux", calculate_dry_hydrogen_influx) |>
    with_column("FreeH2Oinfood", calculate_free_water_in_food) |>
    with_column("WaterinFood", calculate_water_in_food) |>
    with_column("DrinkingH2OIngested", calculate_drinking_water_ingested) |>
    with_column("DrinkingWater", calculate_drinking_water)
}

#' Calculate effective food mass for metabolic processes
#' @param df Data frame containing FoodMassIngested, Digestibility, and EEE
#' @return Numeric vector of effective food mass
#'         (FoodMassIngested × Digestibility × EEE)
#' @keywords internal
calculate_effective_food_mass <- function(df) {
  df$FoodMassIngested * df$Digestibility * df$EEE
}

#' Calculate energy density of food
#' @param df Data frame containing food composition columns
#' @return Numeric vector of energy densities (kJ/g)
#' @keywords internal
calculate_energy_density <- function(df) {
  df$foodcarbcontent * df$foodcarbenergy +
    df$foodproteincontent * df$foodproteinenergy +
    df$foodfatcontent * df$foodfatenergy
}

#' Calculate food mass ingested
#' @param df Data frame containing EnergyExp, Digestibility, EEE, and
#'           food composition
#' @return Numeric vector of food mass ingested (g)
#' @keywords internal
calculate_food_mass <- function(df) {
  df$EnergyExp / (df$Digestibility * df$EEE * calculate_energy_density(df))
}

#' Calculate dry oxygen influx
#' @param df Data frame containing food mass, digestibility, EEE,
#'           and oxygen composition
#' @return Numeric vector of dry oxygen influx
#' @keywords internal
calculate_dry_oxygen_influx <- function(df) {
  o_composition <- df$foodcarbcontent * df$Ocarb +
    df$foodproteincontent * df$Oprotein +
    df$foodfatcontent * df$Ofat
  calculate_effective_food_mass(df) * o_composition
}

#' Calculate dry hydrogen influx
#' @param df Data frame containing food mass, digestibility, EEE,
#'           and hydrogen composition
#' @return Numeric vector of dry hydrogen influx
#' @keywords internal
calculate_dry_hydrogen_influx <- function(df) {
  h_composition <- df$foodcarbcontent * df$Hcarb +
    df$foodproteincontent * df$Hprotein +
    df$foodfatcontent * df$Hfat

  calculate_effective_food_mass(df) * h_composition
}

#' Calculate free water in food
#' @param df Data frame containing FoodMassIngested and freeH20food
#' @return Numeric vector of free water in food (moles)
#' @keywords internal
calculate_free_water_in_food <- function(df) {
  mole_water <- 55.56
  df$FoodMassIngested * mole_water * (df$freeH20food / (1 - df$freeH20food))
}

#' Calculate drinking water (vapor)
#' @param df Data frame containing DrinkingH2OIngested
#' @return Numeric vector of drinking water vapor
#' @keywords internal
calculate_drinking_water <- function(df) {
  df$DrinkingH2OIngested / 2
}

#' Calculate drinking water ingested
#' @param df Data frame containing water balance components
#' @return Numeric vector of drinking water ingested (moles)
#' @keywords internal
calculate_drinking_water_ingested <- function(df) {
  df$TotalH2OTurnover - df$FreeH2Oinfood - df$dryHinflux - df$WVinLungs
}

#' Calculate water in food (vapor)
#' @param df Data frame containing FreeH2Oinfood
#' @return Numeric vector of water vapor from food
#' @keywords internal
calculate_water_in_food <- function(df) {
  df$FreeH2Oinfood / 2
}
