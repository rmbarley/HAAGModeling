#' Add column to dataframe by applying function
#'
#' Helper function for pipeline calculations
#'
#' @param df Data frame to modify
#' @param column_name Character. Name of new column to add
#' @param func Function that takes a data frame and returns calculated values
#' @return Data frame with new column added
#' @keywords internal
with_column <- function(df, column_name, func) {
  df[[column_name]] <- func(df)
  return(df)
}

#' Create combinations dataframe from Species, Food, and Environment inputs
#'
#' @param species Data frame with species parameters
#' @param food Data frame with food composition data
#' @param environment Data frame with environmental conditions
#' @return Data frame with all input combinations
#' @keywords internal
combine_inputs <- function(species, food, environment) {
  combinations <- expand.grid(
    species_row     = seq_len(nrow(species)),
    food_row        = seq_len(nrow(food)),
    environment_row = seq_len(nrow(environment))
  )

  data.frame(
    species[combinations$species_row, ],
    food[combinations$food_row, ],
    environment[combinations$environment_row, ],
    FoodMassIngested = 0,
    dryOinflux = 0,
    dryHinflux = 0,
    FreeH2Oinfood = 0,
    WaterinFood = 0,
    DrinkingH2OIngested = 0,
    DrinkingWater = 0,
    row.names = NULL
  )
}
