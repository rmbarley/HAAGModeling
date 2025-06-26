#' Create test Species dataframe
#' @param n_rows Number of rows to create
#' @return Data frame with species data
create_test_species <- function(n_rows = 2) {
  data.frame(
    Bodymass = seq(1000, 1000 + (n_rows - 1) * 500, length.out = n_rows),
    EnergyExp = seq(50000, 50000 + (n_rows - 1) * 15000, length.out = n_rows),
    WEI = seq(0.5, 0.5 + (n_rows - 1) * 0.1, length.out = n_rows),
    TotalH2OTurnover = seq(2500, 2500 + (n_rows - 1) * 500, length.out = n_rows),
    WVinLungs = seq(100, 100 + (n_rows - 1) * 20, length.out = n_rows),
    stringsAsFactors = FALSE
  )
}

#' Create test Food dataframe
#' @param n_rows Number of rows to create
#' @return Data frame with food data
create_test_food <- function(n_rows = 2) {
  data.frame(
    Digestibility = seq(0.8, 0.8 + (n_rows - 1) * 0.05, length.out = n_rows),
    EEE = seq(0.9, 0.9 + (n_rows - 1) * 0.05, length.out = n_rows),
    foodcarbcontent = seq(0.5, 0.5 + (n_rows - 1) * 0.1, length.out = n_rows),
    foodcarbenergy = rep(17, n_rows),
    foodproteincontent = seq(0.2, 0.2 + (n_rows - 1) * 0.05, length.out = n_rows),
    foodproteinenergy = rep(23, n_rows),
    foodfatcontent = seq(0.1, 0.1 - (n_rows - 1) * 0.02, length.out = n_rows),
    foodfatenergy = rep(39, n_rows),
    Ocarb = rep(1.33, n_rows),
    Oprotein = rep(1.41, n_rows),
    Ofat = rep(2.9, n_rows),
    Hcarb = rep(1.51, n_rows),
    Hprotein = rep(1.29, n_rows),
    Hfat = rep(1.94, n_rows),
    freeH20food = seq(0.7, 0.7 + (n_rows - 1) * 0.05, length.out = n_rows),
    stringsAsFactors = FALSE
  )
}

#' Create test Environment dataframe
#' @param n_rows Number of rows to create
#' @return Data frame with environment data
create_test_environment <- function(n_rows = 2) {
  data.frame(
    Temperature = seq(20, 20 + (n_rows - 1) * 5, length.out = n_rows),
    Humidity = seq(50, 50 + (n_rows - 1) * 10, length.out = n_rows),
    stringsAsFactors = FALSE
  )
}
