test_that("Input_Function returns correct data structure", {
  species <- create_test_species(1)
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  result <- Input_Function(species = species, food = food, environment = environment)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), ncol(species) + ncol(food) + ncol(environment) + 7)
})

test_that("Input_Function includes all required columns", {
  species <- create_test_species(1)
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  result <- Input_Function(species = species, food = food, environment = environment)

  # Check original columns are preserved
  expect_true(all(colnames(species) %in% colnames(result)))
  expect_true(all(colnames(food) %in% colnames(result)))
  expect_true(all(colnames(environment) %in% colnames(result)))

  # Check new calculated columns exist
  new_cols <- c(
    "FoodMassIngested", "dryOinflux", "dryHinflux",
    "FreeH2Oinfood", "WaterinFood", "DrinkingH2OIngested",
    "DrinkingWater"
  )
  expect_true(all(new_cols %in% colnames(result)))
})

test_that("Input_Function handles multiple combinations correctly", {
  species <- data.frame(
    SpeciesID = c("A", "B"),
    EnergyExp = c(1000, 2000),
    TotalH2OTurnover = c(100, 200),
    WVinLungs = c(10, 20)
  )

  food <- data.frame(
    FoodID = c("X", "Y"),
    Digestibility = c(0.8, 0.85),
    EEE = c(0.9, 0.92),
    foodcarbcontent = c(0.5, 0.55),
    foodcarbenergy = rep(17, 2),
    foodproteincontent = c(0.2, 0.22),
    foodproteinenergy = rep(23, 2),
    foodfatcontent = c(0.1, 0.08),
    foodfatenergy = rep(39, 2),
    Ocarb = rep(1.33, 2), Oprotein = rep(1.41, 2), Ofat = rep(2.9, 2),
    Hcarb = rep(1.51, 2), Hprotein = rep(1.29, 2), Hfat = rep(1.94, 2),
    freeH20food = c(0.7, 0.72)
  )

  environment <- data.frame(
    EnvID = c("1", "2"),
    Temperature = c(20, 25),
    Humidity = c(50, 60)
  )

  result <- Input_Function(species = species, food = food, environment = environment)

  # Should have 2 × 2 × 2 = 8 combinations
  expect_equal(nrow(result), 8)

  # Each species should appear 4 times (2 foods × 2 environments)
  expect_equal(sum(result$SpeciesID == "A"), 4)
  expect_equal(sum(result$SpeciesID == "B"), 4)

  # Each food should appear 4 times (2 species × 2 environments)
  expect_equal(sum(result$FoodID == "X"), 4)
  expect_equal(sum(result$FoodID == "Y"), 4)

  # Each environment should appear 4 times (2 species × 2 foods)
  expect_equal(sum(result$EnvID == "1"), 4)
  expect_equal(sum(result$EnvID == "2"), 4)
})

# Calculation tests

test_that("calculate_energy_density works correctly", {
  food_data <- data.frame(
    foodcarbcontent = 0.5, foodcarbenergy = 17,
    foodproteincontent = 0.2, foodproteinenergy = 23,
    foodfatcontent = 0.1, foodfatenergy = 39
  )

  result <- calculate_energy_density(food_data)
  expected <- 0.5 * 17 + 0.2 * 23 + 0.1 * 39 # 17.0

  expect_equal(result, expected)
})

test_that("calculate_food_mass works correctly", {
  test_data <- data.frame(
    EnergyExp = 1000,
    Digestibility = 0.8,
    EEE = 0.9,
    foodcarbcontent = 0.5, foodcarbenergy = 17,
    foodproteincontent = 0.2, foodproteinenergy = 23,
    foodfatcontent = 0.1, foodfatenergy = 39
  )

  result <- calculate_food_mass(test_data)

  energy_density <- calculate_energy_density(test_data)
  expected <- 1000 / (0.8 * 0.9 * energy_density)

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("calculate_dry_oxygen_influx works correctly", {
  test_data <- data.frame(
    FoodMassIngested = 100,
    Digestibility = 0.8,
    EEE = 0.9,
    foodcarbcontent = 0.5, Ocarb = 1.33,
    foodproteincontent = 0.2, Oprotein = 1.41,
    foodfatcontent = 0.1, Ofat = 2.9
  )

  result <- calculate_dry_oxygen_influx(test_data)

  o_composition <- 0.5 * 1.33 + 0.2 * 1.41 + 0.1 * 2.9
  expected <- 0.8 * 0.9 * 100 * o_composition

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("calculate_dry_hydrogen_influx works correctly", {
  test_data <- data.frame(
    FoodMassIngested = 100,
    Digestibility = 0.8,
    EEE = 0.9,
    foodcarbcontent = 0.5, Hcarb = 1.51,
    foodproteincontent = 0.2, Hprotein = 1.29,
    foodfatcontent = 0.1, Hfat = 1.94
  )

  result <- calculate_dry_hydrogen_influx(test_data)

  h_composition <- 0.5 * 1.51 + 0.2 * 1.29 + 0.1 * 1.94
  expected <- 0.8 * 0.9 * 100 * h_composition

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("calculate_free_water_in_food works correctly", {
  test_data <- data.frame(
    FoodMassIngested = 100,
    freeH20food = 0.7
  )

  result <- calculate_free_water_in_food(test_data)
  expected <- 100 * 55.56 * (0.7 / (1 - 0.7))

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("calculate_drinking_water_ingested works correctly", {
  test_data <- data.frame(
    TotalH2OTurnover = 1000,
    FreeH2Oinfood = 200,
    dryHinflux = 50,
    WVinLungs = 30
  )

  result <- calculate_drinking_water_ingested(test_data)
  expected <- 1000 - 200 - 50 - 30 # 720

  expect_equal(result, expected)
})

test_that("calculate_drinking_water works correctly", {
  test_data <- data.frame(
    DrinkingH2OIngested = 720
  )

  result <- calculate_drinking_water(test_data)
  expected <- 720 / 2 # 360

  expect_equal(result, expected)
})

test_that("water calculations work together correctly", {
  test_data <- data.frame(
    TotalH2OTurnover = 1000,
    FreeH2Oinfood = 200,
    dryHinflux = 50,
    WVinLungs = 30
  )

  drinking_h2o <- calculate_drinking_water_ingested(test_data)
  expect_equal(drinking_h2o, 720)

  test_data$DrinkingH2OIngested <- drinking_h2o

  drinking_water <- calculate_drinking_water(test_data)
  expect_equal(drinking_water, 360)
  expect_equal(drinking_water, drinking_h2o / 2)
})

test_that("water calculations handle edge cases", {
  test_data_negative <- data.frame(
    TotalH2OTurnover = 100,
    FreeH2Oinfood = 200,
    dryHinflux = 50,
    WVinLungs = 30
  )

  result_negative <- calculate_drinking_water_ingested(test_data_negative)
  expected_negative <- 100 - 200 - 50 - 30 # -180
  expect_equal(result_negative, expected_negative)

  # Test with zero values
  test_data_zero <- data.frame(
    TotalH2OTurnover = 100,
    FreeH2Oinfood = 50,
    dryHinflux = 25,
    WVinLungs = 25,
    DrinkingH2OIngested = 0
  )

  result_zero_drinking <- calculate_drinking_water_ingested(test_data_zero)
  expect_equal(result_zero_drinking, 0)

  result_zero_water <- calculate_drinking_water(test_data_zero)
  expect_equal(result_zero_water, 0)
})

test_that("water in food calculation works correctly", {
  test_data <- data.frame(
    FreeH2Oinfood = 200
  )

  result <- calculate_water_in_food(test_data)
  expected <- 200 / 2 # 100

  expect_equal(result, expected)
})

test_that("all water-related calculations work together", {
  test_data <- data.frame(
    FoodMassIngested = 100,
    freeH20food = 0.6,
    TotalH2OTurnover = 1000,
    dryHinflux = 75,
    WVinLungs = 25
  )

  # Calculate free water in food
  free_h2o <- calculate_free_water_in_food(test_data)
  expected_free_h2o <- 100 * 55.56 * (0.6 / (1 - 0.6)) # 8334
  expect_equal(free_h2o, expected_free_h2o, tolerance = 1e-12)

  # Calculate water in food
  test_data$FreeH2Oinfood <- free_h2o
  water_in_food <- calculate_water_in_food(test_data)
  expect_equal(water_in_food, free_h2o / 2, tolerance = 1e-12)

  # Calculate drinking water ingested
  drinking_h2o <- calculate_drinking_water_ingested(test_data)
  expected_drinking <- 1000 - free_h2o - 75 - 25
  expect_equal(drinking_h2o, expected_drinking, tolerance = 1e-12)

  # Calculate drinking water
  test_data$DrinkingH2OIngested <- drinking_h2o
  drinking_water <- calculate_drinking_water(test_data)
  expect_equal(drinking_water, drinking_h2o / 2, tolerance = 1e-12)
})

test_that("Input_Function produces consistent results", {
  species <- create_test_species(1)
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  result1 <- Input_Function(species = species, food = food, environment = environment)
  result2 <- Input_Function(species = species, food = food, environment = environment)

  expect_identical(result1, result2)
})

test_that("biological scaling relationships work", {
  # Test that higher energy expenditure leads to higher food intake
  species_low <- data.frame(EnergyExp = 1000, TotalH2OTurnover = 100, WVinLungs = 10, stringsAsFactors = FALSE)
  species_high <- data.frame(EnergyExp = 5000, TotalH2OTurnover = 500, WVinLungs = 50, stringsAsFactors = FALSE)

  food <- create_test_food(1)
  environment <- create_test_environment(1)

  result_low <- Input_Function(species = species_low, food = food, environment = environment)
  result_high <- Input_Function(species = species_high, food = food, environment = environment)

  # Higher energy should lead to proportionally higher food intake and metabolite fluxes
  expect_gt(result_high$FoodMassIngested, result_low$FoodMassIngested)
  expect_gt(result_high$dryOinflux, result_low$dryOinflux)
  expect_gt(result_high$dryHinflux, result_low$dryHinflux)
  expect_gt(result_high$FreeH2Oinfood, result_low$FreeH2Oinfood)
})

test_that("helper functions handle edge cases", {
  edge_data <- data.frame(
    EnergyExp = 1.0,
    TotalH2OTurnover = 1.0,
    WVinLungs = 0.1,
    Digestibility = 0.5, EEE = 0.5,
    foodcarbcontent = 0.3, foodcarbenergy = 10,
    foodproteincontent = 0.3, foodproteinenergy = 10,
    foodfatcontent = 0.3, foodfatenergy = 10,
    Ocarb = 1.0, Oprotein = 1.0, Ofat = 1.0,
    Hcarb = 1.0, Hprotein = 1.0, Hfat = 1.0,
    freeH20food = 0.5,
    FoodMassIngested = 0.1,
    FreeH2Oinfood = 0.05,
    dryHinflux = 0.01,
    stringsAsFactors = FALSE
  )

  expect_true(is.finite(calculate_energy_density(edge_data)))
  expect_true(is.finite(calculate_food_mass(edge_data)))
  expect_true(is.finite(calculate_dry_oxygen_influx(edge_data)))
  expect_true(is.finite(calculate_dry_hydrogen_influx(edge_data)))
  expect_true(is.finite(calculate_free_water_in_food(edge_data)))
  expect_true(is.finite(calculate_drinking_water_ingested(edge_data)))
})

# Parameter Validation

test_that("Input_Function validates data frame inputs", {
  species <- create_test_species(1)
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  expect_error(
    Input_Function(species = "not_a_dataframe", food = food, environment = environment),
    "Species, Food, and Environment must be data frames"
  )

  expect_error(
    Input_Function(species = species, food = list(a = 1), environment = environment),
    "Species, Food, and Environment must be data frames"
  )

  expect_error(
    Input_Function(species = species, food = food, environment = matrix(1:4, nrow = 2)),
    "Species, Food, and Environment must be data frames"
  )

  expect_error(
    Input_Function(species = NULL, food = food, environment = environment),
    "Species, Food, and Environment must be data frames"
  )
})

test_that("Input_Function validates default arguments", {
  # Test that default values (0) are rejected
  expect_error(
    Input_Function(),
    "All three arguments.*must be provided as data frames"
  )

  expect_error(
    Input_Function(species = 0, food = 0, environment = 0),
    "All three arguments.*must be provided as data frames"
  )
})

test_that("Input_Function handles empty data frames", {
  species <- create_test_species(1)
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  species_empty <- data.frame(
    EnergyExp = numeric(0),
    TotalH2OTurnover = numeric(0),
    WVinLungs = numeric(0)
  )
  expect_error(
    Input_Function(species = species_empty, food = food, environment = environment),
  )

  food_empty <- create_test_food(1)[0, ]
  expect_error(
    Input_Function(species = species, food = food_empty, environment = environment),
  )
})

test_that("Input_Function validates column data types", {
  food <- create_test_food(1)
  environment <- create_test_environment(1)

  species_bad_types <- data.frame(
    EnergyExp = "not_a_number",
    TotalH2OTurnover = 1000,
    WVinLungs = 50,
    stringsAsFactors = FALSE
  )

  expect_error(
    Input_Function(
      species = species_bad_types,
      food = food,
      environment = environment
    ),
  )
})
