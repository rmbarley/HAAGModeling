create_sample_inputs <- function(n = 3) {
  data.frame(
    FoodMassIngested = c(100, 150, 200),
    Digestibility = c(0.8, 0.75, 0.85),
    TotalH2OTurnover = c(1000, 1200, 1400),
    H2OOral = c(50, 60, 70),
    H2ONasal = c(20, 25, 30),
    TranscutaneousH2OLoss = c(80, 90, 100),
    UrinaryH2OLoss = c(150, 180, 200),
    Oprotein = c(0.1, 0.12, 0.08),
    foodproteincontent = c(0.2, 0.18, 0.22),
    EEE = c(0.9, 0.85, 0.92),
    MolesO2Air = c(10, 12, 8),
    dryHinflux = c(4, 5, 3),
    dryOinflux = c(2, 2.5, 1.5)
  )
}

test_that("calculate_dry_fecal_output works correctly", {
  df <- create_sample_inputs()
  result <- calculate_dry_fecal_output(df)

  expected <- df$FoodMassIngested * (1 - df$Digestibility)
  expect_equal(result, expected)
  expect_equal(result, c(20, 37.5, 30))
  expect_length(result, nrow(df))
  expect_type(result, "double")
})

test_that("calculate_fecal_water_loss works correctly", {
  df <- create_sample_inputs()
  df$DryFecalOutput <- calculate_dry_fecal_output(df)
  result <- calculate_fecal_water_loss(df)

  expected <- df$DryFecalOutput * 55.56 * 0.6 / (1 - 0.6)
  expect_equal(result, expected)
  expect_length(result, nrow(df))
  expect_type(result, "double")
  expect_true(all(result >= 0))
})

test_that("calculate_wv_fecal works correctly", {
  df <- create_sample_inputs()
  df$FecalH20Loss <- c(100, 150, 200)
  result <- calculate_wv_fecal(df)

  expect_equal(result, c(50, 75, 100))
  expect_equal(result, df$FecalH20Loss / 2)
})

test_that("calculate_water_heat_loss works correctly", {
  df <- create_sample_inputs()
  df$FecalH20Loss <- c(100, 150, 200)
  result <- calculate_water_heat_loss(df)

  expected <- df$TotalH2OTurnover - df$H2OOral - df$H2ONasal -
    df$TranscutaneousH2OLoss - df$UrinaryH2OLoss - df$FecalH20Loss
  expect_equal(result, expected)
  expect_length(result, nrow(df))
})

test_that("calculate_panting works correctly", {
  df <- data.frame(WaterHeatLoss = c(100, 200, 300))
  result <- calculate_panting(df)

  expect_equal(result, c(50, 100, 150))
  expect_equal(result, 0.5 * df$WaterHeatLoss)
})

test_that("calculate_wv_mouth works correctly", {
  df <- data.frame(
    H2OOral = c(50, 60, 70),
    Panting = c(50, 100, 150)
  )
  result <- calculate_wv_mouth(df)

  expected <- df$H2OOral / 2 + df$Panting / 2
  expect_equal(result, expected)
  expect_equal(result, c(50, 80, 110))
})

test_that("calculate_urea_produced works correctly", {
  df <- create_sample_inputs()
  result <- calculate_urea_produced(df)

  expected <- df$Oprotein * df$foodproteincontent * df$EEE *
    df$Digestibility * df$FoodMassIngested
  expect_equal(result, expected)
  expect_length(result, nrow(df))
  expect_true(all(result >= 0))
})

test_that("calculate_wv_co2 works correctly", {
  df <- create_sample_inputs()
  df$UreaProduced <- c(1, 2, 3)
  result <- calculate_wv_co2(df)

  expected <- df$MolesO2Air - df$UreaProduced -
    (df$dryHinflux / 2 - df$dryOinflux)
  expect_equal(result, expected)
  expect_length(result, nrow(df))
})

test_that("calculate_wv_sweating works correctly", {
  df <- data.frame(WaterHeatLoss = c(100, 200, 300))
  result <- calculate_wv_sweating(df)

  expect_equal(result, c(75, 150, 225))
  expect_equal(result, 0.75 * df$WaterHeatLoss)
})

test_that("calculate_wv_not_sweating works correctly", {
  df <- create_sample_inputs()
  result <- calculate_wv_not_sweating(df)

  expect_equal(result, rep(0, nrow(df)))
  expect_length(result, nrow(df))
  expect_true(all(result == 0))
})

test_that("prepare_outputs_dataframe works correctly", {
  inputs <- create_sample_inputs()
  result <- prepare_outputs_dataframe(inputs)

  expect_true(all(names(inputs) %in% names(result)))

  new_cols <- c(
    "DryFecalOutput", "FecalH20Loss", "WVFecal", "WaterHeatLoss",
    "WVSweat", "Panting", "WVMouth", "UreaProduced", "WVCO2"
  )
  expect_true(all(new_cols %in% names(result)))

  for (col in new_cols) {
    expect_true(all(result[[col]] == 0))
  }

  expect_equal(nrow(result), nrow(inputs))
})

test_that("outputs_function works for non-sweating species", {
  inputs <- create_sample_inputs()

  result <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = FALSE
  )))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(inputs))

  expect_true(all(result$WVSweat == 0))

  expected_cols <- c(
    names(inputs), "DryFecalOutput", "FecalH20Loss", "WVFecal",
    "WaterHeatLoss", "WVSweat", "Panting", "WVMouth",
    "UreaProduced", "WVCO2"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("outputs_function works for sweating species - robust version", {
  inputs <- create_sample_inputs()

  result <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = TRUE
  )))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(inputs))

  expect_equal(result$WVSweat, 0.75 * result$WaterHeatLoss)

  result_no_sweat <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = FALSE
  )))
  expect_true(all(result_no_sweat$WVSweat == 0))

  expect_false(identical(result$WVSweat, result_no_sweat$WVSweat))
})

test_that("outputs_function handles edge cases", {
  single_row <- create_sample_inputs()[1, ]
  result <- suppressMessages(suppressWarnings(outputs_function(single_row)))
  expect_equal(nrow(result), 1)

  zero_inputs <- create_sample_inputs()
  zero_inputs[zero_inputs < 0] <- 0
  result <- suppressMessages(suppressWarnings(outputs_function(zero_inputs)))
  expect_s3_class(result, "data.frame")

  incomplete_inputs <- create_sample_inputs()[, 1:5]
  expect_error(outputs_function(incomplete_inputs))
})

test_that("Function handles different input sizes", {
  large_inputs <- do.call(rbind, replicate(100, create_sample_inputs(),
    simplify = FALSE
  ))
  result <- suppressMessages(suppressWarnings(outputs_function(large_inputs)))
  expect_equal(nrow(result), 300)

  minimal_inputs <- create_sample_inputs()[1:1, ]
  result <- suppressMessages(suppressWarnings(outputs_function(minimal_inputs)))
  expect_equal(nrow(result), 1)
})

test_that("Function produces consistent results", {
  inputs <- create_sample_inputs()

  result1 <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = TRUE
  )))
  result2 <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = TRUE
  )))

  expect_identical(result1, result2)
})

test_that("Calculation chain is mathematically consistent", {
  inputs <- create_sample_inputs()
  result <- suppressMessages(suppressWarnings(outputs_function(
    inputs,
    sweating_species = FALSE
  )))

  expect_equal(
    result$DryFecalOutput,
    inputs$FoodMassIngested * (1 - inputs$Digestibility)
  )

  expect_equal(
    result$FecalH20Loss,
    result$DryFecalOutput * 55.56 * 0.6 / (1 - 0.6)
  )

  expect_equal(result$WVFecal, result$FecalH20Loss / 2)

  expect_equal(result$Panting, 0.5 * result$WaterHeatLoss)
})

test_that("Function performs reasonably on large datasets", {
  large_inputs <- do.call(rbind, replicate(1000, create_sample_inputs(),
    simplify = FALSE
  ))

  start_time <- Sys.time()
  result <- suppressMessages(suppressWarnings(outputs_function(large_inputs)))
  end_time <- Sys.time()

  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(execution_time, 5)
  expect_equal(nrow(result), 3000)
})

test_that("validate_inputs catches missing columns", {
  empty_df <- data.frame(x = 1:3, y = 4:6)
  expect_error(validate_inputs(empty_df), "Missing required columns")

  partial_df <- data.frame(
    FoodMassIngested = c(100, 150),
    Digestibility = c(0.8, 0.75)
  )
  expect_error(validate_inputs(partial_df), "Missing required columns")

  almost_complete <- create_sample_inputs()
  almost_complete$MolesO2Air <- NULL
  expect_error(validate_inputs(almost_complete), "MolesO2Air")
})

test_that("validate_inputs catches empty data frame", {
  empty_df <- create_sample_inputs()[0, ]
  expect_error(validate_inputs(empty_df), "Input data frame is empty")
})

test_that("validate_inputs warns about negative values", {
  inputs_with_negatives <- create_sample_inputs()
  inputs_with_negatives$FoodMassIngested[1] <- -10
  inputs_with_negatives$TotalH2OTurnover[2] <- -5

  expect_warning(
    validate_inputs(inputs_with_negatives),
    "Negative values found in FoodMassIngested"
  )
  expect_warning(
    validate_inputs(inputs_with_negatives),
    "Negative values found in TotalH2OTurnover"
  )
})

test_that("validate_inputs warns about invalid digestibility", {
  inputs_high_digest <- create_sample_inputs()
  inputs_high_digest$Digestibility[1] <- 1.5
  expect_warning(
    validate_inputs(inputs_high_digest),
    "Digestibility values should be between 0 and 1"
  )

  inputs_neg_digest <- create_sample_inputs()
  inputs_neg_digest$Digestibility[2] <- -0.1
  expect_warning(
    validate_inputs(inputs_neg_digest),
    "Digestibility values should be between 0 and 1"
  )

  inputs_valid <- create_sample_inputs()
  expect_silent(validate_inputs(inputs_valid))
})

test_that("validate_inputs handles NA values appropriately", {
  inputs_with_na <- create_sample_inputs()
  inputs_with_na$FoodMassIngested[1] <- NA
  inputs_with_na$Digestibility[2] <- NA

  expect_silent(validate_inputs(inputs_with_na))
})

test_that("validate_inputs accepts valid input", {
  valid_inputs <- create_sample_inputs()
  expect_silent(validate_inputs(valid_inputs))
})

test_that("outputs_function fails gracefully with invalid inputs", {
  expect_error(
    outputs_function(data.frame(x = 1, y = 2)),
    "Missing required columns"
  )

  empty_inputs <- create_sample_inputs()[0, ]
  expect_error(
    outputs_function(empty_inputs),
    "Input data frame is empty"
  )

  expect_error(
    outputs_function(NULL),
    "Missing required columns"
  )
})

test_that("Input validation warnings are informative", {
  problematic_inputs <- create_sample_inputs()
  problematic_inputs$FoodMassIngested[1] <- -50
  problematic_inputs$TotalH2OTurnover[2] <- -100
  problematic_inputs$Digestibility[1] <- 1.2

  expect_warning(
    validate_inputs(problematic_inputs),
    "FoodMassIngested"
  )
  expect_warning(
    validate_inputs(problematic_inputs),
    "TotalH2OTurnover"
  )
  expect_warning(
    validate_inputs(problematic_inputs),
    "Digestibility"
  )
})

test_that("Validation works with single row input", {
  single_row <- create_sample_inputs()[1, ]
  expect_silent(validate_inputs(single_row))

  single_row$Digestibility <- 2.0
  expect_warning(validate_inputs(single_row), "Digestibility")
})

test_that("Validation works with large datasets", {
  large_inputs <- do.call(rbind, replicate(1000, create_sample_inputs(),
    simplify = FALSE
  ))
  expect_silent(validate_inputs(large_inputs))

  large_inputs$FoodMassIngested[500] <- -10
  expect_warning(validate_inputs(large_inputs), "FoodMassIngested")
})

test_that("All required columns are properly specified", {
  valid_inputs <- create_sample_inputs()
  expect_silent(validate_inputs(valid_inputs))

  required_cols <- c(
    "FoodMassIngested", "Digestibility", "TotalH2OTurnover",
    "H2OOral", "H2ONasal", "TranscutaneousH2OLoss", "UrinaryH2OLoss",
    "Oprotein", "foodproteincontent", "EEE", "MolesO2Air",
    "dryHinflux", "dryOinflux"
  )

  for (col in required_cols) {
    test_inputs <- valid_inputs
    test_inputs[[col]] <- NULL
    expect_error(
      validate_inputs(test_inputs),
      col,
      info = paste("Column", col, "should be required")
    )
  }
})
