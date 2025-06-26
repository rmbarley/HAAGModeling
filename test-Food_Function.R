library(testthat)

create_test_food <- function(Digestibility = 0.6, foodcarbcontent = 0.8,
                             foodproteincontent = 0.1, foodfatcontent = 0.1,
                             freeH20food = 0.4, ...) {
  defaults <- list(
    Digestibility = Digestibility,
    EEE = 0.9,
    foodcarbcontent = foodcarbcontent,
    foodcarbenergy = 17300,
    foodproteincontent = foodproteincontent,
    foodproteinenergy = 20100,
    foodfatcontent = foodfatcontent,
    foodfatenergy = 39700,
    Ocarb = 15.4,
    Hcarb = 30.9,
    Oprotein = 3,
    Hprotein = 11,
    Ofat = 2,
    Hfat = 6,
    freeH20food = freeH20food
  )

  additional_params <- list(...)
  for(name in names(additional_params)) {
    defaults[[name]] <- additional_params[[name]]
  }

  return(defaults)
}

test_that("Food_Function returns correct structure and values", {
  result <- suppressMessages(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    )
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 15)
  expect_equal(nrow(result), 1)

  expected_names <- c("Digestibility", "EEE", "foodcarbenergy", "foodcarbcontent",
                      "Ocarb", "Hcarb", "foodproteinenergy", "Oprotein", "Hprotein",
                      "foodfatenergy", "Ofat", "Hfat", "foodproteincontent",
                      "foodfatcontent", "freeH20food")
  expect_equal(names(result), expected_names)

  expected <- create_test_food()

  expect_equal(result$Digestibility[1], expected$Digestibility)
  expect_equal(result$foodcarbcontent[1], expected$foodcarbcontent)
  expect_equal(result$foodproteincontent[1], expected$foodproteincontent)
  expect_equal(result$foodfatcontent[1], expected$foodfatcontent)
  expect_equal(result$freeH20food[1], expected$freeH20food)

  expect_equal(result$EEE[1], expected$EEE)
  expect_equal(result$foodcarbenergy[1], expected$foodcarbenergy)
  expect_equal(result$foodproteinenergy[1], expected$foodproteinenergy)
  expect_equal(result$foodfatenergy[1], expected$foodfatenergy)
  expect_equal(result$Ocarb[1], expected$Ocarb)
  expect_equal(result$Hcarb[1], expected$Hcarb)
  expect_equal(result$Oprotein[1], expected$Oprotein)
  expect_equal(result$Hprotein[1], expected$Hprotein)
  expect_equal(result$Ofat[1], expected$Ofat)
  expect_equal(result$Hfat[1], expected$Hfat)
})

test_that("Food_Function validates inputs using base test values", {
  expect_error(
    Food_Function(
      Digestibility_of_food = 0,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    ),
    "Enter Digestibility value"
  )

  expect_error(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    ),
    "Enter Carbohydrate Content of Food as Proportion Value"
  )

  expect_error(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    ),
    "Enter Protein Content of Food as Proportion Value"
  )

  expect_error(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0,
      Free_Water_Content_Food = 0.4
    ),
    "Enter Fat Content of Food as Proportion Value"
  )

  expect_error(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0
    ),
    "Enter Free Water Content of Food as Proportion Value"
  )
})

test_that("Food_Function handles multiple parameter values", {
  result_multi <- suppressMessages(
    Food_Function(
      Digestibility_of_food = c(0.6, 0.8),
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    )
  )

  expect_equal(nrow(result_multi), 2)
  expect_equal(result_multi$Digestibility, c(0.6, 0.8))
  expect_equal(result_multi$foodcarbcontent, c(0.8, 0.8))
  expect_equal(result_multi$freeH20food, c(0.4, 0.4))
})

test_that("Food_Function produces consistent output format", {
  result <- suppressMessages(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    )
  )

  expect_true(all(sapply(result, is.numeric)))

  proportion_cols <- c("Digestibility", "foodcarbcontent", "foodproteincontent",
                       "foodfatcontent", "freeH20food")
  for(col in proportion_cols) {
    expect_true(all(result[[col]] >= 0 & result[[col]] <= 1),
                info = paste("Column", col, "should be between 0 and 1"))
  }

  energy_cols <- c("foodcarbenergy", "foodproteinenergy", "foodfatenergy")
  for(col in energy_cols) {
    expect_true(all(result[[col]] > 0),
                info = paste("Column", col, "should be positive"))
  }
})

test_that("create_test_food helper function works correctly", {
  expected_default <- create_test_food()
  expect_equal(expected_default$Digestibility, 0.6)
  expect_equal(expected_default$foodcarbcontent, 0.8)
  expect_equal(expected_default$EEE, 0.9)

  expected_custom <- create_test_food(Digestibility = 0.8, freeH20food = 0.3)
  expect_equal(expected_custom$Digestibility, 0.8)
  expect_equal(expected_custom$freeH20food, 0.3)
  expect_equal(expected_custom$foodcarbcontent, 0.8)
})

test_that("Food_Function produces exact expected output", {
  output <- suppressMessages(
    Food_Function(
      Digestibility_of_food = 0.6,
      Carbohydrate_Content = 0.8,
      Protein_Content = 0.1,
      Fat_Content = 0.1,
      Free_Water_Content_Food = 0.4
    )
  )

  expected_output <- data.frame(
    Digestibility = 0.6,
    EEE = 0.9,
    foodcarbenergy = 17300,
    foodcarbcontent = 0.8,
    Ocarb = 15.4,
    Hcarb = 30.9,
    foodproteinenergy = 20100,
    Oprotein = 3,
    Hprotein = 11,
    foodfatenergy = 39700,
    Ofat = 2,
    Hfat = 6,
    foodproteincontent = 0.1,
    foodfatcontent = 0.1,
    freeH20food = 0.4
  )

  expect_equal(output, expected_output)
})

test_file <- function() {
  test_that("Food_Function returns correct structure and values", {})
  test_that("Food_Function validates inputs using base test values", {})
  test_that("Food_Function handles multiple parameter values", {})
  test_that("Food_Function produces consistent output format", {})
  test_that("create_test_food helper function works correctly", {})
  test_that("Food_Function produces exact expected output", {})
}

