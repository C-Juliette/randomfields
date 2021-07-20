test_that("Regular test - 22 is an integer", {
  #Arrange
  number <- 22
  #Act
  actual <- iswholenumber(number)
  #Assert
  expected <- TRUE
  expect_equal(actual, expected)
})

test_that("Regular test - -22 is an integer", {
  #Arrange
  number <- -22
  #Act
  actual <- iswholenumber(number)
  #Assert
  expected <- TRUE
  expect_equal(actual, expected)
})

test_that("Regular test - 22.5 is not an integer", {
  #Arrange
  number <- 22.5
  #Act
  actual <- iswholenumber(number)
  #Assert
  expected <- FALSE
  expect_equal(actual, expected)
})
