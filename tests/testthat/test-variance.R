######################### REGULAR TESTS #######################
test_that("Regular test - full of zeros matrix", {
  # arrange
  Z <- matrix(rep(0, 5*5), nrow = 5)
  # act
  actual <- variance(Z)
  # assert
  expected <- 0
  expect_identical(actual, expected)
})

test_that("Regular test - Bernoulli matrix", {
  set.seed(1)
  Z <- matrix(rbinom(2*2, 1, 0.5), nrow = 2)
  expect_identical(variance(Z), 0.5**2)
})

test_that("Regular test - variance of a vector", {
  # arrange
  v <- c(1, 2, 3, 4, 5)
  # act
  actual <- variance(v)
  # assert
  expected <- (1**2)*2/5 + (2**2)*2/5
  expect_identical(actual, expected)
})

test_that("Regular test - variance of a vector with NA", {
  # arrange
  v <- c(1, 2, 3, 4, 5, NA)
  # act
  actual <- variance(v)
  # assert
  expected <- (1**2)*2/5 + (2**2)*2/5
  expect_identical(actual, expected)
})

test_that("Regular test - variance of a number", {
  # arrange
  Z <- 2
  # act
  actual <- variance(Z)
  # assert
  expected <- 0
  expect_identical(actual, expected)
})

######################## ANOMALY TESTS #####################

test_that("Anomaly test - error when the parametre is a string", {
  # arrange
  # act & assert
  expect_error(variance("2"), "^^M must contain numeric values$")
})

test_that("Anomaly test - error when the parametre is a vector of strings", {
  # arrange
  # act & assert
  expect_error(variance(c("2", "3", "4")), "^M must contain numeric values$")
})

test_that("Anomaly test - error when the parametre is a vector of string", {
  # arrange
  # act & assert
  expect_error(variance(c("2")), "^M must contain numeric values$")
})


test_that("Anomaly test - error when the parametre is a function", {
  # arrange
  f <- function(){return("I am a function")}
  # act & assert
  expect_error(variance(f), "^M must be a number/vector/matrix$")
})

