######################### REGULAR TESTS #######################
test_that("Regular test - Z a matrix", {
  # arrange
  Z <- matrix(1:4, nrow = 2)
  Var1 <- c(1, 2, 1, 2)
  Var2 <- c(1, 1, 2, 2)
  value <- c(1, 2, 3, 4)
  # act
  actual <- long_shaped_matrix(Z)
  # assert
  expected <- data.frame(Var1, Var2, value)
  expect_equal(actual, expected)
})


test_that("Regular test - Z a dataframe", {
  # arrange
  Z <- as.data.frame(matrix(1:4, nrow = 2))
  Var1 <- c(1, 2, 1, 2)
  Var2 <- c(1, 1, 2, 2)
  value <- c(1, 2, 3, 4)
  # act
  actual <- long_shaped_matrix(Z)
  # assert
  expected <- data.frame(Var1, Var2, value)
  expect_equal(actual, expected)
})




test_that("Regular test - Z a number", {
  # arrange
  Z <- 1
  Var1 <- c(1)
  Var2 <- c(1)
  value <- c(1)
  # act
  actual <- long_shaped_matrix(Z)
  # assert
  expected <- data.frame(Var1, Var2, value)
  expect_equal(actual, expected)
})

test_that("Regular test - Z a vector", {
  # arrange
  Z <- c(1:4)

  Var1 <- c(1, 2, 3, 4)
  Var2 <- c(1, 1, 1, 1)
  value <- c(1, 2, 3, 4)
  # act
  actual <- long_shaped_matrix(Z)
  # assert
  expected <- data.frame(Var1, Var2, value)
  expect_equal(actual, expected)
})


test_that("Regular test - Z a string", {
  # arrange
  Z <- "a"
  Var1 <- c(1)
  Var2 <- c(1)
  value <- c("a")
  # act
  actual <- long_shaped_matrix(Z)
  # assert
  expected <- data.frame(Var1, Var2, value)
  expect_equal(actual, expected)
})

######################## ANOMALY TESTS #####################
test_that("Anomaly test - error when the parametre is not a matrix/dataframe/vector", {
  # arrange
  f <- function(){paste("I am a function")}
  # act & assert
  expect_error(long_shaped_matrix(f), "^Z must be a vector/matrix/dataframe$")
})





