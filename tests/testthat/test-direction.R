########################## REGULAR TESTS #######################

test_that("Regular test - c(0, 1) becomes c(1, 0)", {
  # arrange
  # act
  actual <- direction(c(1,0))
  # assert
  expected <- c(0,1)
  expect_equal(actual, expected)
})


test_that("Regular test - c(5, 10) becomes c(1, 2)", {
  # arrange
  # act
  actual <- direction(c(5, 10))
  # assert
  expected <- c(1,2)
  expect_equal(actual, expected)
})

test_that("Regular test - c(10, 10) becomes c(1, 1)", {
  # arrange
  # act
  actual <- direction(c(10, 10))
  # assert
  expected <- c(1,1)
  expect_equal(actual, expected)
})

test_that("Regular test - c(0, 0) stays c(0, 0)", {
  # arrange
  # act
  actual <- direction(c(0, 0))
  # assert
  expected <- c(0,0)
  expect_equal(actual, expected)
})

test_that("Regular test - c(0.5, 0.5) becomes c(1, 1)", {
  # arrange
  # act
  actual <- direction(c(0.5, 0.5))
  # assert
  expected <- c(1,1)
  expect_equal(actual, expected)
})

test_that("Regular test - c(0.5, 0.5) becomes c(1, 1)", {
  # arrange
  # act
  actual <- direction(c(0.5, 0.5))
  # assert
  expected <- c(1,1)
  expect_equal(actual, expected)
})

test_that("Regular test - c(0, 10) becomes c(0, 1)", {
  # arrange
  # act
  actual <- direction(c(0, 10))
  # assert
  expected <- c(0,1)
  expect_equal(actual, expected)
})

test_that("Regular test - c(-4, 2) becomes c(1, 2)", {
  # arrange
  # act
  actual <- direction(c(-4, 2))
  # assert
  expected <- c(1,2)
  expect_equal(actual, expected)
})

