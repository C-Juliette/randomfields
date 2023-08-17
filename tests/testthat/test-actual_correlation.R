test_that("multiplication works", {
  expect_equal(actual_correlation(matrix(rnorm(1:100), nrow = 10), c(1,2))[[1]][1], 0)
})
