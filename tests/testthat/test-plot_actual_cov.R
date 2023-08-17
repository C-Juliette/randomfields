test_that("typeof plot_actual_cov works", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)), max = 20))[1], "list")
})
