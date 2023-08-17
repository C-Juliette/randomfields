test_that("typeof plot_actual_cov works 1", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)), max = 20))[1], "list")
})


test_that("typeof plot_actual_cov works 2", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)),
                                                                                           relier = TRUE, max = 20))[1], "list")
})

test_that("typeof plot_actual_cov works 3", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)),
                                      x = "Distance", max = 20))[1], "list")
})

test_that("typeof plot_actual_cov works 4", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)),
                                      x = "Distance_km", max = 20))[1], "list")
})

test_that("typeof plot_actual_cov works 5", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)),
                                      rayon = "Radius", max = 20))[1], "list")
})

test_that("typeof plot_actual_cov works 6", {
  expect_equal(typeof(plot_actual_cov(matrix(rnorm(10000, 0, 1), nrow = 100), c(1,2), list(c(0,1), c(1,1)),
                                      rayon = "Radius_km", max = 20))[1], "list")
})
