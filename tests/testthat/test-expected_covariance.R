test_that("covariance works", {
  expect_equal(expected_covariance(1, 1, c(0, 1))[[3]][1], 0.1111111, tolerance=1e-3)
})
