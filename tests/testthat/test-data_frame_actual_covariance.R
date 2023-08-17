test_that("multiplication works", {
  expect_equal(data_frame_actual_covariance(matrix(rnorm(1:100), nrow = 10), c(1,2), list(c(0,1), c(1,1)))[[1]][1], 0)
})
