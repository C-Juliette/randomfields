test_that("max (1) correlation when superposed state works", {
  expect_equal(data_frame_expected_cov(1, c(1), list(c(0,1), c(1,1), c(1, 2)))[[1]][2], 1)
})
