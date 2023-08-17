test_that("multiplication works", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2))))[1], "list")
})
