test_that("expected code typeof works 1", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2))))[1], "list")
})


test_that("expected code typeof works 2", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2)),
                                        x= "Distance_km",
                                        y = "Theoritical_correlation",
                                        connect = TRUE))[1], "list")
})

test_that("expected code typeof works 3", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2)),
                                        x= "Distance_km",
                                        y = "Theoritical_correlation",
                                        connect = TRUE))[1], "list")
})

test_that("expected code typeof works 4", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2)),
                                        y = "Theoritical_correlation",
                                        connect = TRUE))[1], "list")
})

test_that("expected code typeof works 5", {
  expect_equal(typeof(plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2)),
                                        y = "Theoritical_correlation",
                                        max = 2,
                                        maxy_sup = 2))[1], "list")
})
