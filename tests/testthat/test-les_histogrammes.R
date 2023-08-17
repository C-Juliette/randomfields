test_that("typeof histograms works", {
  expect_equal(typeof(les_histogrammes(matrix(1:100, nrow = 10), matrix(101:200, nrow = 10))), "list")
})
