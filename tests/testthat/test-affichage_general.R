test_that("typeof works", {
  expect_equal(typeof(affichage_general(matrix(rnorm(100), nrow = 10), r =2))[1], "list")
})
