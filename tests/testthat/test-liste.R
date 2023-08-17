test_that("list of directions works", {
  expect_equal(liste(c("(0, 1)", "(1, 1)"))[[1]][1], 0)
  expect_equal(liste(c("(0, 1)", "(1, 1)"))[[1]][2], 1)
  expect_equal(liste(c("(0, 1)", "(1, 1)"))[[2]][1], 1)
  expect_equal(liste(c("(0, 1)", "(1, 1)"))[[2]][2], 1)
})
