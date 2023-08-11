test_that("Dim of matrix ok", {
  set.seed(1)
  expect_equal(dim(gen_norm(10))[1], 10)
  expect_equal(dim(gen_norm(10))[2], 10)
})
