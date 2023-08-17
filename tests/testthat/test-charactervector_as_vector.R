test_that("char to double works", {
  expect_equal(typeof(charactervector_as_vector("(0, 1)")), "double")
})
