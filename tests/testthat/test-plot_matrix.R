########################## SNAPSHOT TESTS #######################
# test_that("The plot is the right plot", {
#   M <- plot_matrix(matrix(rbinom(10*10, 1, 0.5), nrow = 10))
#   expect_snapshot("plot_matrix(M)")
# })
#
# test_that("The plot is the right plot", {
#   M <- plot_matrix(matrix(rbinom(10*10, 1, 0.5), nrow = 10), r = 2)
#   expect_snapshot("plot_matrix(M)")
# })
#
# test_that("The plot is the right plot", {
#   M <- matrix(rbinom(10*10, 1, 0.5), nrow = 10)
#   expect_snapshot("plot_matrix(M)")
# })

# test_that("The plot is the right plot", {
#   M <- matrix(1:100, nrow = 10)
#   vdiffr::expect_doppelganger("figuretest", plot_matrix(M))
# })

# IT DOES NOT WORK
# test_that("plots have known output", {
#   #Arrange
#   M <- matrix(1:100, nrow = 10)
#   Leplot <- function() plot_matrix(M)
#   #Act & Assert
#   vdiffr::expect_doppelganger("fichierplot", Leplot)
# })

######################## ANOMALY TESTS #####################

test_that("Anomaly test - error when M does not contain number - test 1", {
  # arrange
  M <- "a"
  # act & assert
  expect_error(plot_matrix(M), "^M must contain numbers$")
})


test_that("Anomaly test - error when M does not contain number - test 2", {
  # arrange
  M <- c("a")
  # act & assert
  expect_error(plot_matrix(M), "^M must contain numbers$")
})
