#' Variance
#'
#' Returns the biased variance (division by n, not by (n-1))
#'
#' @param M a number, a vector or a matrix, with or without NA
#'
#' @return a float (variance )
#' @export
#'
#' @examples
#' variance(c(1, 2, 3))
#' variance (matrix(rbinom(2*2, 1, 0.5), nrow = 2))
#'
variance <- function(M){
  if (!is.vector(M) & !is.matrix(M)){cli::cli_abort("M must be a number/vector/matrix")}
  if (!is.numeric(M)){cli::cli_abort("M must contain numeric values")}
  return(mean(M**2, na.rm = TRUE) - mean(M, na.rm = TRUE)**2)
}
