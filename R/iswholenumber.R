#' iswholenumber
#'
#' @param x the number to test : is it an integer ?
#' @param tol tolerance
#'
#' @return TRUE/FALSE : test if x is an integer
#' @export
#'
#' @examples
#' iswholenumber(22)
iswholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}
