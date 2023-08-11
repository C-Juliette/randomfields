#' gen_norm
#'
#' @param l side lenght
#'
#' @return a matrix with random values
#' @export
#'
#' @examples
#' gen_norm(10)
gen_norm <- function(l) {
  return(matrix(stats::rnorm(1:(l*l)), nrow = l))
}


