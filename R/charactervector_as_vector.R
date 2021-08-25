#' charactervector_as_vector
#'
#' @param chaine Ex : x <- "(0, 1)" a string
#'
#' @return a vector Ex : c(0, 1)
#' @export
#'
#' @examples
#' charactervector_as_vector("(0, 1)")
#'
charactervector_as_vector <- function(chaine){
  res <- regmatches(chaine, gregexpr("[[:digit:]]+", chaine))
  res <- as.numeric(unlist(res))
return(res)
}
