#' charactervector_as_vector
#'
#' @param x Ex : x <- "(0, 1)" a string
#'
#' @return a vector Ex : c(0, 1)
#' @export
#'
#' @examples
#' charactervector_as_vector("(0, 1)")
#'
charactervector_as_vector <- function(x){
  chaine_splitee <- strsplit(x,"")
  return(c(as.numeric(chaine_splitee[[1]][2]),as.numeric(chaine_splitee[[1]][5])))}
