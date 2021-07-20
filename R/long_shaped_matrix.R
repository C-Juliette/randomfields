#' Long shaped matrix
#'
#' @param Z a matrix/dataframe/vector
#'
#' @return a dataframe : first column is x coordinates, second column is y coordinate, third column is the value
#' @export
#'
#' @examples
#' Z <- matrix(1:25, nrow = 5)
#' long_shaped_matrix(Z)

long_shaped_matrix <- function(Z){
  if (!is.vector(Z) & !is.matrix(Z) & !is.data.frame(Z)){stop("Z must be a vector/matrix/dataframe")}
  Z <- as.matrix(Z)
  nrows <- dim(Z)[1]
  ncols <- dim(Z)[2]
  value <- as.vector(Z)
  Var1 <- rep(c(1:nrows), ncols)
  Var2 <- c()
  for (i in 1:ncols){
    Var2 <- append(Var2, rep(i, nrows))}
  Z <- data.frame(Var1, Var2, value)
  return(Z)
}
