#' Moving average
#'
#' Create a new map/matrix by doing a moving average on a matrix
#'
#' @param Z matrix/dataframe
#' @param r integer : the radius of the window of the moving average. Ex : r = 1 implies a window of 3*3
#'
#' @return matrix (side effects imply a smaller matrix than the Z matrix passed in parametre)
#' @export
#'
#' @examples
#' moving_average(matrix(rnorm(10*10, 0,1), nrow = 10), r=2)
moving_average <- function(Z, r){
  if (!is.atomic(r)){cli::cli_abort("r must be a scalar")}
  if (!is.numeric(r)){cli::cli_abort("r must be an integer")}
  if (!iswholenumber(r)){cli::cli_abort("r must be an integer")}
  if (r < 0 ){cli::cli_abort("r must be >= 0")}
  if (!is.numeric(Z)){cli::cli_abort("Z must be a matrix of numbers")}
  Z <- as.matrix(Z)
  if ( 2*r+1 > dim(Z)[1] | 2*r+1 > dim(Z)[2]){cli::cli_abort("The grid must be larger than the window. Decrease r or increase the grid size")}


  nrowsZ <- dim(Z)[1]
  ncolsZ <- dim(Z)[2]
  nrowsY <- nrowsZ - 2*r
  ncolsY <- ncolsZ - 2*r

  Y <- matrix(data = rep(0, nrowsY*ncolsY), nrow = nrowsY)

  for(i in (r+1):(nrowsZ-r)){
    for(j in (r+1):(ncolsZ-r)){
      window <- Z[(i-r):(i+r), (j-r):(j+r)]
      Y[i-r,j-r] <- mean(window)
    }
  }
  return (Y)

}

