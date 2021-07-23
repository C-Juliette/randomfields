#' Actual correlation
#' @importFrom stats setNames
#' @param M a matrix
#' @param a_vector 2 coordinates vector which give a direction. The correlation and covariance according to the distance are calculated for this direction.
#' @param scale 1 km/pixel by default. This number of km by pixels can eventually be changed.
#' @param vect_dir true by default. It reshapes the direction Ex: direction c(4,6) is reshaped as c(2,3). c(1,0) is reshaped as c(0,1) because of symetry considerations.
#'
#' @return a dataframe, giving the distance between points, in km and in pixels, and the empirical values of correlation and covariance according to these distances.
#' Warning : the more the distance between points increases, the less the calculus of correlation and covariance is accurate. That's because, on a grid, there are less points far away from
#' each other than points close to each other. That's to say, there are less and less couples of points to do the calculation as the distance between them increases.
#' @export
#'
#' @examples
#' actual_correlation(matrix(rnorm(1:100), nrow = 10), c(1,2))
actual_correlation <- function(M, a_vector, scale = 1, vect_dir = T){
  #tests on M
  if(is.data.frame(M) | is.vector(M)){M <- as.matrix(M)}
  if(!is.numeric(M)){stop("M must contain a vector, matrix or dataframe")}
  #tests on a_vector
  if(!is.vector(a_vector)){stop("a_vector must contain a vector")}
  if(length(a_vector) != 2){stop("a_vector must have 2 coordinates")}
  if(!is.numeric(a_vector)){stop("a_vector must contain numbers")}
  #end of tests
  if(vect_dir){vect_dir <- direction(a_vector)}
  else{vect_dir <- a_vector}
  Distance_pixels <- c()
  Empirical_covariance <- c()
  Empirical_correlation <- c()
  h <- sqrt(vect_dir%*%vect_dir)
  h1 <- vect_dir[1]
  h2 <- vect_dir[2]
  dim1 <- dim(M)[1]
  dim2 <- dim(M)[2]
  i <- 0
  while (h1*i <= (dim1-1) & h2*i <= (dim2-1)){
    row <- h1*i
    column <- h2*i
    Distance_pixels <- append(Distance_pixels,h*i)
    back_grid <- M[(row+1):dim1,(column+1):dim2]
    sigma_back_grid <- sqrt(variance(back_grid))
    front_grid <- M[(1:(dim1-row)),1:(dim2-column)]
    sigma_front_grid <- sqrt(variance(front_grid))
    cov <- mean(back_grid*front_grid)-mean(back_grid)*mean(front_grid)
    cor <- cov/(sigma_back_grid*sigma_front_grid)
    Empirical_covariance <- append(Empirical_covariance,cov)
    Empirical_correlation <- append(Empirical_correlation,cor)
    i <- i+1
  }
  Distance_km <- Distance_pixels*scale
  result <- as.data.frame(cbind(Distance_pixels, Distance_km, Empirical_covariance, Empirical_correlation))
  if(sum(is.nan(as.matrix(result))) == 1){warning("the result contains infinity NaN")}
  return(result)
}

