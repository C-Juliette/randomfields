#' Expected covariance
#'
#' @param sigma_Z the standard error of the theoritical distribution
#' @param r the radius of the moving average window
#' @param the_direction the direction we consider ex : c(1, 1)
#' @param the_scale how many km/pixels
#'
#' @return the covariance according to the distance for a given direction
#' @export
#'
#' @examples
#' expected_covariance(1, 1, c(0, 1))
#'
#'
expected_covariance <- function(sigma_Z, r, the_direction, the_scale = 1){
  c <- 2*r+1 #cote de la fenetre
  i <- 0
  H1 <- c()
  H2 <- c()
  Distance_pixels <- c()
  dir_vect <- direction(the_direction)
  h <- sqrt(dir_vect%*%dir_vect)
  h1 <- dir_vect[1]
  h2 <- dir_vect[2]
  while (h1*i <= c & h2*i <= c){
    H1 <- append(H1,h1*i)
    H2 <- append(H2,h2*i)
    Distance_pixels <- append(Distance_pixels,h*i)
    i <- i+1
  }
  Expected_covariance <- (sigma_Z**2)*(c**2 - (H1+H2)*c +H1*H2)/ c**4
  for (j in i:(i+5)){
    Distance_pixels <- append(Distance_pixels,h*j)
    Expected_covariance <- append(Expected_covariance,0)
  }
  Distance_km <- Distance_pixels*the_scale
  result <- as.data.frame(cbind(Distance_pixels, Distance_km, Expected_covariance))
  return(result)
}
