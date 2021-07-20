#' data_frame_actual_covariance
#'
#' @param Z the matrix
#' @param les_rayons radius of moving average window in a vector
#' @param les_directions list of direction vectors
#' @param echelle scale (optional)
#' @param vecteur_directeur set the direction with minimum numbers in the vector
#'
#' @return a dtaframe Distance_pixels Distance_km Empirical_covariance Empirical_correlation
#' @export
#'
#' @examples
#' data_frame_actual_covariance(matrix(rnorm(1:100), nrow = 10), c(1,2), list(c(0,1), c(1,1)))
data_frame_actual_covariance <- function(Z, les_rayons, les_directions, echelle = 1, vecteur_directeur = TRUE){
  datfr <- as.data.frame(setNames(replicate(4,numeric(0), simplify = F),c("Distance_pixels","Distance_km","actual_correlation","Correlation_empirique")))
  for (r in les_rayons){
    for (dir in les_directions){
      if (vecteur_directeur == TRUE){dir <- direction(dir)}
      Y <- moving_average(Z,r)
      df <- actual_correlation(Y, dir, echelle)
      df$Direction <- stringr::str_c(as.character(dir[1]), as.character(dir[2]), sep = " - ")
      df$Rayon_pixels <- r
      df$Rayon_km <- echelle*df$Rayon_pixels
      datfr <- rbind(datfr, df)
    }
  }
  return(datfr)
}
