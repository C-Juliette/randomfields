#' Dataframe for the expected covariance
#'
#' @param sigma_Z the standard error of the theoritical distribution
#' @param the_radius the radius of the window of the moving average
#' @param the_directions the directions we consider in a list ex : list(c(0,1), c(1, 1))
#' @param the_scale how many km per pixels ?
#' @param direction TRUE by default. reduce the direction into its director vector
#'
#' @return A dataframe with seven colomns : the distance between two points in pixels unit, this distance in km unit,
#' the value of the expected covariance and expected correlation
#'  the considered direction, and the radius of the window of the moving average (in pixels and then km unit).
#' @export
#'
#' @examples
#' data_frame_expected_cov(1, c(1), list(c(0,1), c(1,1), c(1, 2)))
#'
data_frame_expected_cov <- function(sigma_Z, the_radius, the_directions, the_scale = 1, direction = TRUE){
  dframe <- NULL
  for (r in the_radius){
    for (dir in the_directions){
      if (direction == TRUE){dir <- direction(dir)}
      df <- expected_covariance(sigma_Z, r, dir, the_scale)
      df$Expected_correlation <- df$Expected_covariance/df$Expected_covariance[1]
      df$Direction <- stringr::str_c(as.character(dir[1]), as.character(dir[2]), sep = " - ")
      df$Radius_pixels <- r
      df$Radius_km <- the_scale*df$Radius_pixels
      if(is.null(dframe)){dframe <- df}#dframe <- df}
      else{dframe <- rbind(dframe, df)}
    }
  }
  return(dframe)
}
