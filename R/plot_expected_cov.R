ggplot <- ggplot2::ggplot
aes <- ggplot2::aes
geom_point <- ggplot2::geom_point
geom_line <- ggplot2::geom_line
scale_fill_viridis_c <- ggplot2::scale_fill_viridis_c
labs <- ggplot2::labs
theme <- ggplot2::theme
scale_x_continuous <- ggplot2::scale_x_continuous
margin <- ggplot2::margin
#' Title
#'
#' @param sigma_Z the standard error
#' @param the_radius radius of the window of the moving average (many are possibles ex : c(1, 2, 3))
#' @param the_directions ex : list(c(0,1), c(1,1))
#' @param xlabs the label of x axis
#' @param ylabs the label of y axis
#' @param x A string : "Distance" by default, or "Distance_km"
#' @param y A string : ""Expected covariance"" by default, or "Expected correlation
#' @param rayon A string for the unit of radius : "Radius" by default, or "Radius_km"
#' @param the_scale how many km/pixel
#' @param max the maximum value for x axis (the minimum is always zero)
#' @param maxy_sup the maximum value for y axis
#' @param maxy_inf the minimum value for y axis
#' @param connect TRUE by default : to connect points for a given radius and direction
#' @param director_vector TRUE by default : the given direction is transformed : it takes the director vector
#'
#' @return a plot of the expected covariance/correlation according to the distance
#' @export
#'
#' @examples
#' plot_expected_cov(1, c(1,2,3), list(c(0,1), c(1,1), c(1,2)))
plot_expected_cov <- function(sigma_Z, the_radius, the_directions, xlabs ="Distance between variables", ylabs ="Theoritical covariance",  x = "Distance", y = "Theoritical_covariance", rayon = "Radius",  the_scale = 1,  max = "", maxy_sup ="", maxy_inf="", connect = TRUE, director_vector = TRUE){
  df <- data_frame_expected_cov(sigma_Z, the_radius, the_directions, the_scale)
  colnames(df) <- c("Distance", "Distance_km", "Theoritical_covariance", "Theoritical_correlation", "Direction", "Radius", "Radius_km")
  ################COVARIANCE###################@
  df$Radius <- as.factor(df$Radius)
  df$Radius_km <- as.factor(df$Radius_km)
  if(y == "Theoritical_covariance"){
    if(x== "Distance"){p <- ggplot(data = df, aes(x = Distance, y = Theoritical_covariance))}
    if(x== "Distance_km"){p <- ggplot(data = df, aes(x = Distance_km, y = Theoritical_covariance))}
    if (rayon == "Radius"){p <- p + geom_point(aes(col = Radius, shape = Direction))}
    if (rayon == "Radius_km"){p <- p + geom_point(aes(col = Radius_km, shape  = Direction))}

    if(connect == TRUE){
      if (rayon == "Radius"){p <- p + geom_line(aes(col = Radius, group = interaction(Direction, Radius)))}
      if (rayon == "Radius_km"){p <- p + geom_line(aes(col = Radius_km, group = interaction(Direction, Radius_km)))}
    }



    #return(p)
  }
  ###########CORRELATION###############
  if(y == "Theoritical_correlation"){
    if(x== "Distance"){p <- ggplot(data = df, aes(x = Distance, y = Theoritical_correlation))}
    if(x== "Distance_km"){p <- ggplot(data = df, aes(x = Distance_km, y = Theoritical_correlation))}
    if (rayon == "Radius"){p <- p + geom_point(aes(shape = Direction, col = factor(Radius)))}
    if (rayon == "Radius_km"){p <- p + geom_point(aes(shape = Direction, col = factor(Radius_km_unit)))}

    if(connect == TRUE){
      if (rayon == "Radius"){p <- p + geom_line(aes(col = factor(Radius)))}
      if (rayon == "Radius_km"){p <- p + geom_line(aes(col = factor(Radius_km_unit)))}
    }

  }

  p <- p +
    viridis::scale_color_viridis(discrete = TRUE, option = "B")+
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  if (max != ""){p <- p + scale_x_continuous(breaks = pretty_breaks()) + xlim(min=0, max = max)
  }
  if (maxy_sup != ""){p <- p +  ylim(min = maxy_inf, max = maxy_sup) }
  p <- p +
    guides(col = ggplot2::guide_legend("Radius r \n of the \n moving average's \n window"))+
    labs(x = xlabs, y = ylabs)
  gg_ply <- plotly::ggplotly(p) #|>
  #     #plotly::layout(legend = list(title=list(text="Radius r \n of the \n moving average's \n window"))) |>
  #     plotly::layout(legend = list(orientation = "v", x = 0.6, y = 0.99))
  gg_ply <- plotly::hide_legend(gg_ply)

  return(gg_ply)
}
