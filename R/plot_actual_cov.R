#' plot_actual_covariance
#'
#' @param Z a matrix
#' @param les_rayons radius of the moving average (vector)
#' @param les_directions directions (list)
#' @param ylabs set y legend
#' @param xlabs set x legend
#' @param x x parametre
#' @param y y parametre
#' @param rayon "Radius" or "Rayon_km" (radius unit)
#' @param echelle scale
#' @param max set x max
#' @param maxy_sup set y max
#' @param maxy_inf set y min
#' @param relier connect points
#' @param vecteur_directeur TRUE/FALSE : ex : c(2,4) become c(1,2)
#'
#' @return a plot
#' @export
#'
#' @examples
#' plot_actual_cov(matrix(rnorm(1:50), nrow = 10), c(1,2), list(c(0,1), c(1,1)), max = 20)
plot_actual_cov <- function(Z,les_rayons,les_directions,  ylabs = "", xlabs ="Distance between variables", x = "Distance", y = "Empirical_covariance", rayon = "Radius",  echelle = 1,  max = "", maxy_sup = "", maxy_inf = "" , relier = TRUE,vecteur_directeur = TRUE){
  df <- data_frame_actual_covariance(Z, les_rayons, les_directions, echelle)
  colnames(df) <- c("Distance", "Distance_km", "Empirical_covariance", "Empirical_correlation", "Direction", "Radius", "Rayon_km")

  ################COVARIANCE###################@
  if(y == "Empirical_covariance"){
    if(x == "Distance"){p <- ggplot(data = df, aes(x = Distance, y = Empirical_covariance))}
    if(x == "Distance_km"){p <- ggplot(data = df, aes(x = Distance_km, y = Empirical_covariance))}
    if (rayon == "Radius"){p <- p + geom_point(aes(group = interaction(Direction, Radius), col = factor(Radius)))}
    if (rayon == "Rayon_km"){p <- p + geom_point(aes(shape=Direction, col = factor(Rayon_km)))}

    if(relier == TRUE){
      if (rayon == "Radius"){p <- p + geom_line(aes(group = interaction(Direction, Radius), col = factor(Radius)))}
      if (rayon == "Rayon_km"){p <- p + geom_line(aes(group = interaction(Direction, Radius), col = factor(Rayon_km)))}
    }
  }
  ###########CORRELATION###############
  if(y == "Empirical_correlation"){
    if(x == "Distance"){p <- ggplot(data = df, aes(x = Distance, y = Empirical_correlation))}
    if(x == "Distance_km"){p <- ggplot(data = df, aes(x = Distance_km, y = Empirical_correlation))}
    if (rayon == "Radius"){p <- p + geom_point(aes(shape = Direction, col = factor(Radius)))}
    if (rayon == "Rayon_km"){p <- p + geom_point(aes(shape = Direction, col = factor(Rayon_km)))}

    if(relier == TRUE){
      if (rayon == "Radius"){p <- p + geom_line(aes(shape=Direction, col = factor(Radius)))}
      if (rayon == "Rayon_km"){p <- p + geom_line(aes(shape=Direction, col = factor(Rayon_km)))}
    }
  }

  p <- p +
    viridis::scale_color_viridis(discrete = TRUE, option = "B")
    #scale_shape(solid = T)+
    if (max != ""){p <- p +  ggplot2::xlim(min=0, max = max) }
   if (maxy_sup != ""){p <- p +  ylim(min=maxy_inf, max = maxy_sup) }
    p <- p + labs(x = xlabs ,
         y = ylabs,
         shape = "Direction",
         color = rayon)+
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    ) +
      guides(col = ggplot2::guide_legend("Radius r \n of the \n moving average's \n window"))
    if(length(les_rayons) == 1){
       if(les_rayons == 0){
      p <- p + guides(col = "none")}
    }
  gg_ply <- plotly::ggplotly(p) |>
    plotly::layout(legend = list(title=list(text="Radius r \n of the \n moving average's \n window"))) |>
    plotly::layout(legend = list(orientation = "v", x = 0.6, y = 0.99))
  return(gg_ply)
}
