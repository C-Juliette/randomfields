ggplot <- ggplot2::ggplot
aes <- ggplot2::aes
geom_tile <- ggplot2::geom_tile
scale_fill_viridis_c <- ggplot2::scale_fill_viridis_c
labs <- ggplot2::labs
guides <- ggplot2::guides
guide_colorbar <- ggplot2::guide_colorbar
scale_y_continuous <- ggplot2::scale_y_continuous
scale_x_continuous <- ggplot2::scale_x_continuous
scale_y_reverse <- ggplot2::scale_y_reverse

#' Plot a matrix
#'
#' @param M a matrix/vector/number/dataframe of numbers
#' @param r a radius for the window of the moving average (optional)
#' @param paletteinf extreme color of color scale
#' @param palettesup extreme color of color scale
#' @param titre a title for the matrix
#' @param nom_axeX name for X axis
#' @param nom_axeY name for Y axis
#' @param echelle title of scale
#'
#' @return a plot
#' @export
#'
#' @examples
#' plot_matrix(matrix(rbinom(10*10, 1, 0.5), nrow = 10))
#' plot_matrix(matrix(rbinom(10*10, 1, 0.5), nrow = 10), r = 2)
plot_matrix <- function(M, r = "", paletteinf = "", palettesup = "", titre = "", nom_axeX = "", nom_axeY = "", echelle = "Scale"){
  # if (is.data.frame(M)){M <- as.matrix(M)}
  M <- t(M)
  if (!is.numeric(M)){stop("M must contain numbers")}

  if(titre == ""){titre <- stringr::str_c("Var = " ,as.character(round(variance(M), 4)),  sep = "")}
  else{titre <- stringr::str_c(titre, "  -  Var = " ,as.character(round(variance(M), 4)),  sep = "")}
  M <- M |> long_shaped_matrix()
  if(r == ""){p <- ggplot(data = M, aes(x = .data$Var1 - 0.5, y = .data$Var2 - 0.5))}
  else {p <- ggplot(M, aes(x = .data$Var1 - 0.5 + r , y = .data$Var2 - 0.5 + r))}
  p <- p +
    geom_tile(aes(fill = .data$value))
  if(paletteinf != "" & palettesup != ""){p <- p + scale_fill_viridis_c(option = "B", direction = -1, limits = c(paletteinf, palettesup))}
  else{p <- p + scale_fill_viridis_c(option = "B", direction = -1)}
  p <- p +
    labs(title = titre,
         x = nom_axeX,
         y = nom_axeY) +
    guides(fill = guide_colorbar(title = echelle))+
    scale_y_reverse(breaks = scales::pretty_breaks())+
    scale_x_continuous(breaks = scales::pretty_breaks(), position = "top")
  return(p)
}

