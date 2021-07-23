#' lLs histogrammes
#'
#' @param Z a matrix
#' @param Y another matrix
#' @param titre title if histograms
#' @param nom_axeX x label
#' @param nom_axeY y label
#'
#' @return a plot with 2 histograms
#' @export
#'
#' @examples
#' les_histogrammes(matrix(1:100, nrow = 10), matrix(101:200, nrow = 10))
les_histogrammes <- function(Z,Y, titre = 'Histogrammes', nom_axeX = "", nom_axeY = ""){
  Z <- as.vector(Z)
  Z <- as.data.frame(Z)
  Y <- as.vector(Y)
  Y <- as.data.frame(Y)

  p <- ggplot() +
    ggplot2::geom_histogram(data = Y, aes(x = Y, y=..count../sum(..count..)), color="black", fill= "#F8766D")+
    ggplot2::geom_histogram(data = Z, aes(x = Z, y=..count../sum(..count..)), color="black", fill="#01BDC2", alpha=0.5)+

    labs(title = titre,
         x = nom_axeX,
         y = nom_axeY)

  return(p)
}
