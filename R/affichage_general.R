#' Affichage matrices boxplots et histogrammes
#'
#' @param Z matrice
#' @param r rayon de lissage
#' @param titre titre du graphique
#' @param paletteinf to fix color on the scale
#' @param palettesup to fix color on the scale
#' @param mmechelle to set the two same scales TRUE/FALSE
#'
#' @return a plot
#' @export
#'
#' @examples
#' affichage_general(matrix(rnorm(100), nrow = 10), r =2)
affichage_general <- function(Z, r, titre = "",paletteinf = "", palettesup = "", mmechelle = FALSE){
  Y <- moving_average(Z,r)
  p1 <- plot_matrix(Z, titre =  "Z grid",paletteinf = paletteinf, palettesup = palettesup) + ggplot2::coord_cartesian(xlim = c(0, dim(Z)[1]), ylim = c(0, dim(Z)[2]))
  if(mmechelle == FALSE){p2 <- plot_matrix(Y,  r, titre = "Y grid", paletteinf = paletteinf, palettesup = palettesup)+ ggplot2::coord_cartesian(xlim = c(0, dim(Z)[1]), ylim = c(0, dim(Z)[2]))}
  else if(mmechelle == TRUE){
    paletteinfY <- min(Z)
    palettesupY <- max(Z)
    p2 <- plot_matrix(Y,  r, titre = "Y grid", paletteinf = paletteinfY, palettesup = palettesupY)+ ggplot2::coord_cartesian(xlim = c(0, dim(Z)[1]), ylim = c(0, dim(Z)[2]))
    }
  p3 <- les_histogrammes(Z, Y, titre = "Histograms")
  p4 <- les_boxplots(Z, Y)

  title <- cowplot::ggdraw() + cowplot::draw_label(titre, fontface='bold')

  p <- cowplot::plot_grid(p1,p2,p3,p4, ncol=2, nrow=2) +
  scale_fill_viridis_c(option = "B", direction = -1)
  p
}
