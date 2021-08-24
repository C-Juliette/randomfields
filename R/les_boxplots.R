#' Boxplots
#'
#' @param Z first matrix
#' @param Y second matrix
#' @param titre title of the graph
#' @param nom_axeX x label
#' @param nom_axeY y label
#'
#' @return a plot with the boxplots
#' @export
#'
#' @examples
#' les_boxplots(1:10, 10:20)
les_boxplots <- function(Z, Y, titre = "Boxplots", nom_axeX = "", nom_axeY = ""){
  Z <- as.vector(Z)
  Y <- as.vector(Y)
  dataZ <- data.frame(Grille = rep("Z", length(Z)), Valeurs = Z)
  dataY <- data.frame(Grille = rep("Y", length(Y)), Valeurs = Y)
  data <- rbind(dataZ, dataY)
  data$Grille <- factor(data$Grille , levels=c("Z", "Y"))

  p <- ggplot(data, aes(x = Grille, y = Valeurs, fill = Grille))+
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(fun = "mean", color = "blue")+
    ggplot2::labs(title = titre,
         x = nom_axeX,
         y = nom_axeY)
  p
}
