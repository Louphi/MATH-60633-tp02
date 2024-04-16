f_display_copula_margins <- function(data, labels, lim1, lim2){
  ## Affiche le nuage de points et les marges
  
  # ENTRÉES
  #   data    : [vector] vecteur (n x 2) de deux séries temporelles marginales
  #   labels  : [vector] vecteur (2 x 1) d'étiquettes
  #   lim1    : [integer] limite de l'axe des ordonnées pour la première distribution
  #   lim2    : [integer] limite de l'axe des ordonnées pour la seconde distribution
  
  # SORTIES
  # Nuage de points et distribution des marginales
  n_bins <- round(sqrt(length(data[,1]))) # Calcul du nombre de bins pour l'histogramme
  
  # Configuration de la zone de tracé principale pour le nuage de points
  par(fig = c(0.4, 1, 0.4, 1), mar = c(.5, .5, 4, .5))
  plot(data,
       xlab = labels[1], ylab = labels[2],
       type = "p", pch = 20, cex = 0.6, tck = 0)
  
  # Configuration et tracé de l'histogramme pour la seconde variable
  par(fig = c(0.4, 1, 0, .3),mar = c(3, .5, .5, .5),new = TRUE)
  h2 <- hist(x = data[,2], breaks = n_bins, plot = FALSE)
  barplot(height = h2$counts,
          names.arg = round(h2$mids,1),
          xlab = "", ylab = "", ylim = rev(c(0, lim2)), tck = 0)
  box(which = "plot")
  
  # Configuration et tracé de l'histogramme pour la première variable
  par(fig = c(0, 0.38, 0.4, 1),mar = c(.5, 3, 4, 2),new = TRUE)
  h1 <- hist(x = data[,1], breaks = n_bins, plot = FALSE)
  barplot(height = h1$counts, names.arg = h1$mids,
          xlab = "", ylab = "",
          horiz = TRUE, xlim = rev(c(0, lim1)),
          yaxt = "n", tck = 0, col = "blue")
  
  # Ajout des quantiles sur l'axe des ordonnées de l'histogramme horizontal
  qtl <- round(quantile(x = data[,1], probs = c(0, 0.25, 0.5, 0.75, 1)), 1)
  axis(2, at = seq(from = 1, to = 80, length.out = 5),
       labels = qtl, padj = 0.2, las = 2, tck = 0)
  box(which = "plot")
  
  # Titre global pour l'ensemble des graphiques
  mtext("Marges et Jointure", side = 3, outer = TRUE,
        line = -3, font = 2, cex = 1.3)
  
}
