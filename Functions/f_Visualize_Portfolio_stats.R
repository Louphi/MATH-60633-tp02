f_Visualize_Portfolio_stats <- function(data, VaR, ES){
  ## Utilisée pour tracer un graphique montrant des statistiques importantes sur le portefeuille :
  ##                           1. Distribution
  ##                           2. Moyenne
  ##                           3. VaR (Valeur à Risque)
  ##                           4. Perte Espérée (Expected Shortfall)
  
  # ENTRÉES :
  #       data: [vector] un vecteur de bénéfices et pertes (P&L) simulés à une semaine d'un portefeuille
  #       VaR: [numeric] la Valeur à Risque à 95% du portefeuille
  #       ES: [numeric] la Perte Espérée à 95% du portefeuille
  
  # SORTIE :
  #       Un histogramme et les statistiques importantes
  
  plot <- ggplot(data = data.frame(data),
                 aes(x = data)) +
    geom_histogram(bins = 100, color="black", fill="gray") +  # Histogramme des P&L
    geom_vline(aes(xintercept = mean(data), color = "Moyenne P&L")) +  # Ligne verticale pour la moyenne
    geom_vline(aes(xintercept = VaR, color = "95% Valeur à Risque")) +  # Ligne verticale pour la VaR
    geom_vline(aes(xintercept = ES, color = "95% Perte Espérée")) +  # Ligne verticale pour l'ES
    scale_color_manual(values = c("red", "blue", "orange"),
                       labels = c("95% Perte Espérée", "95% Valeur à Risque", "Moyenne P&L")) +
    labs(x = "P&L", y = "Fréquence", color = "",
         title = "P&L Simulés à Une Semaine du Portefeuille") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)  # Affichage du graphique
}
