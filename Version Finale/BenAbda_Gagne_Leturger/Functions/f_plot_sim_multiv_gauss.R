f_plot_sim_multiv_gauss <- function(prices, vix) {
  ## Trace des histogrammes des prix simulés de l'S&P 500 et du VIX
  ## basés sur un modèle gaussien multivarié
  
  # ENTRÉES :
  #    prices: un vecteur de prix projetés à une semaine simulés
  #    vix   : un vecteur de valeurs simulées du VIX à une semaine
  
  # SORTIES :
  #   Histogrammes des prix et des valeurs du VIX simulés
  
  # Histogramme des prix de l'S&P 500
  price <- ggplot(data = data.frame(prices),
                  aes(x = prices)) +
    geom_histogram(bins = 100, color="black", fill="gray") +  # Histogramme des prix
    geom_vline(aes(xintercept = mean(prices), color = "Prix Moyen")) +  # Ligne verticale pour la moyenne
    geom_vline(aes(xintercept = quantile(prices, probs = 0.05), color = "Percentiles 5% & 95% des prix")) +  # Ligne verticale pour le 5ème percentile
    geom_vline(aes(xintercept = quantile(prices, probs = 0.95), color = "Percentiles 5% & 95% des prix")) +  # Ligne verticale pour le 95ème percentile
    scale_color_manual(values = c("blue", "red"),
                       labels = c("Percentiles 5% & 95% des prix", "Prix Moyen")) +
    labs(x = "Prix", y = "Fréquence", color = "Légende",
         title = "Prix Projetés Simulés sur Une Semaine de l'S&P 500 - Modèle Gaussien Multiv.") +
    theme(plot.title = element_text(hjust = 0.5))
  print(price)
  
  # Histogramme des valeurs du VIX
  vix_plot <- ggplot(data = data.frame(vix),
                     aes(x = vix)) +
    geom_histogram(bins = 100, color="black", fill="gray") +  # Histogramme du VIX
    geom_vline(aes(xintercept = mean(vix), color = "Valeur Moyenne")) +  # Ligne verticale pour la moyenne
    geom_vline(aes(xintercept = quantile(vix, probs = 0.05), color = "Percentiles 5% & 95% des valeurs")) +  # Ligne verticale pour le 5ème percentile
    geom_vline(aes(xintercept = quantile(vix, probs = 0.95), color = "Percentiles 5% & 95% des valeurs")) +  # Ligne verticale pour le 95ème percentile
    scale_color_manual(values = c("blue", "red"),
                       labels = c("Percentiles 5% & 95% des valeurs", "Valeur Moyenne")) +
    labs(x = "Valeur", y = "Fréquence", color = "Légende",
         title = "Valeurs Simulées sur Une Semaine du VIX - Modèle Gaussien Multiv.") +
    theme(plot.title = element_text(hjust = 0.5))
  print(vix_plot)
}
