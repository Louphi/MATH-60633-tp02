f_plot_sim_univ_gauss <- function(returns, prices) {
  ## Trace des histogrammes des rendements simulés et des prix de l'S&P 500
  ## basés sur un modèle gaussien univarié
  
  # ENTRÉES :
  #    returns: un vecteur de rendements cumulatifs sur une semaine simulés
  #    prices: un vecteur de prix projetés à une semaine simulés
  
  # SORTIES :
  #   Histogrammes des rendements simulés et des prix
  
  # Création de l'histogramme des rendements
  ret <- ggplot(data = data.frame(returns),
                aes(x = returns)) +
    geom_histogram(bins = 100, color="black", fill="gray") +
    geom_vline(aes(xintercept = mean(returns), color = "Moyenne des rendements cumulés")) +
    geom_vline(aes(xintercept = quantile(returns, probs = 0.05), color = "Percentiles 5% & 95% des rendements cumulés")) +
    geom_vline(aes(xintercept = quantile(returns, probs = 0.95), color = "Percentiles 5% & 95% des rendements cumulés")) +
    scale_color_manual(values = c("blue", "red"),
                       labels = c("Percentiles 5% & 95% des rendements cumulés", "Moyenne des rendements cumulés")) +
    labs(x = "Rendement Cumulatif", y = "Fréquence", color = "Légende",
         title = "Rendements Cumulatifs Simulés sur Une Semaine de l'S&P 500 - Modèle Gaussien Univ.") +
    theme(plot.title = element_text(hjust = 0.5))  # Centrage du titre
  print(ret)
  
  # Création de l'histogramme des prix
  price <- ggplot(data = data.frame(prices),
                  aes(x = prices)) +
    geom_histogram(bins = 100, color="black", fill="gray") +
    geom_vline(aes(xintercept = mean(prices), color = "Prix Moyen")) +
    geom_vline(aes(xintercept = quantile(prices, probs = 0.05), color = "Percentiles 5% & 95% des prix")) +
    geom_vline(aes(xintercept = quantile(prices, probs = 0.95), color = "Percentiles 5% & 95% des prix")) +
    scale_color_manual(values = c("blue", "red"),
                       labels = c("Percentiles 5% & 95% des prix", "Prix Moyen")) +
    labs(x = "Prix", y = "Fréquence", color = "Légende",
         title = "Prix Projetés Simulés sur Une Semaine de l'S&P 500 - Modèle Gaussien Univ.") +
    theme(plot.title = element_text(hjust = 0.5))  # Centrage du titre
  print(price)
}
