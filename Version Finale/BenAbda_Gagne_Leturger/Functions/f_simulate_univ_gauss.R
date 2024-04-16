f_simulate_univ_gauss <- function(n_sim, n_days, mean, sd, lastprice){
  ## Simule des prix selon une distribution gaussienne univariée
  
  # ENTRÉES :
  #      n_sim    : [numeric] nombre de simulations à exécuter
  #      n_days   : [numeric] nombre de jours à simuler
  #      mean     : [numeric] moyenne de la distribution gaussienne
  #      sd       : [numeric] écart-type de la distribution gaussienne
  #      lastprice: [numeric] dernier prix connu de l'actif
  
  # SORTIES :
  #      rets    : [vector] un vecteur de rendements générés par la simulation
  #      prices  : [vector] un vecteur de prix simulés pour l'actif sur le nombre de jours donné
  
  rets <- matrix(NA, nrow = n_sim)  # Initialisation du vecteur de rendements
  prices <- foreach(i = 1:n_sim, .combine = 'c') %do%  {
    # rnorm génère des rendements normalement distribués supposés indépendants et identiquement distribués
    rets[i] <- exp(sum(rnorm(n_days, mean = mean, sd = sd))) - 1  # Calcul des rendements cumulés
    (1 + rets[i]) * lastprice  # Calcul des prix en appliquant le rendement au dernier prix
  }
  return(list(rets, prices))  # Retourne les rendements et les prix simulés
}
