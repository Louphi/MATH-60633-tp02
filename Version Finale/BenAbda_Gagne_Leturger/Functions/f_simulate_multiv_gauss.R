f_simulate_multiv_gauss <- function(n_sim, n_days, mu, sigma, lastprice1, lastprice2){
  ## Simule des prix selon une distribution gaussienne multivariée
  
  # ENTRÉES :
  #      n_sim    : [numeric] nombre de simulations à exécuter
  #      n_days   : [numeric] nombre de jours à simuler
  #      mu       : [numeric] vecteur des moyennes de la distribution gaussienne
  #      sigma    : [numeric] matrice de covariance de la distribution gaussienne
  #      lastprice1: [numeric] dernier prix connu de l'actif 1
  #      lastprice2: [numeric] dernier prix connu de l'actif 2
  
  # SORTIES :
  #      sim1     : [vector] un vecteur de prix simulés pour l'actif 1 sur le nombre de jours donné
  #      sim2     : [vector] un vecteur de prix simulés pour l'actif 2 sur le nombre de jours donné
  
  sim1 <- rep(NA, n_sim)  # Initialisation du vecteur pour les prix simulés de l'actif 1
  
  sim2 <- foreach(i = 1:n_sim, .combine = 'c') %do% {
    simulated_joint <- mvrnorm(n_days, mu = mu, Sigma = sigma)  # Génération des rendements conjoints
    
    sim1[i] <- exp(sum(simulated_joint[,1])) * lastprice1  # Calcul du prix final pour l'actif 1
    exp(sum(simulated_joint[,2])) * lastprice2  # Calcul du prix final pour l'actif 2
  }
  return(list(sim1, sim2))  # Retourne les prix simulés pour les deux actifs
}
