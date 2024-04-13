f_Get_Book_values_VolSurf <- function(S, book, vol, n_sim){
  ### Calcule les valeurs d'un portefeuille d'options en utilisant la formule de Black-Scholes en supposant un modèle de surface paramétrique
  
  # ENTRÉES :
  #  S    : [numeric] prix actuel de l'action
  #  book : [matrix] matrice (n x 4) des contrats d'options dans le portefeuille, où n est le nombre
  #         de contrats, et chaque ligne contient des informations sur le prix d'exercice (K), le temps avant expiration (tau),
  #         le taux d'intérêt sans risque (r)
  #  vol  : [numeric] volatilité implicite de l'action sous-jacente pour chaque simulation
  #  n_sim: nombre de simulations à effectuer
  
  # SORTIES :
  #  book_values : [vector] vecteur (n x 1) des valeurs de chaque contrat d'option dans le portefeuille
  
  # Initialisation de la matrice des prix des options pour chaque simulation
  book_prices <- matrix(NA, nrow = n_sim, ncol = ncol(book))
  
  for (i in 1:ncol(book)) {
    book_prices[, i] <- blackscholes(callput = 1,
                                     S0 = S,
                                     K = book$K[i],
                                     r = book$r[i],
                                     time = book$tau[i],
                                     vola = vol[i,])$Price  # Application de la formule de Black-Scholes pour chaque contrat
  }
  # Calcul des valeurs du portefeuille en sommant les prix de chaque option à travers les simulations
  book_values <- apply(book_prices, 1, sum)
  return(book_values) # Retourne les valeurs du portefeuille
}
