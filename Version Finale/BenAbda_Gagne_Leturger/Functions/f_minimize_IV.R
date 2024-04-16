f_minimize_IV <- function (m, tau, alpha, market_IV){
  ## Calcule la distance absolue entre les volatilités implicites du modèle et du marché
  
  # ENTRÉES :
  #   m          : [vector] vecteur (T x 1) de moneyness
  #   tau        : [vector] vecteur (T x 1) de temps jusqu'à maturité 
  #   alpha      : [vector] ensemble de quatre paramètres (4 x 1) pour le modèle
  #   market_IV  : [vector] vecteur (T x 1) de la surface de volatilité implicite du marché
  
  # SORTIES :
  #   dist       : [numeric] distance totale entre les surfaces de volatilité du modèle et du marché
  
  # Définition des paramètres
  alpha1 <- alpha[1]  # coefficient pour le terme constant
  alpha2 <- alpha[2]  # coefficient pour le terme quadratique en moneyness
  alpha3 <- alpha[3]  # coefficient pour le terme cubique en moneyness
  alpha4 <- alpha[4]  # coefficient pour le terme en racine carrée du temps jusqu'à maturité
  
  # Calcul de la volatilité implicite du modèle
  model_IV <- alpha1 +
    alpha2 * (m - 1) ^ 2 +
    alpha3 * (m - 1) ^ 3 +
    alpha4 * sqrt(tau)
  
  # Calcul de la distance entre les volatilités implicites du modèle et du marché
  dist <- sum(abs(model_IV - market_IV))
  
  # Retour de la distance
  return(dist)
}
