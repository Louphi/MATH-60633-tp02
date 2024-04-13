f_parametric_surface <-function (m, tau, alpha){
  ## Calcule la volatilité implicite
  
  # ENTRÉES
  #   m         : [vector] vecteur (T x 1) de moneyness
  #   tau       : [vector] vecteur (4 x 1) de temps jusqu'à maturité 
  #   alpha     : [vector] ensemble de quatre paramètres (4 x 1)
  
  # SORTIES
  #   model_IV  : [vector] vecteur (T x 1) de la surface de volatilité
  
  # Extraction des quatre paramètres du vecteur alpha 
  alpha1 <- alpha[1] # coefficient pour le terme constant
  alpha2 <- alpha[2] # coefficient pour le terme quadratique en moneyness
  alpha3 <- alpha[3] # coefficient pour le terme cubique en moneyness
  alpha4 <- alpha[4] # coefficient pour le terme en racine carrée du temps jusqu'à maturité
  
  # Calcul de la volatilité implicite utilisant la formule de surface paramétrique
  model_IV <- alpha1 +
    alpha2 * (m - 1) ^ 2 +
    alpha3 * (m - 1) ^ 3 +
    alpha4 * sqrt(tau)
  
  # Retour de la volatilité implicite du modèle
  return(model_IV) 
}
