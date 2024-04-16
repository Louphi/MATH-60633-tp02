f_copula_cov_mat <- function(rho){
  ## Détermine la matrice de covariance
  
  # ENTRÉES
  #   rho : [integer] corrélation
  
  # SORTIES
  #   Sigma: [matrix] matrice de covariance (2 x 2)
  
  # Définition des paramètres pour obtenir notre copule gaussienne
  R <- matrix(data = c(1, rho, rho, 1), # Matrice de corrélation
              nrow = 2,
              ncol = 2,
              byrow = TRUE)
  sig   <- c(1, 1) # Écart-type standard (pour des variables standardisées)
  Sigma <- diag(sig) %*% R %*% diag(sig) # Matrice de covariance calculée
  
  return(Sigma)
}
