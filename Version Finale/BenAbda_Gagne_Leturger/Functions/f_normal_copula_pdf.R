f_normal_copula_pdf <- function(u, mu, Sigma) {
  ## Calcule la PDF de la copule de la distribution normale en un 
  ## point générique u dans l'hypercube unité
  
  # ENTRÉES
  #  u     : [vector] vecteur (J x 1) des rangs
  #  mu    : [vector] vecteur (N x 1) des moyennes
  #  Sigma : [matrix] matrice (N x N) de covariance
  
  # SORTIES
  #  F_U   : [vector] vecteur (J x 1) des valeurs de la PDF
  
  N <- length(u)  # Nombre d'observations
  m <- length(mu) # Nombre d'actifs
  s <- sqrt(diag(Sigma)) # Écart-type (racine carrée de la diagonale de Sigma)
  
  x <- qnorm(p = u, mean = mu, sd = s) # Transformation inverse de la normale
  
  # Calcul du numérateur de la fonction de densité
  Numerator <- (2 * pi)^(-N/2) * ((det(Sigma))^(-0.5)) * exp(-0.5 * (x - mu) %*% pracma::mldivide(A = Sigma, B = (x - mu)))
  
  fs <- dnorm(x, mu, s) # Densité de probabilité normale pour x
  Denominator <- prod(fs) # Produit des densités individuelles
  
  # Division du numérateur par le dénominateur pour obtenir la PDF de la copule
  F_U <- pracma::mrdivide(Numerator, Denominator)
  
  return(F_U) # Retourne la valeur de la PDF de la copule
}
