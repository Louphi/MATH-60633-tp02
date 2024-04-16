f_ES <- function(data, level){
  ## Calcule la Perte Espérée (Expected Shortfall)
  
  # ENTRÉES :
  #    data  : [vector] un vecteur de données historiques de P&L (profits et pertes)
  #    level : [numeric] le niveau de confiance pour le calcul de l'ES
  
  # SORTIE :
  #    ES  : valeur numérique de l'ES
  
  # Calcul de la VaR pour filtrer les données
  var_value <- f_VaR(data, level)
  
  # Calcul de la moyenne des pertes qui sont pires que la VaR
  ES <- mean(data[data <= var_value])
  return(ES)
}
