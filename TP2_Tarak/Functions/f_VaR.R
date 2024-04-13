f_VaR  <- function(data, level){
  ## Calcule la Valeur à Risque (VaR)
  
  # ENTRÉES :
  #    data  : [vector] un vecteur de données historiques de P&L (profits et pertes)
  #    level : [numeric] le niveau de confiance pour le calcul de la VaR
  
  # SORTIE :
  #    VaR  : valeur numérique de la VaR
  
  # Calcul de la quantile correspondante au niveau de confiance spécifié
  VaR <- quantile(data, probs = 1 - level)
  return(VaR)
}
