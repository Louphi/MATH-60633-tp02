f_transform_to_unif <- function(data, density, df = NA){
  ## Transforme les données en distribution uniforme en utilisant la fonction de répartition inverse
  
  # ENTRÉES
  #   data    : [vector] vecteur (n x 1) de données 
  #   density : c('normal','student') la densité désirée
  #   df      : [integer] degrés de liberté dans le cas d'une distribution de Student
  
  # SORTIES
  #   result : [vector] vecteur (n x 1) de données uniformément distribuées
  #   params : [vector] vecteur (2 x 1) des paramètres estimés de la distribution
  
  # Traitement selon la densité choisie
  if (density == "normal") {
    # Estimation des paramètres de la distribution normale
    params <- fitdistr(x = data, "normal")$estimate
    # Transformation des données en distribution uniforme
    result <- as.numeric(pnorm(data, params["mean"], params["sd"]))
  }
  else {
    # Estimation des paramètres de la distribution de Student
    params <- fitdistr(x = data,
                       densfun = dstd,
                       start = list(mean = 0, sd = 1),
                       nu = df)$estimate
    # Transformation des données en distribution uniforme
    result <- as.numeric(pstd(data, mean = params[1], sd = params[2], nu = df))
  }
  # Retourne un liste contenant les données transformées et les paramètres estimés
  return(list(result, params))
}
