f_display_copula <- function(my_copula, grid_1, grid_2, type = 1) {
  ## Affiche un graphique 3D d'une fonction copule donnée
  
  # ENTRÉES :
  #       my_copula: une fonction représentant une copule
  #       grid_1: un vecteur de valeurs pour la première variable
  #       grid_2: un vecteur de valeurs pour la seconde variable
  #       type: un entier spécifiant le type de graphique à afficher (1 pour un graphique statique, 2 pour un graphique interactif)
  
  # SORTIE :
  #     Graphique 3D de la fonction copule
  
  # Calcul de la taille du maillage
  n_mesh <- length(grid_1) 
  
  # Initialisation de la matrice pour les valeurs de la copule
  f_U <- matrix(NA, n_mesh, n_mesh) 
  # Remplissage de la matrice avec les valeurs de la copule
  for (j in 1:n_mesh) {
    for (k in 1:n_mesh) {
      u <- c(grid_1[j],  grid_2[k])         
      f_U[j,k] <- my_copula(u) # Calcul de la valeur de la copule pour le point (j, k)
    }
  }
  
  # Affichage du graphique selon le type spécifié
  if (type == 1) {
    # Graphique statique avec persp
    persp(x = grid_1, 
          y = grid_2, 
          z = f_U, 
          xlab = "U_SP500", # Étiquette axe x
          ylab = "U_VIX", # Étiquette axe y
          zlab = "Pdf", # Étiquette axe z pour la densité de probabilité
          theta = 30, # Angle de vue horizontal
          phi = 20, # Angle de vue vertical
          col = "yellow", # Couleur des faces
          ticktype = "detailed", # Type de graduation
          nticks = 4, # Nombre de graduations
          expand = 0.5) # Facteur d'expansion
  }
  if (type == 2) {
    # Graphique interactif avec rgl::persp3d
    rgl::persp3d(x = grid_1, y = grid_2, z = f_U, add = FALSE) # Utilisation de persp3d pour un rendu 3D interactif
  }
}
