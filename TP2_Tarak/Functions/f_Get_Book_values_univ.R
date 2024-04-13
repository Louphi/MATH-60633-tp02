f_Get_Book_values_univ <- function(S, book) {
  ### Calcule les valeurs d'un portefeuille d'options utilisant la formule de Black-Scholes en supposant un modèle univarié
  
  # ENTRÉES :
  #  S   : [numeric] prix actuel de l'action
  #  book: [matrix] matrice (n x 4) des contrats d'options dans le portefeuille
  # SORTIES :
  #  book_values: [vector] vecteur (n x 1) des valeurs de chaque contrat d'option dans le portefeuille
  
  # Calcul des prix des options
  book_prices <- apply(book, 1, function(x) {
    blackscholes(callput = 1,  # 1 pour une option d'achat, utiliser -1 pour une option de vente
                 S0 = S,
                 K = x["K"],
                 r = x["r"],
                 time = x["tau"],
                 vola = x["vol"])$Price  # Volatilité spécifique pour chaque option du portefeuille
  })
  
  # Calcul des valeurs du portefeuille en additionnant les prix de chaque contrat
  book_values <- apply(book_prices, 1, sum)
  return(book_values) # Retourne les valeurs du portefeuille
}
