## CHARGE ET INSTALLE AUTOMATIQUEMENT LES PACKAGES UTILISÉS

libraries <- c(
  'here',                   # Simplifie les chemins de fichiers en trouvant le répertoire racine du projet
  'xts',                    # Gestion des séries temporelles
  'PerformanceAnalytics',   # Analyse de la performance des portefeuilles
  'ggplot2',                # Création de graphiques avancés
  'ragtop',                 # Modélisation financière et tarification des options
  'MASS',                   # Méthodes statistiques supplémentaires
  'doParallel',             # Support pour le calcul parallèle
  'foreach',                # Boucles 'foreach' pour le calcul parallèle
  'copula',                 # Modèles pour la dépendance et les copules
  'fGarch',                 # Modélisation de la volatilité avec les modèles ARCH et GARCH
  'qtl',                    # Analyse de localisation quantitative pour les études génétiques
  'zeallot',                # Affectation multiple et déballage de structures de données
  'tidyverse',              # Collection de packages pour la science des données
  'pracma',                 # Fonctions pour le calcul numérique et l'ingénierie
  'psych',                  # Techniques pour la psychologie expérimentale et la recherche en sciences sociales
  'qrmtools',               # Outils pour la gestion quantitative du risque
  'fGarch'                  # Modélisation de la volatilité financière (dupliquée, considérer une vérification)
)

# Boucle pour vérifier, installer et charger chaque bibliothèque
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

# Supprime les variables libraries et lib
rm(libraries, lib)
