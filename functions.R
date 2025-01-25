

classifier_variables_tab <- function(data_set) {
  # Initialisation des listes
  variables_na <- c()
  variables_numeriques <- c()
  variables_binaires <- c()
  variables_categorielles <- c()
  
  # Parcourir les colonnes du dataset
  for (colu in colnames(data_set)) {
    # Vérifier si la colonne contient des valeurs NA
    if (any(is.na(data_set[[colu]]))) {
      variables_na <- c(variables_na, colu)
    }
    
    # Vérifier si la colonne est numérique
    if (is.numeric(data_set[[colu]])) {
      # Vérifier si la colonne est binaire (deux valeurs uniques)
      if (length(unique(data_set[[colu]])) == 2) {
        variables_binaires <- c(variables_binaires, colu)
      } else {
        variables_numeriques <- c(variables_numeriques, colu)
      }
    } else {
      # Gérer les colonnes non numériques
      if (length(unique(data_set[[colu]])) == 2) {
        variables_binaires <- c(variables_binaires, colu)
      } else {
        variables_categorielles <- c(variables_categorielles, colu)
      }
    }
  }
  
  # Création du résultat sous forme de liste
  resultat <- list(
    variables_na = variables_na,
    variables_numeriques = variables_numeriques,
    variables_binaires = variables_binaires,
    variables_categorielles = variables_categorielles
  )
  
  return(resultat)
}



classifier_variables <- function(data_set) {
  # Initialisation des listes
  variables_na <- c()
  variables_numeriques <- c()
  variables_binaires <- c()
  variables_categorielles <- c()
  
  # Parcourir les colonnes du jeu de données
  for (colu in colnames(data_set)) {
    # Vérifier si la colonne contient des valeurs NA
    if (any(is.na(data_set[[colu]]))) {
      variables_na <- c(variables_na, colu)
    }
    
    # Vérifier si la colonne est numérique
    if (is.numeric(data_set[[colu]])) {
      # Vérifier si la colonne est binaire (deux valeurs uniques)
      if (length(unique(data_set[[colu]])) == 2) {
        variables_binaires <- c(variables_binaires, colu)
      } else {
        variables_numeriques <- c(variables_numeriques, colu)
      }
    } else {
      # Gérer les colonnes non numériques
      if (length(unique(data_set[[colu]])) == 2) {
        variables_binaires <- c(variables_binaires, colu)
      } else {
        variables_categorielles <- c(variables_categorielles, colu)
      }
    }
  }
  
  # Création du tableau final
  variables <- data.frame(
    Type = c("Variables NA", "Variables Numériques", "Variables Binaires", "Variables Catégorielles"),
    Colonnes = c(
      paste(variables_na, collapse = ", "),
      paste(variables_numeriques, collapse = ", "),
      paste(variables_binaires, collapse = ", "),
      paste(variables_categorielles, collapse = ", ")
    )
  )
  
}

