plot_hist_by_claim<-function(data, col){
  # calculate the number of participants in each class
  library(ggplot2)
  data$Claim <- as.factor(data$Claim)
  df <- data %>%
    group_by(Claim) %>%
    count(.data[[col]]) 
  
  p1<-ggplot(df, aes(x = n, y = reorder(.data[[col]], n), fill = Claim) ) + 
    geom_bar(stat = "identity",  width = 0.7) +
    theme_bw()+
    scale_fill_brewer(palette = "Set2", name = "Nombre de sinistres")+
    theme(legend.position = "bottom") +
    labs(title = paste("Répartition de claim par" ,col),
         x = "nombre d'observations",
         y = col)
  
  
  
  
  p2<-ggplot(data, aes(x = .data[[col]], fill = as.factor(Claim))) +
    geom_bar(position = "dodge") +
    coord_flip() +
    labs(title = paste("Répartition des sinistres par" ,col ), x =col, y = "Nombre de sinistres") +
    theme_bw()+
    scale_fill_brewer(palette = "Set2", name = "Nombre de sinistres")
  
  return(p1)
  
}

plot_percentage <- function(data, col,precision=1) {
  library(ggplot2)
  library(dplyr)
  
  # Calcul des pourcentages
  data_summary <- data %>%
    count(.data[[col]]) %>%
    mutate(percentage = n / sum(n) * 100)  # Ajout des pourcentages
  
  # Création du graphique avec les pourcentages stylisés
  ggplot(data_summary, aes(x = as.factor(.data[[col]]), y = n, fill = as.factor(.data[[col]]))) +
    geom_bar(
      stat = "identity",
      width = 0.7    ) +
    geom_text(
      aes(
        label = paste0(round(percentage, precision), "%"), 
        color = as.factor(.data[[col]])  # Couleur des étiquettes dépendante de la colonne
      ), 
      vjust =- 0.2, 
      fontface = "bold",  # Style gras
      size = 4          # Taille de la police
    ) +
    labs(
      title = paste("Distribution de la variable", col),
      x = col,
      y = "Fréquence"
    ) +
    scale_fill_discrete(name = col) +
    scale_color_discrete(guide = "none") +  # Supprime la légende pour les couleurs des étiquettes
    theme_bw() +  # Thème épuré
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Incline les étiquettes si elles sont nombreuses
      legend.position = "none"  # Supprime la légende
    )
}



plot_claims_by_region <- function(train_data, geojson_file) {
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(viridis)

  # Étape 1 : Correspondance des noms de régions
  region_mapping <- c(
    "Ile-de-France" = "Île-de-France",
    "Champagne-Ardenne" = "Champagne-Ardenne",
    "Picardie" = "Picardie",
    "Haute-Normandie" = "Haute-Normandie",
    "Centre" = "Centre",
    "Basse-Normandie" = "Basse-Normandie",
    "Bourgogne" = "Bourgogne",
    "Nord-Pas-de-Calais" = "Nord-Pas-de-Calais",
    "Lorraine" = "Lorraine",
    "Alsace" = "Alsace",
    "Franche-Comte" = "Franche-Comté",
    "Pays-de-la-Loire" = "Pays de la Loire",
    "Bretagne" = "Bretagne",
    "Poitou-Charentes" = "Poitou-Charentes",
    "Aquitaine" = "Aquitaine",
    "Midi-Pyrenees" = "Midi-Pyrénées",
    "Limousin" = "Limousin",
    "Rhone-Alpes" = "Rhône-Alpes",
    "Auvergne" = "Auvergne",
    "Languedoc-Roussillon" = "Languedoc-Roussillon",
    "Provence-Alpes-Cotes-D'Azur" = "Provence-Alpes-Côte d'Azur",
    "Corse" = "Corse"
  )
  
  # Étape 2 : Appliquer la correspondance aux noms de régions
  train_data <- train_data %>%
    mutate(French_region = recode(French_region, !!!region_mapping))
  
  # Étape 3 : Calculer le nombre total de Claim par région
  claim_region <- train_data %>%
    group_by(French_region) %>%
    summarise(Total_Claim = sum(Claim))
  
  # Étape 4 : Charger la carte des régions
  france_map <- st_read(geojson_file, quiet = TRUE)
  
  # Étape 5 : Joindre les données des sinistres à la carte
  france_map <- france_map %>%
    left_join(claim_region, by = c("nom" = "French_region"))
  
  # Étape 6 : Calculer les centroïdes pour l'affichage des étiquettes
  centroides <- france_map %>%
    st_centroid() %>%
    mutate(long = st_coordinates(.)[, 1],
           lat = st_coordinates(.)[, 2])
  
  

  
  # Étape 7 : Tracer la carte
  ggplot(france_map) +
    geom_sf(aes(fill = Total_Claim), color = "white") +
    scale_fill_viridis_c(option = "C", na.value = "grey90") +
    geom_text(data = centroides,
              aes(x = long, y = lat, label = nom),
              color = "black", size = 2.5, fontface = "bold") +
    theme_void() +
    labs(
      title = "Répartition des sinistres (Claim) par région en France",
      subtitle = "Données avant le redécoupage régional de 2015",
      fill = "Total Claim"
    )
}


plot_categorical <- function(data, col) {
  library(ggplot2)
  ggplot(data, aes(x = as.factor(.data[[col]]), fill = as.factor(.data[[col]]))) +
    geom_bar() +
    labs(
      title = paste("Distribution de la variable", col),
      x = col,
      y = "Fréquence"
    ) +
    scale_fill_discrete(name = col) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Incline les étiquettes si elles sont nombreuses
      legend.position = "none"  # Supprime la légende
    )
}

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

