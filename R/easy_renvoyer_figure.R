retourner_figure <- function(table_agrege,nouveaux_noms_colonnes,params) {
  # Extraction des variables de croisement
  var_croisement <- params$var_croisement
  var_x <- var_croisement[1]
  lab_x <- nouveaux_noms_colonnes[var_x]
  
  var_fill <- params$var_croisement_relative
  lab_fill <- nouveaux_noms_colonnes[var_fill]
  
  # Détection si on travaille avec des parts
  is_part <- any(grepl("-part|Part", colnames(table_agrege)))
  var_y <- grep("-part|Part", colnames(table_agrege), value = TRUE)
  
  if (length(var_y) == 0) var_y <- setdiff(colnames(table_agrege), c(var_croisement, var_fill))
  
  type_figure <- params$rmd$type_figure
  # Configuration du graphique selon le type
  if (type_figure == "kable") {
    out <- table_agrege
    colnames(out)<- nouveaux_noms_colonnes
    return(knitr::kable(out))
  }
  
  lab_y <- ifelse(is_part, "Pourcentage", "Valeur")
  
  # Paramètres communs pour les graphiques
  params_graph <- list(
    data = table_agrege,
    var_x = var_x,
    var_y = var_y,
    var_fill = var_fill,
    titre_legende_fill = lab_fill,
    lab_x = lab_x,
    lab_y = lab_y,
    titre = params$autres$titre,
    param_position = ifelse(is_part, "stack", "dodge")
  )
  
  # type_figure = "camembert"
  # Création du graphique selon le type
  graph <- do.call(creer_graphique, c(
    params_graph,
    list(type = switch(type_figure,
      "camembert" = "pie",
      "hbar" = "hbar",
      "vbar" = "vbar",
      "graphique"="vbar" # sorte de par défaut
    ))
  ))
  
  # Ajout de facet si plusieurs variables de croisement
  if (length(var_croisement) > 1) {
    graph <- graph + facet_wrap(as.formula(paste("~", nouveaux_noms_colonnes[[var_croisement[2]]])))
  }
  
  return(graph)
}

#' Créer un graphique avec ggplot2
#'
#' Génère différents types de graphiques (barres verticales/horizontales, camembert) 
#' à partir d'un jeu de données en utilisant ggplot2.
#'
#' @param data data.frame contenant les données
#' @param var_x Nom de la variable pour l'axe X
#' @param var_y Nom de la variable pour l'axe Y
#' @param var_fill Nom de la variable pour le remplissage (couleurs)
#' @param lab_x Label de l'axe X
#' @param lab_y Label de l'axe Y
#' @param titre_legende_fill Titre de la légende des couleurs
#' @param titre Titre du graphique
#' @param type Type de graphique ("vbar", "hbar", "pie"). Défaut: "vbar"
#' @param color_theme Vecteur de couleurs hexadécimales
#' @param param_position Position des barres ("stack", "dodge"). Défaut: "stack"
#' @param save Booléen pour sauvegarder le graphique. Défaut: FALSE
#'
#' @return Un objet ggplot2
#'
#' @examples
#' \dontrun{
#' # Graphique en barres verticales
#' creer_graphique_bar(
#'   data = df,
#'   var_x = "categorie",
#'   var_y = "valeur",
#'   var_fill = "groupe",
#'   lab_x = "Catégories",
#'   lab_y = "Valeurs",
#'   titre_legende_fill = "Groupes",
#'   titre = "Mon graphique"
#' )
#'
#' # Graphique en barres horizontales
#' creer_graphique_bar(
#'   data = df,
#'   var_x = "categorie",
#'   var_y = "valeur", 
#'   var_fill = "groupe",
#'   type = "hbar"
#' )
#'
#' # Camembert
#' creer_graphique_bar(
#'   data = df,
#'   var_x = "categorie",
#'   var_y = "valeur",
#'   var_fill = "groupe", 
#'   type = "pie"
#' )
#' }
#'
#' @import ggplot2
#' @export
creer_graphique <- function(
  data,
  var_x,
  var_y,
  var_fill,
  lab_x,
  lab_y,
  titre_legende_fill,
  titre,
  type = "vbar",
  color_theme = c("#FF4858", "#1B7F79", "#00CCC0", "#72F2EB", "#747F7F"),
  param_position = "stack",
  save = FALSE
) { 
  # data <- table_agrege
  # Preparation des labels pour la legende
  n_color <- length(unique(data[[var_fill]]))
  colors_to_use <- color_theme[1:n_color]

  # Preparation des arguments pour labs()
  labs_args <- list(
    title = titre,
    x = lab_x,
    y = lab_y,
    fill = titre_legende_fill 
  )

  # Application automatique aux variables
  var_y <- add_backticks(var_y)
  var_x <- add_backticks(var_x)
  var_fill <- add_backticks(var_fill)

  # Base commune pour tous les graphiques
  p <- ggplot(data, aes_string(x = var_x, y = var_y, fill = var_fill)) +
    scale_fill_manual(values = colors_to_use) +
    theme_minimal() +
    do.call(labs, labs_args)
  
  # Ajout des éléments spécifiques selon le type
  p <- p + switch(type,
    "vbar" = geom_bar(stat = "identity", position = param_position),
    "hbar" = list(
      geom_bar(stat = "identity", position = param_position),
      coord_flip()
    ),
    "pie" = list(
      geom_bar(stat = "identity", width = 1),
      coord_polar("y"),
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
    ),
    # Par défaut: barres verticales
    geom_bar(stat = "identity", position = param_position)
  )

  # Creation du dossier output s'il n'existe pas
  if (!dir.exists("output")) {
    dir.create("output")
  }

  # Generation du nom de fichier en remplaçant les espaces par des underscores
  safe_title <- gsub("[^[:alnum:] ]", "", titre)  # Enlever les caracteres non-alphanumeriques
  filename <- paste0("output/", gsub(" ", "_", safe_title), ".png")

  # Sauvegarde du graphique
  if (save) ggsave(filename, plot = p, width = 8, height = 6, units = "in")

  return(p)
}

#' Ajouter des backticks a un nom de variable
#'
#' Cette fonction ajoute des backticks (`) autour d'un nom de variable s'ils ne sont pas deja presents.
#' Elle est utilisee pour formater correctement les noms de variables dans les appels ggplot2.
#'
#' @param var_name Chaîne de caracteres representant le nom de la variable.
#' @return Une chaîne de caracteres avec des backticks autour du nom de la variable.
#'
#' @examples
#' add_backticks("ma_variable")  # Retourne "`ma_variable`"
#' add_backticks("`ma_variable`")  # Retourne "`ma_variable`" (inchange)
#'
#' @noRd
add_backticks <- function(var_name) {
    if (!startsWith(var_name, "`")) {
        var_name <- paste0("`", var_name, "`")
    }
    return(var_name)
}