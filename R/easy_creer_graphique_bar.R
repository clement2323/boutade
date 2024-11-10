#' Creer un graphique a barres avec ggplot2 et le sauvegarder automatiquement
#'
#' Cette fonction genere un graphique a barres a partir d'un jeu de donnees en utilisant ggplot2.
#' Le graphique est automatiquement sauvegarde dans un repertoire 'output' avec un nom de fichier
#' genere a partir du titre du graphique.
#'
#' @param data \code{data.frame} contenant lesgit push -u origin main donnees a representer.
#' @param var_x Chaîne de caracteres specifiant le nom de la variable pour l'axe des abscisses dans \code{data}.
#' @param var_y Chaîne de caracteres sgipecifiant le nom de la variable pour l'axe des ordonnees dans \code{data}.
#' @param var_fill Chaîne de caracteres specifiant le nom de la variable de remplissage (couleur) dans \code{data}.
#' @param lab_x Chaîne de caracteres pour le label de l'axe des abscisses.
#' @param lab_y Chaîne de caracteres pour le label de l'axe des ordonnees.
#' @param titre Chaîne de caracteres pour le titre du graphique.
#' @param labels_fill Vecteur de chaînes de caracteres pour les labels de la legende des couleurs.
#' @param soustitre (Optionnel) Chaîne de caracteres pour le sous-titre du graphique. Par defaut : \code{NULL}.
#' @param color_theme Vecteur de codes couleurs hexadecimaux pour le graphique. Par defaut : palette de couleurs predefinie.
#' @param param_position Chaîne de caracteres specifiant la position des barres (\code{"stack"}, \code{"dodge"}, etc.). Par defaut : \code{"stack"}.
#' @param save booleen permettant de choisir si l'on veut sauvegarder ou non.
#' @return Un objet \code{ggplot2} representant le graphique a barres.
#'
#' @details
#' La fonction cree un graphique a barres en utilisant ggplot2 et le sauvegarde automatiquement dans un repertoire 'output'.
#' Le nom du fichier est genere a partir du titre du graphique en remplaçant les espaces par des underscores.
#'
#' Les couleurs utilisees pour le remplissage sont determinees par \code{color_theme}, et les labels de la legende sont specifies par \code{labels_fill}.
#' Si le repertoire 'output' n'existe pas, il sera cre automatiquement.
#'
#' @examples
#' \dontrun{
#' creer_graphique_bar(
#'   data = taux_global_long,
#'   var_x = "dep_EP",
#'   var_y = "percentage",
#'   var_fill = "region",
#'   lab_x = "Departement",
#'   lab_y = "Pourcentage",
#'   titre = "Structure globale de detention",
#'   labels_fill = c("filiales 971", "filiales 972")
#' )
#' }
#'
#' @import ggplot2
#' @importFrom stats setNames
#' @export
creer_graphique_bar <- function(
  data,
  var_x,
  var_y,
  var_fill,
  lab_x,
  lab_y,
  titre,
  labels_fill = NULL,
  soustitre = NULL,
  color_theme = c("#FF4858", "#1B7F79", "#00CCC0", "#72F2EB", "#747F7F"),
  param_position = "stack",
  save = FALSE
) { 
  
  # Preparation des labels pour la legende
  if (!is.null(labels_fill)) lab_fill <- setNames(labels_fill, sort(unique(data[[var_fill]])))
  n_color <- length(unique(data[[var_fill]]))
  colors_to_use <- color_theme[1:n_color]

  # Preparation des arguments pour labs()
  labs_args <- list(
    title = titre,
    x = lab_x,
    y = lab_y
  )

  # Ajouter le sous-titre si 'soustitre' n'est pas NULL ou vide
  if (!is.null(soustitre) && soustitre != "") {
    labs_args$subtitle <- soustitre
  }

  # Application automatique aux variables
  var_y <- add_backticks(var_y)
  var_x <- add_backticks(var_x)
  var_fill <- add_backticks(var_fill)

  # Creation du graphique
  p <-ggplot(data, aes_string(x = var_x, y = var_y, fill = var_fill)) +
    geom_bar(stat = "identity", position = param_position) 
  
  if(!is.null(labels_fill)) {
    p <- p + scale_fill_manual(values = colors_to_use, labels = labels_fill)
  }else{
    p <- p + scale_fill_manual(values = colors_to_use)
  }

  p <- p +
    theme_minimal() +
    theme(legend.title = element_text(face = "italic")) +
    do.call(labs, labs_args)

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