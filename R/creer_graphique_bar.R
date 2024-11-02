#' Créer un graphique à barres avec ggplot2 et le sauvegarder automatiquement
#'
#' Cette fonction génère un graphique à barres à partir d'un jeu de données en utilisant ggplot2.
#' Le graphique est automatiquement sauvegardé dans un répertoire 'output' avec un nom de fichier
#' généré à partir du titre du graphique.
#'
#' @param data \code{data.frame} contenant lesgit push -u origin main données à représenter.
#' @param var_x Chaîne de caractères spécifiant le nom de la variable pour l'axe des abscisses dans \code{data}.
#' @param var_y Chaîne de caractères sgipécifiant le nom de la variable pour l'axe des ordonnées dans \code{data}.
#' @param var_fill Chaîne de caractères spécifiant le nom de la variable de remplissage (couleur) dans \code{data}.
#' @param lab_x Chaîne de caractères pour le label de l'axe des abscisses.
#' @param lab_y Chaîne de caractères pour le label de l'axe des ordonnées.
#' @param titre Chaîne de caractères pour le titre du graphique.
#' @param labels_fill Vecteur de chaînes de caractères pour les labels de la légende des couleurs.
#' @param soustitre (Optionnel) Chaîne de caractères pour le sous-titre du graphique. Par défaut : \code{NULL}.
#' @param color_theme Vecteur de codes couleurs hexadécimaux pour le graphique. Par défaut : palette de couleurs prédéfinie.
#' @param param_position Chaîne de caractères spécifiant la position des barres (\code{"stack"}, \code{"dodge"}, etc.). Par défaut : \code{"stack"}.
#' @param save booleen permettant de choisir si l'on veut sauvegarder ou non.
#' @return Un objet \code{ggplot2} représentant le graphique à barres.
#'
#' @details
#' La fonction crée un graphique à barres en utilisant ggplot2 et le sauvegarde automatiquement dans un répertoire 'output'.
#' Le nom du fichier est généré à partir du titre du graphique en remplaçant les espaces par des underscores.
#'
#' Les couleurs utilisées pour le remplissage sont déterminées par \code{color_theme}, et les labels de la légende sont spécifiés par \code{labels_fill}.
#' Si le répertoire 'output' n'existe pas, il sera créé automatiquement.
#'
#' @examples
#' \dontrun{
#' creer_graphique_bar(
#'   data = taux_global_long,
#'   var_x = "dep_EP",
#'   var_y = "percentage",
#'   var_fill = "region",
#'   lab_x = "Département",
#'   lab_y = "Pourcentage",
#'   titre = "Structure globale de détention",
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
  # Préparation des labels pour la légende
  if (!is.null(labels_fill)) lab_fill <- setNames(labels_fill, sort(unique(data[[var_fill]])))
  n_color <- length(unique(data[[var_fill]]))
  colors_to_use <- color_theme[1:n_color]

  # Préparation des arguments pour labs()
  labs_args <- list(
    title = titre,
    x = lab_x,
    y = lab_y
  )

  # Ajouter le sous-titre si 'soustitre' n'est pas NULL ou vide
  if (!is.null(soustitre) && soustitre != "") {
    labs_args$subtitle <- soustitre
  }

  # Création du graphique
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

  # Création du dossier output s'il n'existe pas
  if (!dir.exists("output")) {
    dir.create("output")
  }

  # Génération du nom de fichier en remplaçant les espaces par des underscores
  safe_title <- gsub("[^[:alnum:] ]", "", titre)  # Enlever les caractères non-alphanumériques
  filename <- paste0("output/", gsub(" ", "_", safe_title), ".png")

  # Sauvegarde du graphique
  if (save) ggsave(filename, plot = p, width = 8, height = 6, units = "in")

  return(p)
}
