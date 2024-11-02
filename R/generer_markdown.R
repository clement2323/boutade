#' Génère un rapport RMarkdown automatisé à partir d'une table de demandes et d'une liste de graphiques ou de tables.
#'
#' Cette fonction crée un fichier RMarkdown structuré qui inclut des graphiques ggplot2 et des tables, organisés par sous-titres et titres.
#' Les graphiques et les tables sont fournis dans une liste, et les titres et sous-titres sont extraits de la table de demandes.
#'
#' @param table_demandes Un data frame contenant au moins deux colonnes :
#'   \describe{
#'     \item{\code{titre}}{Les titres des graphiques ou tables à inclure dans le rapport.}
#'     \item{\code{nom_onglet}}{Les sous-titres ou sections sous lesquelles les éléments seront regroupés.}
#'   }
#' @param out Une liste contenant les graphiques (objets ggplot) et les tables (data frames) à inclure dans le rapport.
#'   Chaque élément de la liste doit correspondre à un titre dans \code{table_demandes$titre}.
#'
#' @details
#' La fonction génère un fichier RMarkdown nommé \code{rapport_automatique.Rmd} dans le répertoire \code{output}.
#' Le rapport est structuré avec des sous-titres (niveaux 2) et des titres (niveaux 3) correspondant respectivement à \code{nom_onglet} et \code{titre}.
#' Les graphiques et les tables sont insérés sous leurs titres correspondants.
#' Si le répertoire \code{output} n'existe pas, il sera créé automatiquement.
#'
#' @return
#' Cette fonction ne retourne pas de valeur, mais écrit un fichier RMarkdown dans le répertoire \code{output}.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation de generer_markdown_auto
#'
#' # Préparation de la table des demandes
#' table_demandes <- data.frame(
#'   titre = c("Graphique 1", "Tableau 1", "Graphique 2"),
#'   nom_onglet = c("Section A", "Section A", "Section B"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Création des graphiques et tables
#' library(ggplot2)
#' plot1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' table1 <- head(mtcars)
#' plot2 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
#'
#' # Liste des objets à inclure
#' out <- list(plot1, table1, plot2)
#'
#' # Génération du rapport
#' generer_markdown_auto(table_demandes, out)
#'
#' # Rendu du rapport en HTML
#' rmarkdown::render("output/rapport_automatique.Rmd")
#' }
#'
#' @export
generer_markdown_auto <- function(table_demandes, out) {
  # 'out' doit être fourni en argument et doit être une liste contenant les graphiques et tables
  
  titles <- table_demandes$titre
  soustitres <- table_demandes$nom_onglet
  
  # Vérifier que les longueurs des vecteurs correspondent
  if (length(titles) != length(out) || length(soustitres) != length(out)) {
    stop("Les longueurs de 'titles', 'soustitres' et 'out' doivent être identiques.")
  }
  
  # Initialisation du contenu du RMarkdown avec les métadonnées YAML
  rmd_content <- "---
title: \"Rapport Automatisé\"
output:
  rmdformats::readthedown
---

"
  # Ajout des bibliothèques au début du document
  rmd_content <- paste0(rmd_content, "```{r setup, include=FALSE}\n")
  rmd_content <- paste0(rmd_content, "library(ggplot2)\n")
  rmd_content <- paste0(rmd_content, "library(knitr)\n")
  rmd_content <- paste0(rmd_content, "```\n\n")
  
  # Obtenir les sous-titres uniques tout en conservant l'ordre d'apparition
  unique_subtitles <- unique(soustitres)
  
  # Parcourir chaque sous-titre
  for (subtitle in unique_subtitles) {
    # Ajouter le sous-titre en tant qu'en-tête de niveau 2
    rmd_content <- paste0(rmd_content, "## ", subtitle, "\n\n")
    
    # Parcourir les éléments qui correspondent à ce sous-titre
    indices <- which(soustitres == subtitle)
    for (i in indices) {
      # Ajouter le titre en tant qu'en-tête de niveau 3
      rmd_content <- paste0(rmd_content, "### ", titles[i], "\n\n")
      
      # Nom unique pour chaque chunk
      chunk_name <- paste0("chunk_", i)
      
      # Vérification si l'élément est une table ou un graphique
      if (is.data.frame(out[[i]])) {
        # Ajout du chunk pour une table avec knitr::kable
        rmd_content <- paste0(rmd_content, "```{r ", chunk_name, ", echo=FALSE}\n")
        rmd_content <- paste0(rmd_content, "knitr::kable(out[[", i, "]], caption = '", titles[i], "')\n")
        rmd_content <- paste0(rmd_content, "```\n\n")
      } else {
        # Ajout du chunk pour un graphique ggplot
        rmd_content <- paste0(rmd_content, "```{r ", chunk_name, ", echo=FALSE}\n")
        rmd_content <- paste0(rmd_content, "out[[", i, "]] + ggtitle('", titles[i], "')\n")
        rmd_content <- paste0(rmd_content, "```\n\n")
      }
    }
  }
  
  # Création du répertoire 'output' s'il n'existe pas
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # Écriture du contenu dans un fichier RMarkdown
  writeLines(rmd_content, "output/rapport_automatique.Rmd")
}
