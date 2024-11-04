#' Genere un rapport RMarkdown automatise à partir d'une table de demandes et d'une liste de graphiques ou de tables.
#'
#' Cette fonction cree un fichier RMarkdown structure qui inclut des graphiques ggplot2 et des tables, organises par sous-titres et titres.
#' Les graphiques et les tables sont fournis dans une liste, et les titres et sous-titres sont extraits de la table de demandes.
#'
#' @param table_demandes Un data frame contenant au moins les colonnes suivantes :
#'   \describe{
#'     \item{\code{titre}}{Les titres des graphiques ou tables à inclure dans le rapport.}
#'     \item{\code{nom_onglet}}{Les sous-titres ou sections sous lesquelles les elements seront regroupes.}
#'     \item{\code{nom_fichier_xls}}{Le nom du fichier Excel associe. Une chaîne vide indique un element Markdown.}
#'   }
#' @param out Une liste contenant les graphiques (objets ggplot) et les tables (data frames) à inclure dans le rapport.
#'   Chaque element de la liste doit correspondre à un titre dans \code{table_demandes$titre}.
#' @param out_ollama Liste optionnelle contenant les sorties d'Ollama à inclure dans le rapport.
#' @param fonction_ask_ollama Fonction optionnelle pour interroger Ollama.
#' @param metadata Donnees supplementaires optionnelles pour la generation des prompts Ollama.
#' @param prompt_header En-tête optionnel pour les prompts Ollama.
#' @param prompt_instruction Instructions optionnelles pour les prompts Ollama.
#' @param model_name Nom optionnel du modèle Ollama à utiliser.
#'
#' @details
#' La fonction genere un fichier RMarkdown nomme \code{rapport_automatique.Rmd} dans le repertoire \code{output}.
#' Le rapport est structure avec des sous-titres (niveaux 2) et des titres (niveaux 3) correspondant respectivement à \code{nom_onglet} et \code{titre}.
#' Les graphiques et les tables sont inseres sous leurs titres correspondants.
#' Si le repertoire \code{output} n'existe pas, il sera cre automatiquement.
#'
#' Si des parametres Ollama sont fournis, la fonction peut enrichir le rapport avec des commentaires generes par l'IA.
#'
#' @return
#' Cette fonction ne retourne pas de valeur, mais ecrit un fichier RMarkdown dans le repertoire \code{output}.
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation de generer_markdown_auto
#'
#' # Preparation de la table des demandes
#' table_demandes <- data.frame(
#'   titre = c("Graphique 1", "Tableau 1", "Graphique 2"),
#'   nom_onglet = c("Section A", "Section A", "Section B"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Creation des graphiques et tables
#' library(ggplot2)
#' plot1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' table1 <- head(mtcars)
#' plot2 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
#'
#' # Liste des objets à inclure
#' out <- list(plot1, table1, plot2)
#'
#' # Generation du rapport
#' generer_markdown_auto(table_demandes, out)
#'
#' # Rendu du rapport en HTML
#' rmarkdown::render("output/rapport_automatique.Rmd")
#' }
#'
#' @export
generer_markdown_auto <- function(table_demandes, out,
out_ollama = NULL, fonction_ask_ollama = NULL, metadata = NULL,
prompt_header=NULL,prompt_instruction = NULL,model_name = NULL){
  # 'out' doit être fourni en argument et doit être une liste contenant les graphiques et tables
  
  is_markdown <- nchar(table_demandes$nom_fichier_xls)==0
  titles <- table_demandes$titre
  soustitres <- table_demandes$nom_onglet[is_markdown]
  
  
  # Verifier que les longueurs des vecteurs correspondent
  if (length(titles) != length(out)) {
    stop("Les longueurs de 'titles', 'soustitres' et 'out' doivent être identiques.")
  }
  
  # Initialisation du contenu du RMarkdown avec les metadonnees YAML
  rmd_content <- "---
title: \"Bilan\"
output:
  rmdformats::readthedown:
    toc_depth: 3
---

"

  # Ajout des bibliotheques au debut du document
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
    
    # Parcourir les elements qui correspondent à ce sous-titre
    indices <- which(soustitres == subtitle & is_markdown)
    for (i in indices) {
      # Ajouter le titre en tant qu'en-tête de niveau 3
      rmd_content <- paste0(rmd_content, "### ", titles[i], "\n\n")
      
      # Nom unique pour chaque chunk
      chunk_name <- paste0("chunk_", i)
      
      # Verification si l'element est une table ou un graphique
      if (is.data.frame(out[[i]])) {
        # Ajout du chunk pour une table avec knitr::kable
        rmd_content <- paste0(rmd_content, "```{r ", chunk_name, ", echo=FALSE}\n")
        rmd_content <- paste0(rmd_content, "knitr::kable(out[[", i, "]])\n")
        rmd_content <- paste0(rmd_content, "```\n\n")
      } else {
        # Ajout du chunk pour un graphique ggplot
        rmd_content <- paste0(rmd_content, "```{r ", chunk_name, ", echo=FALSE}\n")
        rmd_content <- paste0(rmd_content, "out[[", i, "]] \n")
        rmd_content <- paste0(rmd_content, "```\n\n")
      }
      # i <-1
      if (!is.null(out_ollama)){
        table <- out_ollama[[i]]

        if (!is.null(prompt_header))
        { 
          prompt <- preparer_prompt(table,metadata,prompt_header,prompt_instruction)
        }else{
          prompt <- preparer_prompt(table,metadata)
        }
        rep_ollama <- fonction_ask_ollama(prompt,model_name = model_name) 
        rmd_content <- paste0(rmd_content, rep_ollama, "\n\n")
    }
  }}
  
  # Creation du repertoire 'output' s'il n'existe pas
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }
  
  # ecriture du contenu dans un fichier RMarkdown
  writeLines(rmd_content, "output/rapport_automatique.Rmd")
}
