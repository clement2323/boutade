#' Génère un rapport RMarkdown à partir d'une liste de figures
#' 
#' @param liste Liste contenant les figures et leurs paramètres
#' @param output_dir Répertoire de sortie (défaut: "output/")
#' @param nom_rapport Nom du fichier Rmd généré (défaut: "rapport_automatique.Rmd")
#' @return NULL
#' @export
generer_markdown_auto_simple <- function(liste,
                                       output_dir = "output/",
                                       nom_rapport = "rapport_automatique.Rmd") {
  nom_liste <- deparse(substitute(liste))
  
  # Template RMarkdown initial
  rmd_content <- c(
    "---",
    "title: \"Bilan\"",
    "output:",
    "  rmdformats::readthedown:",
    "    toc_depth: 3",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(ggplot2)",
    "library(knitr)",
    "```\n"
  )
  
  parties <- unique(sapply(liste, \(x) x$params$rmd$partie))
  
  for (partie in parties) {
    rmd_content <- c(
      rmd_content,
      sprintf("## %s\n", partie)
    )
    
    idx_partie <- which(sapply(liste, \(x) x$params$rmd$partie) == partie)
    sous_parties <- unique(sapply(liste[idx_partie], \(x) x$params$rmd$sous_partie))
    
    for (sous_partie in sous_parties) {
      rmd_content <- c(
        rmd_content,
        sprintf("### %s\n", sous_partie)
      )
      
      idx_figures <- which(sapply(liste, \(x) 
        x$params$rmd$partie == partie & x$params$rmd$sous_partie == sous_partie))
      
      for (i in idx_figures) {
        rmd_content <- c(
          rmd_content,
          sprintf("```{r chunk_%d, echo=FALSE}", i),
          sprintf("%s[[%d]]$figure", nom_liste, i),
          "```\n",
          liste[[i]]$rep_ollama,
          "\n"
        )
      }
    }
  }
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  writeLines(unlist(rmd_content), file.path(output_dir, nom_rapport))
}