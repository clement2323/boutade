#' Génère un rapport automatique à partir des métadonnées d'une étude
#' 
#' @param nom_etude Nom de l'étude (chaîne de caractères)
#' @param ollama Utilisation d'Ollama pour la génération (logique, défaut FALSE)
#' @param model_name Nom du modèle Ollama (chaîne, défaut "mistral-small")
#' @param role Rôle utilisateur pour personnalisation (chaîne, optionnel)
#' 
#' @return Génère un fichier Rmd et son rendu HTML dans le dossier output/
#' @import rmarkdown
#' @importFrom jsonlite fromJSON
#' @importFrom pbapply pblapply
#' @examples
#' ecrire_etude("titanic", ollama = TRUE, role = "analyste")
#' @export 
ecrire_etude <- function(nom_etude, ollama = FALSE, model_name = "mistral-small", role = NULL) {
  
  #nom_etude = "titanic"
  #ollama = TRUE
  #model_name = "mistral-small"
  #role = "blase"
  
  return_filename <- function(dir,nom_etude){
    paste0(dir,"/",grep(nom_etude,list.files(dir),value=TRUE))
  }


  source(return_filename("codes",nom_etude),encoding="UTF-8")
  table_demandes <- read.csv(return_filename("metadonnees/demandes",nom_etude),encoding="UTF-8")
  metadonnees_tables <- read.csv(return_filename("metadonnees/metadonnees_tables",nom_etude),encoding="UTF-8")
  metadonnees_etude <- fromJSON(return_filename("metadonnees/metadonnees_etudes",nom_etude))
  if(ollama)   metadonnees_role <- fromJSON(paste0("metadonnees/roles/role_",role,".json"))

  liste_info_chunk <- pblapply(
    1:nrow(table_demandes), 
    function(i) { 
      # i <- 8
      if(ollama){
        generer_figure_from_demande(
          table_demandes[i, ],
          metadonnees_tables, 
          ollama = TRUE,
          fonction_ask = function(prompt) ask_ollama(prompt,model_name),
          metadonnees_role = metadonnees_role,
          metadonnees_etude = metadonnees_etude
        )
      }else{
        generer_figure_from_demande(
          table_demandes[i,],
          metadonnees_tables
          )
      }
  })
  
  nom_rapport <- paste0("rapport_automatique_", nom_etude, "_",role,".Rmd")
  generer_markdown_auto_simple(liste_info_chunk, nom_rapport = nom_rapport)
  rmarkdown::render(
    input = paste0("output/", nom_rapport), 
    envir = environment()
    )
}
  
