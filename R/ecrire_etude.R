#' Génère un rapport automatique à partir des demandes d'analyse
#'
#' @param nom_etude Character. Nom du dossier d'étude dans le répertoire "Etudes/"
#' @param ollama Logical. Si TRUE, utilise l'IA Ollama pour la génération des figures
#'
#' @details
#' La fonction lit les métadonnées et demandes depuis les fichiers :
#' - metadonnees_etude.json
#' - metadonnees_tables.csv  
#' - fichier_demandes.csv
#'
#' @return Génère un fichier Rmd dans le dossier output/ et le compile
#' @export
#'
#' @examples
#' \dontrun{
#' ecrire_etude("mon_etude", ollama = FALSE)
#' }
ecrire_etude <- function(nom_etude, ollama) {
  #nom_etude <- liste_dir_etude[[1]]
  #ollama <- TRUE
  
  dir_etude <- paste0("Etudes/",nom_etude,"/")
  
  # source
  # env <- new.env()
  # env$dir_etude <- dir_etude

  code_path <- file.path(dir_etude, "codes")
  source(file.path(code_path,"Preparation_donnees.R"),#local = env,
         encoding= "UTF-8")
  
  # Définition du chemin de base
  input_path <- file.path(dir_etude, "input")
  metadonnees_etude <- jsonlite::fromJSON(file.path(input_path, "metadonnees_etude.json"))
  metadonnees_tables <- read.csv(file.path(input_path, "metadonnees_tables.csv"))
  table_demandes_rmd <- read.csv(file.path(input_path, "fichier_demandes.csv"))
  
  #erreurs <- controler_demandes(table_demandes_rmd)
  #if(nrow(erreurs)!=0) erreurs; stop("il ya des erreurs dans les demandes")
  
  
  liste_info_chunk <- pblapply(1:nrow(table_demandes_rmd),function(i){
    if(ollama){      
      generer_figure_from_demande(
        table_demandes_rmd[i,],
        metadonnees_tables,
        ollama  = TRUE,
        fonction_ask = function(prompt) ask_ollama(prompt,"mistral-small"),
        metadonnees_etude = metadonnees_etude
      )
    }else{
      generer_figure_from_demande(
        table_demandes_rmd[i,],
        metadonnees_tables
      )
    }
  }
  )
  nom_rapport <- paste0("rapport_automatique_",nom_etude,".Rmd")
  generer_markdown_auto_simple(liste_info_chunk,nom_rapport = nom_rapport)
  
  rmarkdown::render(
    input = paste0("output/", nom_rapport),
    envir = environment()
  )
  
}