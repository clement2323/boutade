#' Génère une étude automatique avec analyses statistiques
#' 
#' @param dir_input Chemin vers le dossier d'entrée contenant les fichiers de données. Par défaut "input/"
#' @param nom_fichier_demandes Nom du fichier CSV contenant les demandes d'analyse. Par défaut "fichier_demandes_titanic.csv"
#' @param nom_fichier_metadonnees Nom du fichier JSON contenant les métadonnées. Par défaut "metadonnees.json"
#' @param dir_codes Chemin vers le dossier contenant les scripts R. Par défaut "codes/"
#' @param nom_code_setup Nom du fichier de setup. Par défaut "0.Setup.R"
#' @param nom_code_preparation Nom du fichier de préparation. Par défaut "1.Preparation_donnees.R"
#' @param dir_output Chemin vers le dossier de sortie. Par défaut "output/"
#' @param nom_rapport Nom du fichier rapport Rmd à générer. Par défaut "rapport_titanic_ollame.Rmd"
#' @param environnement Type d'environnement ("local" ou autre). Par défaut "local"
#' @param use_ia Booléen indiquant si utiliser l'IA pour l'analyse. Par défaut TRUE
#' @param fonction_ask_ia Fonction à utiliser pour les requêtes IA. Par défaut ask_ollama
#' @param model_name Nom du modèle IA à utiliser. Par défaut "mistral-small"
#'
#' @return Le chemin vers le rapport généré
#' @export
#'
#' @examples
#' ecrire_etude()
#' ecrire_etude(nom_fichier_demandes = "autres_demandes.csv", use_ia = FALSE)
ecrire_etude <- function(
    dir_etude,
    dir_input = "input/",
    nom_fichier_demandes = "fichier_demandes.csv",
    nom_fichier_metadonnees = "metadonnees.json",
    dir_codes = "codes/",
    nom_code_setup = "Setup.R",
    nom_code_preparation = "Preparation_donnees.R",
    dir_output = "output/",
    nom_rapport = "rapport.Rmd",
    use_ia = FALSE,
    fonction_ask_ia = NULL,
    model_name = "mistral-small"
) {
  
  setwd(dir_etude)
  # Chargement des scripts nécessaires
  source(paste0(dir_codes, nom_code_setup), encoding="UTF-8")
  source(paste0(dir_codes, nom_code_preparation), encoding = "UTF-8")

  # Lecture et préparation des données
  table_demandes <- read.csv(paste0(dir_input, nom_fichier_demandes))
  table_demandes$var_evolution[is.na(table_demandes$var_evolution)] <- ""

  if(BoutadE::controler_demandes(table_demandes)%>%nrow() != 0) 
    stop("une demande n'est pas valide")

  # Lecture des métadonnées
  metadata <- NULL
  if(!is.null(nom_fichier_metadonnees)){
    metadata <- jsonlite::fromJSON(
      paste0(dir_input, nom_fichier_metadonnees),
      simplifyDataFrame = FALSE
    )
  }

  # Traitement des demandes
  out <- pbapply(table_demandes, 1, gerer_une_demande, metadata = metadata)

  # Génération du rapport
  if(!use_ia){
    generer_markdown_auto(
      table_demandes,
      out,
      output_dir = dir_output,
      nom_rapport = nom_rapport
    )
  } else {
    out_ia <- pbapply(
      table_demandes,
      1,
      gerer_une_demande,
      for_ollama = TRUE,
      metadata = metadata
    )
    
    generer_markdown_auto(
      table_demandes, 
      out,
      out_ollama = out_ia, 
      fonction_ask_ollama = fonction_ask_ia,
      metadata = metadata,
      prompt_header = NULL,
      prompt_instruction = NULL,
      model_name = model_name,
      output_dir = dir_output,
      nom_rapport = nom_rapport
    )
  }

  # Rendu du rapport
  rmarkdown::render(
    input = paste0(dir_output, nom_rapport),
    envir = globalenv()
  )
  
  # Retourne le chemin vers le rapport généré
  return(paste0("Le rapport a été généré dans ",paste0(dir_output, nom_rapport)))
}
