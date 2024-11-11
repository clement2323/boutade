#' Formater les métadonnées des variables
#' @param metadata Liste des métadonnées des tables
#' @param nom_table Nom de la table
#' @return Chaîne formatée des descriptions des variables
format_variables_metadata <- function(metadata, nom_table) {
  vars_table <- metadata[metadata$nom_table == nom_table, ]
  
  descriptions <- vapply(seq_len(nrow(vars_table)), function(i) {
    sprintf("- %s (%s) : %s", 
            vars_table$libelle_court[i],
            vars_table$nom_variable[i], 
            vars_table$description[i])
  }, character(1))
  
  paste(descriptions, collapse = "\n")
}

#' Adapter le vocabulaire selon le public cible
#' @param texte Texte à adapter
#' @param vocabulaire_adapte Liste des correspondances de vocabulaire
#' @return Texte avec vocabulaire adapté
adapter_vocabulaire <- function(texte, vocabulaire_adapte) {
  if (is.null(vocabulaire_adapte)) return(texte)
  
  for (terme in names(vocabulaire_adapte)) {
    texte <- gsub(terme, vocabulaire_adapte[[terme]], texte, fixed = TRUE)
  }
  texte
}

#' Générer l'en-tête du prompt selon le public cible
#' @param metadonnees_etude Métadonnées de l'étude
#' @return Texte d'en-tête formaté
generer_entete_prompt <- function(metadonnees_etude) {
  paste0(
    "Je vais te décrire des données concernant ", 
    metadonnees_etude$description, ".\n\n",
    "Je souhaite que tu analyses ces données pour ", 
    metadonnees_etude$public_cible$type, 
    " en utilisant un style ", 
    metadonnees_etude$public_cible$instructions_communication$style,
    ".\n\n"
  )
}

#' Préparer un prompt pour l'analyse de données
#' @param table Table de données à analyser
#' @param metadonnees_tables Métadonnées des tables
#' @param metadonnees_etude Métadonnées de l'étude
#' @return Chaîne de caractères contenant le prompt complet
#' @export
preparer_prompt <- function(table, metadonnees_tables, metadonnees_etude) {
  # Générer l'en-tête
  prompt <- generer_entete_prompt(metadonnees_etude)
  
  # Ajouter les métadonnées des variables
  nom_table <- unique(metadonnees_tables$nom_table)[1]
  vars_desc <- format_variables_metadata(metadonnees_tables, nom_table)
  prompt <- paste0(prompt, "Voici les variables et leur signification :\n", vars_desc, "\n\n")
  
  # Ajouter les données
  donnees_texte <- df_to_text(table)
  prompt <- paste0(prompt, "Voici les données :\n", donnees_texte, "\n\n")
  
  # Ajouter les instructions d'analyse
  instructions <- paste(
    "INSTRUCTION :",
    "Décris les points importants que l'on peut observer dans ces données.",
    "Utilise un langage simple et direct.",
    paste(metadonnees_etude$public_cible$instructions_communication$directives, 
          collapse = ". "),
    sep = "\n"
  )
  prompt <- paste0(prompt, instructions)
  
  # Adapter le vocabulaire
  prompt <- adapter_vocabulaire(
    prompt, 
    metadonnees_etude$public_cible$instructions_communication$vocabulaire_adapte
  )
  
  prompt
}