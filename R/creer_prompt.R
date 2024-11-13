#' Convertir un data.frame en texte CSV
#' @param df data.frame à convertir
#' @return Chaîne de caractères au format CSV
#' @export
df_to_text <- function(df) {
    paste(c(paste(colnames(df), collapse = ","),
            apply(df, 1, paste, collapse = ",")), 
          collapse = "\n")
}

#' Formater les métadonnées des variables
#' @param metadata Liste des métadonnées des tables
#' @param params Paramètres de configuration
#' @return Chaîne formatée des descriptions des variables
format_variables_metadata <- function(metadonnees_tables, params) {
    var_interet <- unlist(c(params$var_croisement, 
                           params$var_croisement_relative,
                           params$var_quanti, 
                           params$autres$var_evolution))
    
    vars_table <- metadonnees_tables %>%
        filter(nom_table == params$table, nom_variable %in% var_interet) %>%
        with(sprintf("- %s (%s) : %s", libelle_court, nom_variable, description))
    
    paste(vars_table, collapse = "\n")
}

#' Générer l'en-tête du prompt selon le public cible
#' @param metadonnees_etude Métadonnées de l'étude
#' @param metadonnees_role Métadonnées du rôle
#' @return Texte d'en-tête formaté
generer_entete_prompt <- function(metadonnees_etude, metadonnees_role) {
    sprintf("Je vais te décrire des données concernant %s.\n\nJe souhaite que tu analyses ces données pour %s en utilisant un style %s.\n\n",
            metadonnees_etude$description,
            metadonnees_role$type,
            metadonnees_role$instructions_communication$style)
}

#' Préparer un prompt pour l'analyse de données
#' @param table Table de données à analyser
#' @param metadonnees_tables Métadonnées des tables
#' @param metadonnees_etude Métadonnées de l'étude
#' @return Chaîne de caractères contenant le prompt complet
#' @export
preparer_prompt <- function(table_agrege, metadonnees_tables,metadonnees_etude,metadonnee_role,params) {
  # Générer l'en-tête
  prompt <- generer_entete_prompt(metadonnees_etude,metadonnees_role)

  contexte <- sprintf(
    "Cette analyse s'inscrit dans la partie '%s', sous-partie '%s'%s.\n\n",
    params$rmd$partie,
    params$rmd$sous_partie,
    ifelse(!is.null(params$autres$titre), 
           sprintf(", et concerne plus précisément '%s'", params$autres$titre),
           "")
  )
  prompt <- paste0(prompt, contexte)
  
  # Ajouter les métadonnées des variables
  nom_table_init <- params$table
  
  vars_desc <- format_variables_metadata(metadonnees_tables, params)
  prompt <- paste0(prompt, "Voici les variables et leur signification :\n", vars_desc, "\n\n")
  
  # Ajouter les données
  donnees_texte <- df_to_text(table_agrege)
  prompt <- paste0(prompt, "Voici les données :\n", donnees_texte, "\n\n")
  
  # Ajouter les instructions d'analyse
  instructions <- paste(
    "INSTRUCTION :",
    "Décris les points importants que l'on peut observer dans ces données.
     Ne fais jamais mention des données en tant que telles décris juste les faits que tu observes.",
    paste(metadonnees_role$instructions_communication$directives, 
          collapse = "\n"),
    sep = "\n"
  )
  prompt <- paste0(prompt, instructions)
  
  vocabulaire_adapte <- metadonnees_role$instructions_communication$vocabulaire_adapte
  equivalences <- paste(
    "\nPour mieux comprendre, voici les équivalences de termes :",
    paste(
    vapply(names(vocabulaire_adapte), function(terme) {
        sprintf("- %s = %s", terme, vocabulaire_adapte[[terme]])
    }, character(1)),
    collapse = "\n"
    ),
    "\n",
    sep = "\n"
  )
  prompt <- paste0(
    prompt, 
    equivalences
  )
  prompt  # prompt |>cat()
}   
