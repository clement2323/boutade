#' Convertir un data.frame en texte CSV
#' @param df data.frame à convertir
#' @return Chaîne de caractères au format CSV
#' @export
df_to_text <- function(df) {
    # Convertir les noms de colonnes en texte
    header <- paste(colnames(df), collapse = ",")

    # Convertir chaque ligne en texte
    rows <- apply(df, 1, function(row) {
    paste(row, collapse = ",")
    })

    # Combiner l'en-tête et les lignes avec des sauts de ligne
    text <- paste(c(header, rows), collapse = "\n")

    return(text)
}

#' Formater les métadonnées des variables
#' @param metadata Liste des métadonnées des tables
#' @param nom_table Nom de la table
#' @return Chaîne formatée des descriptions des variables
format_variables_metadata <- function(metadonnees_tables, params) {

  var_interet <- c(
    params$var_croisement,
    params$var_croisement_relative,
    params$var_quanti,
    params$autres$var_evolution
    )
  nom_table_init <- params$table
  vars_table <-  metadonnees_tables%>%
    filter(nom_table == nom_table_init)%>%
    filter(nom_variable %in% var_interet)
      
  descriptions <- vapply(seq_len(nrow(vars_table)), function(i) {
    sprintf("- %s (%s) : %s", 
            vars_table$libelle_court[i],
            vars_table$nom_variable[i], 
            vars_table$description[i])
  }, character(1))
  
  paste(descriptions, collapse = "\n")
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
preparer_prompt <- function(table_agrege, metadonnees_tables,metadonnees_etude,params) {
  # Générer l'en-tête
  prompt <- generer_entete_prompt(metadonnees_etude)

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
    "Décris les points importants que l'on peut observer dans ces données.",
    "Utilise un langage simple et direct. Tu dois être le plus concis possible, un paragraphe maximum",
    paste(metadonnees_etude$public_cible$instructions_communication$directives, 
          collapse = ". "),
    sep = "\n"
  )
  prompt <- paste0(prompt, instructions)
  
  vocabulaire_adapte <- metadonnees_etude$public_cible$instructions_communication$vocabulaire_adapte
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
  prompt
}   
