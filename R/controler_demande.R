#' Contrôler la validité des demandes d'agrégation
#'
#' @description
#' Vérifie la validité des demandes d'agrégation selon plusieurs critères :
#' 1. Cohérence des dimensions (fonctions/variables/unités)
#' 2. Existence des fonctions et variables
#' 3. Règles métier spécifiques
#' 4. Cohérence des variables de croisement
#'
#' @param table_demandes data.frame avec colonnes :
#'   - id_demande : Identifiant unique
#'   - fonctions_agregations : Fonctions séparées par "-"
#'   - var_quanti : Variables séparées par "-"
#'   - unite : Unités séparées par "-"
#'   - var_croisement : Variables de croisement (optionnel)
#'   - var_croisement_relative : Variable pour calcul relatif (optionnel)
#'   - var_evolution : Variable d'évolution temporelle (optionnel)
#'   - type_output : "table" ou "graphique"
#'
#' @return data.frame des erreurs avec colonnes id_demande et raison
#'
#' @examples
#' donnees <- creer_exemple_minimal()
#' erreurs_valides <- controler_demandes(donnees$table_demandes_valides)
#' erreurs_invalides <- controler_demandes(donnees$table_demandes_erreurs)
#'
#' @export
controler_demandes <- function(table_demandes) {
# table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_valide
  # Initialisation du data.frame des erreurs
  erreurs <- data.frame(
    id_demande = integer(0),
    raison = character(0),
    stringsAsFactors = FALSE
  )
  
  # Contrôle de chaque ligne
  for(i in 1:nrow(table_demandes)) {
    demande <- table_demandes[i, ]
    # i <-1
    # Séparation des éléments multiples
    fonctions <- strsplit(demande$fonctions_agregations, "-")[[1]]
    variables <- strsplit(demande$var_quanti, "-")[[1]]
    unites <- strsplit(demande$unite, "-")[[1]]
    
    # Vérifications
    
    # 1. Nombre cohérent de fonctions/variables/unités
    if(length(fonctions) != length(variables) || length(fonctions) != length(unites)) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "Nombre incohérent de fonctions/variables/unités",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # 2. Fonctions existent dans l'environnement global
    fonctions_inexistantes <- fonctions[!sapply(fonctions, exists, mode = "function")]
    if(length(fonctions_inexistantes) > 0) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = sprintf("Fonction(s) non définie(s): %s", 
                        paste(fonctions_inexistantes, collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }
    
    # 3. Pas de somme sans var_croisement_relative
    if("sum" %in% fonctions && demande$var_croisement_relative == "") {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "Somme non autorisée sans variable de croisement relative",
        stringsAsFactors = FALSE
      ))
    }
    
    # 4. Vérification de l'existence de la table et des variables
    # Vérification de l'existence de la table
    nom_table <- table_demandes[i,]$table
    if (!exists(nom_table)) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = sprintf("Table '%s' non trouvée dans l'environnement global", nom_table),
        stringsAsFactors = FALSE
      ))
      next
    }
    
    table_donnees <- get(nom_table)
    
    # 5. Vérification des variables quantitatives
    vars_quanti_manquantes <- variables[!variables %in% names(table_donnees)]
    if(length(vars_quanti_manquantes) > 0) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = sprintf("Variable(s) quantitative(s) non trouvée(s): %s",
                        paste(vars_quanti_manquantes, collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }
    
    # 6. Vérification des variables de croisement
    if(demande$var_croisement != "") {
      vars_croisement <- strsplit(demande$var_croisement, "-")[[1]]
      vars_croisement_manquantes <- vars_croisement[!vars_croisement %in% names(table_donnees)]
      if(length(vars_croisement_manquantes) > 0) {
        erreurs <- rbind(erreurs, data.frame(
          id_demande = demande$id_demande,
          raison = sprintf("Variable(s) de croisement non trouvée(s): %s",
                          paste(vars_croisement_manquantes, collapse = ", ")),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # 7. Vérification de la variable relative
    if(demande$var_croisement_relative != "" && 
       !demande$var_croisement_relative %in% names(table_donnees)) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "Variable de croisement relative non trouvée",
        stringsAsFactors = FALSE
      ))
    }
    
    # 8. Vérification de la variable d'évolution
    if(demande$var_evolution != "" &  
       !grepl(demande$var_evolution, demande$var_croisement, fixed = TRUE)) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "La variable d'évolution doit être incluse dans les variables de croisement",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(erreurs)
}
