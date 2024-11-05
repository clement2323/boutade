#' Contrôler la validité des demandes d'agrégation
#'
#' @description
#' Vérifie la cohérence et la validité des demandes d'agrégation en contrôlant :
#' - La cohérence entre fonctions, variables et unités
#' - L'existence des fonctions et variables
#' - Les règles métier (ex: somme avec variable relative)
#' - La compatibilité des types de sortie
#'
#' @param table_demandes data.frame contenant les demandes à contrôler avec les colonnes :
#'   \describe{
#'     \item{id_demande}{Identifiant unique de la demande}
#'     \item{fonctions_agregations}{Liste de fonctions séparées par "-"}
#'     \item{var_quanti}{Liste de variables séparées par "-"}
#'     \item{unite}{Liste d'unités séparées par "-"}
#'     \item{var_croisement}{Variables de croisement (optionnel)}
#'     \item{var_croisement_relative}{Variable pour calcul relatif (optionnel)}
#'     \item{type_output}{Type de sortie ("table" ou "graphique")}
#'   }
#'
#' @return data.frame des erreurs détectées avec les colonnes :
#'   \describe{
#'     \item{id_demande}{Identifiant de la demande en erreur}
#'     \item{raison}{Description détaillée de l'erreur}
#'   }
#'
#' @examples
#' # Créer des données de test
#' donnees <- creer_exemple_minimal()
#' 
#' # Contrôler des demandes valides
#' erreurs_valides <- controler_demandes(donnees$table_demandes_valides)
#' 
#' # Contrôler des demandes avec erreurs
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
    
    # 4. Output graphique incompatible avec sum
    if((!identical(fonctions,"sum")) && demande$type_output == "graphique") {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "Output graphique non autorisé si pas de fonction sum uniquement",
        stringsAsFactors = FALSE
      ))
    }
    
    # 5. Pas plus d'une variable de croisement pour un output graphique
    if(demande$type_output == "graphique" && demande$var_croisement != "") {
      vars_croisement <- strsplit(demande$var_croisement, "-")[[1]]
      if(length(vars_croisement) > 1) {
        erreurs <- rbind(erreurs, data.frame(
          id_demande = demande$id_demande,
          raison = "Output graphique non autorisé avec plus d'une variable de croisement",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # 6. Vérification de l'existence de la table et des variables
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
    
    # Variables quantitatives
    vars_quanti_manquantes <- variables[!variables %in% names(table_donnees)]
    if(length(vars_quanti_manquantes) > 0) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = sprintf("Variable(s) quantitative(s) non trouvée(s): %s",
                        paste(vars_quanti_manquantes, collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }
    
    # Variables de croisement
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
    
    # Variable de croisement relative
    if(demande$var_croisement_relative != "" && 
       !demande$var_croisement_relative %in% names(table_donnees)) {
      erreurs <- rbind(erreurs, data.frame(
        id_demande = demande$id_demande,
        raison = "Variable de croisement relative non trouvée",
        stringsAsFactors = FALSE
      ))
    }
    
          
  # Vérifier que la variable d'évolution est dans les variables de croisement
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
