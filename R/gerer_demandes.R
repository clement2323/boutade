#' Gérer une demande d'agrégation
#'
#' Cette fonction traite une demande spécifique d'agrégation en récupérant les informations nécessaires depuis une table de demandes, puis en calculant l'agrégat en fonction des paramètres spécifiés.
#'
#' @param numero_ligne Un entier indiquant le numéro de la ligne dans \code{table_demandes} correspondant à la demande à traiter.
#' @param table_demandes Un \code{data.frame} contenant les demandes d'agrégation. Chaque ligne doit inclure les colonnes nécessaires telles que \code{table}, \code{var_croisement}, \code{var_croisement_relative}, \code{fonctions_agregations}, \code{condition}, et \code{var_quanti}.
#'
#' @return Un \code{data.frame} contenant le résultat de l'agrégation calculée.
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Récupère les informations de la ligne spécifiée dans \code{table_demandes}.
#'   \item Charge la table de données correspondante en utilisant \code{get()}.
#'   \item Extrait les variables de croisement et les fonctions d'agrégation.
#'   \item Évalue la condition fournie pour filtrer les données si nécessaire.
#'   \item Calcule l'agrégat en appelant la fonction \code{calculer_agregat_sur_croisement()} avec les paramètres extraits.
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation de gerer_une_demande
#' # Supposons que table_demandes soit un data.frame correctement formaté
#' resultat <- gerer_une_demande(numero_ligne = 1, table_demandes = table_demandes)
#' }
#'
#' @seealso \code{\link{calculer_agregat_sur_croisement}}
#'
#' @export
gerer_une_demande <- function(numero_ligne, table_demandes) {
  # Récupérer les informations de la ligne spécifiée
  selection <- table_demandes[numero_ligne, ]
  
  # Charger la table de données correspondante
  table <- get(selection$table)
  
  # Extraire les variables de croisement
  var_croisement_relative <- selection$var_croisement_relative
  var_croisement <- strsplit(selection$var_croisement, "-")[[1]]
  
  # Extraire les fonctions d'agrégation
  vecteur_nom_fonction <- strsplit(selection$fonctions_agregations, "-")[[1]]
  liste_fonction_agregation <- lapply(vecteur_nom_fonction, get)
  names(liste_fonction_agregation) <- vecteur_nom_fonction
  
  # Évaluer la condition si elle existe
  condition_texte <- selection$condition
  condition <- with(table, eval(parse(text = condition_texte)))
  if (is.na(condition)) condition <- NULL
  
  var_quanti <- selection$var_quanti
  
  # Calculer l'agrégat
  table_agrege <- calculer_agregat_sur_croisement(
    table = table,
    var_croisement = var_croisement,
    var_croisement_relative = var_croisement_relative,
    var_quanti = var_quanti,
    liste_fonction_agregation = liste_fonction_agregation,
    condition = condition
  )
  
  # Retourner le résultat
  return(table_agrege)
}
