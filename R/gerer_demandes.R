#' Gérer une demande d'agrégation à partir d'un vecteur de paramètres
#'
#' Cette fonction traite une demande spécifique d'agrégation en utilisant un vecteur de paramètres. Elle récupère les informations nécessaires, calcule l'agrégat en fonction des paramètres spécifiés et retourne le résultat.
#'
#' @param vecteur_demande Un vecteur contenant les paramètres de la demande. Les éléments du vecteur doivent être dans l'ordre suivant :
#' \enumerate{
#'   \item \code{id_demande} : Identifiant de la demande.
#'   \item \code{table} : Nom de la table de données à utiliser.
#'   \item \code{var_croisement} : Variables de croisement, séparées par des tirets.
#'   \item \code{var_croisement_relative} : Variable de croisement relative (unique).
#'   \item \code{var_quanti} : Variable quantitative sur laquelle effectuer les agrégations.
#'   \item \code{fonctions_agregations} : Fonctions d'agrégation à appliquer, séparées par des tirets.
#'   \item \code{condition} : Condition à appliquer pour filtrer les données (expression R sous forme de texte).
#'   \item \code{type_output} : Type de sortie souhaité (par exemple, \code{"table"}, \code{"graph"}).
#'   \item \code{nom_fichier_xls} : Nom du fichier Excel de sortie.
#'   \item \code{nom_onglet} : Nom de l'onglet dans le fichier Excel.
#'   \item \code{titre} : Titre pour la sortie (par exemple, titre du graphique).
#' }
#'
#' @return Un \code{data.frame} contenant le résultat de l'agrégation calculée.
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \enumerate{
#'   \item Décompose le \code{vecteur_demande} en variables individuelles.
#'   \item Charge la table de données correspondante en utilisant \code{get()}.
#'   \item Extrait les variables de croisement et les fonctions d'agrégation.
#'   \item Évalue la condition fournie pour filtrer les données si nécessaire.
#'   \item Calcule l'agrégat en appelant la fonction \code{calculer_agregat_sur_croisement()} avec les paramètres extraits.
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation de gerer_une_demande
#' # Supposons que vecteur_demande soit un vecteur correctement formaté
#' vecteur_demande <- c(
#'   id_demande = 1,
#'   table = "donnees_ventes",
#'   var_croisement = "region-produit",
#'   var_croisement_relative = "region",
#'   var_quanti = "chiffre_affaires",
#'   fonctions_agregations = "sum-mean",
#'   condition = "annee == 2023",
#'   type_output = "table",
#'   nom_fichier_xls = "rapport_ventes.xlsx",
#'   nom_onglet = "Synthèse",
#'   titre = "Rapport des ventes 2023"
#' )
#' resultat <- gerer_une_demande(vecteur_demande)
#' }
#'
#' @seealso \code{\link{calculer_agregat_sur_croisement}}
#'
#' @export
gerer_une_demande <- function(vecteur_demande) {
  # vecteur_demande <- unlist(table_demandes[1,])
  
  c(
    id_demande,
    table,
    var_croisement,
    var_croisement_relative,
    var_quanti,
    fonctions_agregations,
    condition,
    type_output,
    nom_fichier_xls,
    nom_onglet,
    titre
  ) %<-% vecteur_demande
  # Charger la table de données correspondante
  table <- get(table)
  
  # Extraire les variables de croisement
  var_croisement <- strsplit(var_croisement, "-")[[1]]
  
  # Extraire les fonctions d'agrégation
  vecteur_nom_fonction <- strsplit(fonctions_agregations, "-")[[1]]
  liste_fonction_agregation <- lapply(vecteur_nom_fonction, get)
  names(liste_fonction_agregation) <- vecteur_nom_fonction
  
  # Évaluer la condition si elle existe
  condition_texte <- condition
  condition <- with(table, eval(parse(text = condition_texte)))
  if (is.na(condition)) condition <- NULL
  
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
