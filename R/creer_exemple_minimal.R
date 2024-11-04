#' Créer un jeu de données minimal pour tester les fonctions du package
#'
#' Cette fonction génère des données fictives représentatives pour tester les fonctionnalités
#' du package, notamment les fonctions de gestion des demandes et de création de rapports.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item \code{EP_FI_AG} : Un data.frame simulant les données des entreprises profilées
#'   \item \code{FI_EP_AG} : Un data.frame simulant les données des filiales
#'   \item \code{vecteur_demande} : Un vecteur de demande exemple
#'   \item \code{table_demandes} : Un data.frame contenant plusieurs demandes exemple
#' }
#'
#' @examples
#' donnees_test <- creer_exemple_minimal()
#' 
#' # Utiliser le vecteur de demande
#' resultat <- gerer_une_demande(donnees_test$vecteur_demande)
#'
#' # Utiliser la table de demandes
#' resultats <- lapply(1:nrow(donnees_test$table_demandes), 
#'                     function(i) gerer_une_demande(donnees_test$table_demandes[i,]))
#'
#' @export
creer_exemple_minimal <- function() {
  # Création des données EP_FI_AG
  EP_FI_AG <- data.frame(
    siren_EP = rep(c("123456789", "987654321"), each = 4),
    denom_EP = rep(c("Entreprise A", "Entreprise B"), each = 4),
    dep_EP = rep(c("971", "972"), each = 4),
    s4_EP = rep(c("Industrie", "Services marchands"), each = 4),
    redi_r310_EP = round(runif(8, 1000, 5000)),
    siren_FI = paste0("FI", 1:8),
    denom_FI = paste("Filiale", 1:8),
    dep_FI = rep(c("971", "972", "973", "974"), 2),
    s4_FI = rep(c("Industrie", "Construction", "Commerces", "Services marchands"), 2),
    redi_r310_FI = round(runif(8, 500, 2000)),
    poids = 1
  )
  
  # Ajout des variables calculées
  EP_FI_AG$dep_FI_relatif <- ifelse(EP_FI_AG$dep_FI == EP_FI_AG$dep_EP, 
                                   "interieur", 
                                   ifelse(EP_FI_AG$dep_FI %in% c("971", "972", "973"),
                                         "antilles-guyane",
                                         "autres"))
  
  # Création des données FI_EP_AG (vue inverse)
  FI_EP_AG <- EP_FI_AG[, c("siren_FI", "siren_EP", "dep_FI", "dep_EP", "s4_FI", "s4_EP",
                           "redi_r310_FI", "redi_r310_EP", "poids")]
  
  # Création d'un vecteur de demande exemple
  vecteur_demande <- c(
    id_demande = "1",
    table = "EP_FI_AG",
    var_croisement = "dep_EP",
    var_croisement_relative = "s4_FI",
    var_quanti = "redi_r310_EP",
    fonctions_agregations = "sum",
    condition = "",
    type_output = "graphique",
    nom_fichier_xls = "",
    nom_onglet = "cadrage",
    titre = "Répartition du CA des EP par branche",
    unite = "million"
  )
  
  # Création d'une table de demandes exemple
  table_demandes <- data.frame(
    id_demande = 1:3,
    table = c("EP_FI_AG", "EP_FI_AG", "FI_EP_AG"),
    var_croisement = c("dep_EP", "dep_EP", "dep_FI"),
    var_croisement_relative = c("s4_FI", "dep_FI_relatif", "s4_EP"),
    var_quanti = c("redi_r310_EP", "poids", "redi_r310_FI"),
    fonctions_agregations = c("sum", "sum", "sum"),
    condition = "",
    type_output = c("graphique", "table", "graphique"),
    nom_fichier_xls = c("", "rapport.xlsx", ""),
    nom_onglet = c("cadrage", "structure", "analyse"),
    titre = c(
      "Répartition du CA par branche",
      "Structure par département",
      "CA des filiales par secteur"
    ),
    unite = c("million", "unite", "million"),
    stringsAsFactors = FALSE
  )
  
  # Retourner la liste des objets créés
  return(list(
    EP_FI_AG = EP_FI_AG,
    FI_EP_AG = FI_EP_AG,
    vecteur_demande = vecteur_demande,
    table_demandes = table_demandes
  ))
}