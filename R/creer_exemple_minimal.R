#' Créer un jeu de données minimal pour tester les fonctions du package
#'
#' @description
#' Génère des données fictives pour tester les fonctionnalités du package :
#' - Données d'entreprises profilées et leurs filiales
#' - Exemples de demandes valides et invalides
#' - Variables de croisement et d'agrégation
#'
#' @param n Nombre d'observations à générer (défaut: 8)
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{EP_FI_AG}{Data.frame des entreprises profilées et leurs filiales avec :
#'     \itemize{
#'       \item Variables d'identification (siren, dénomination)
#'       \item Variables de classification (département, secteur)
#'       \item Variables quantitatives (redi_r310)
#'       \item Variables calculées (dep_FI_relatif)
#'     }
#'   }
#'   \item{table_demandes_valides}{Data.frame de 20 demandes valides avec différentes combinaisons de :
#'     \itemize{
#'       \item Variables de croisement
#'       \item Fonctions d'agrégation
#'       \item Types de sortie (table/graphique)
#'     }
#'   }
#'   \item{table_demandes_erreurs}{Data.frame de 20 demandes invalides illustrant les cas d'erreur}
#' }
#'
#' @examples
#' # Générer les données de test
#' donnees_test <- creer_exemple_minimal()
#'
#' # Tester une demande valide
#' demande <- donnees_test$table_demandes_valides[1, ]
#' resultat <- gerer_une_demande(demande)
#'
#' # Tester une demande invalide
#' demande_erreur <- donnees_test$table_demandes_erreurs[1, ]
#' resultat_erreur <- gerer_une_demande(demande_erreur)
#'
#' @export
creer_table_minimale <- function(n = 8) {
  # Calcul du nombre d'observations par année
  n1 <- floor(n/2)  # première année
  n2 <- n - n1      # deuxième année
  
  # Création des données EP_FI_AG
  EP_FI_AG <- data.frame(
    annee = rep(c("2022", "2023"), c(n1, n2)),
    siren_EP = rep(c("123456789", "987654321"), length.out = n),
    denom_EP = rep(c("Entreprise A", "Entreprise B"), length.out = n),
    dep_EP = rep(c("971", "972"), length.out = n),
    s4_EP = rep(c("Industrie", "Services marchands"), length.out = n),
    redi_r310_EP = round(runif(n, 1000, 5000)),
    siren_FI = paste0("FI", 1:n),
    denom_FI = paste("Filiale", 1:n),
    dep_FI = rep(c("971", "972", "973", "974"), length.out = n),
    s4_FI = rep(c("Industrie", "Construction", "Commerces", "Services marchands"), length.out = n),
    redi_r310_FI = round(runif(n, 500, 2000)),
    poids = 1
  )
  
  # Ajout des variables calculées
  EP_FI_AG$dep_FI_relatif <- ifelse(EP_FI_AG$dep_FI == EP_FI_AG$dep_EP, 
                                   "interieur", 
                                   ifelse(EP_FI_AG$dep_FI %in% c("971", "972", "973"),
                                         "antilles-guyane",
                                         "autres"))

  EP_FI_AG
}

#' Créer des tables de demandes pour les tests
#'
#' @description
#' Génère deux tables de données pour tester le traitement des demandes :
#' - Une table de demandes valides avec différentes combinaisons de paramètres
#' - Une table de demandes invalides pour tester la gestion des erreurs
#'
#' @return Une liste contenant deux data.frames :
#' \describe{
#'   \item{table_demandes_valides}{20 exemples de demandes valides avec différentes combinaisons de :
#'     \itemize{
#'       \item Variables de croisement et relatives
#'       \item Variables quantitatives
#'       \item Fonctions d'agrégation
#'       \item Types de sortie (table/graphique)
#'     }
#'   }
#'   \item{table_demandes_erreurs}{20 exemples de demandes invalides illustrant différents cas d'erreur :
#'     \itemize{
#'       \item Variables inexistantes
#'       \item Combinaisons invalides
#'       \item Fonctions d'agrégation incorrectes
#'       \item Formats de sortie incompatibles
#'     }
#'   }
#' }
#'
#' @examples
#' # Créer les tables de test
#' tables <- creer_tables_demandes()
#'
#' # Accéder aux demandes valides
#' demandes_valides <- tables$table_demandes_valides
#'
#' # Accéder aux demandes avec erreurs
#' demandes_erreurs <- tables$table_demandes_erreurs
#'
#' @export
creer_tables_demandes <- function() {
  # Création d'une table de demandes valides (20 exemples)
  table_demandes_valides <- data.frame(
    id_demande = 1:20,
    table = rep("EP_FI_AG", 20),
    var_croisement = c(
      "annee-dep_EP", "s4_EP", "dep_EP", "s4_EP", "dep_EP-dep_FI",
      "s4_EP-s4_FI", "dep_EP", "s4_EP-dep_EP", "dep_EP", "s4_EP",
      "dep_EP-s4_FI", "s4_EP", "dep_EP-s4_EP", "s4_EP-dep_FI", "dep_EP",
      "s4_EP-s4_FI", "dep_EP-dep_FI", "s4_EP", "dep_EP", "annee-s4_EP"
    ),
    var_croisement_relative = c(
      "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif",
      "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif",
      "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif",
      "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", "dep_FI_relatif", ""
    ),
    var_quanti = c(
      "redi_r310_EP", "redi_r310_EP-redi_r310_FI", "redi_r310_EP", "redi_r310_FI", "redi_r310_EP",
      "redi_r310_EP-redi_r310_FI", "redi_r310_EP", "redi_r310_FI", "redi_r310_EP", "redi_r310_FI",
      "redi_r310_EP-redi_r310_FI", "redi_r310_EP", "redi_r310_FI", "redi_r310_EP", "redi_r310_FI",
      "redi_r310_EP-redi_r310_FI", "redi_r310_EP", "redi_r310_FI", "redi_r310_EP", "redi_r310_EP-redi_r310_FI"
    ),
    fonctions_agregations = c(
      "sum", "sum-mean", "sum", "sum", "sum",
      "mean-sum", "sum", "sum", "sum", "sum",
      "mean-sum", "sum", "sum", "sum", "sum",
      "mean-sum", "sum", "sum", "sum", "mean-max"
    ),
    condition = "",
    type_output = c(
      "table", "table", "graphique", "table", "table",
      "table", "table", "table", "table", "table",
      "table", "table", "table", "table", "table",
      "table", "table", "table", "table", "table"
    ),
    nom_fichier_xls = c(
      "rapport1.xlsx", "", "rapport3.xlsx", "", "rapport5.xlsx",
      "", "rapport7.xlsx", "", "rapport9.xlsx", "",
      "rapport11.xlsx", "", "rapport13.xlsx", "", "rapport15.xlsx",
      "", "rapport17.xlsx", "", "rapport19.xlsx", ""
    ),
    nom_onglet = paste0("analyse", 1:20),
    titre = paste("Analyse", c(
      "multi-indicateurs", "sectorielle", "départementale", "comparative",
      "statistique", "géographique", "temporelle", "structurelle",
      "détaillée", "globale", "spécifique", "agrégée", "ventilée",
      "croisée", "synthétique", "approfondie", "thématique",
      "consolidée", "segmentée", "stratégique"
    )),
    unite = c(
      "millier", "millier-unite", "millier", "unite", "millier",
      "unite-millier", "millier", "millier", "millier", "millier",
      "unite-millier", "millier", "millier", "millier", "unite",
      "unite-millier", "millier", "unite", "millier", "unite-millier"
    ),
    var_evolution = c(
      "annee", "", "", "", "",
      "", "", "", "", "",
      "", "", "", "", "",
      "", "", "", "", "annee"
    ),
    stringsAsFactors = FALSE
  )

  # Création d'une table de demandes avec erreurs (20 exemples)
  table_demandes_erreurs <- data.frame(
    id_demande = 1:20,
    table = rep("EP_FI_AG", 20),
    var_croisement = c(
      "dep_EP", "dep_inexistant", "dep_EP", "s4_EP", "dep_EP",
      "variable_invalide", "dep_EP", "s4_EP", "dep_EP", "s4_EP",
      "dep_inexistant", "s4_EP", "dep_EP", "s4_EP", "dep_EP",
      "s4_EP", "dep_EP", "variable_invalide", "dep_EP", "s4_EP"
    ),
    var_croisement_relative = c(
      "s4_FI", "", "invalide", "s4_FI", "",
      "s4_FI", "invalide", "", "s4_FI", "",
      "s4_FI", "invalide", "", "s4_FI", "invalide",
      "", "s4_FI", "invalide", "", "s4_FI"
    ),
    var_quanti = c(
      "redi_r310_EP-invalide",                    # variable invalide
      "redi_r310_EP",                             # une seule variable avec deux unités
      "variable_inexistante",                      # variable inexistante
      "redi_r310_EP-redi_r310_FI",                # deux variables trois unités
      "redi_r310_EP-invalide",                    # variable invalide
      "redi_r310_EP",                             # sum sans var relative
      "invalide-redi_r310_FI",                    # variable invalide
      "redi_r310_EP-redi_r310_FI",                # mauvais nombre d'unités
      "variable_inexistante",                      # variable inexistante
      "redi_r310_EP-invalide",                    # variable invalide
      "redi_r310_EP",                             # une variable deux unités
      "invalide-redi_r310_FI",                    # variable invalide
      "redi_r310_EP-redi_r310_FI",                # mauvais nombre d'unités
      "variable_inexistante",                      # variable inexistante
      "redi_r310_EP-invalide",                    # variable invalide
      "redi_r310_EP",                             # sum sans var relative
      "invalide-redi_r310_FI",                    # variable invalide
      "redi_r310_EP-redi_r310_FI",                # mauvais nombre d'unités
      "variable_inexistante",                      # variable inexistante
      "redi_r310_EP-invalide"                     # variable invalide
    ),
    fonctions_agregations = c(
      "mean-invalid", "sum", "mean-var", "mean-median-invalid",
      "sum", "invalid", "mean-var", "sum-invalid",
      "mean-invalid", "sum", "invalid-var", "mean-median",
      "sum-invalid", "mean-var", "invalid", "sum",
      "mean-invalid", "invalid-median", "sum", "invalid-var"
    ),
    condition = "",
    type_output = rep(c("table", "graphique"), 10),
    nom_fichier_xls = c(
      "", "rapport1.xlsx", "", "rapport3.xlsx", "",
      "rapport5.xlsx", "", "rapport7.xlsx", "", "rapport9.xlsx",
      "", "rapport11.xlsx", "", "rapport13.xlsx", "",
      "rapport15.xlsx", "", "rapport17.xlsx", "", "rapport19.xlsx"
    ),
    nom_onglet = paste0("erreur", 1:20),
    titre = paste("Test erreur", 1:20),
    unite = c(
      "million", "million-millier", "unite", "million-millier-unite",
      "million", "millier", "unite-millier", "million",
      "million-unite", "millier", "unite", "million-millier",
      "million", "unite-millier", "million", "millier",
      "unite-millier", "million", "million-unite", "millier"
    ),
    var_evolution = c(
      "annee", "annee", "", "periode_invalide", "annee",
      "", "AN", "annee", "", "periode_invalide",
      "annee", "", "AN", "periode_invalide", "",
      "annee", "AN", "", "periode_invalide", "annee"
    ),
    stringsAsFactors = FALSE
  )

  # Retourner la liste des objets créés
  return(list(
    table_demandes_valides = table_demandes_valides,
    table_demandes_erreurs = table_demandes_erreurs
  ))
}
