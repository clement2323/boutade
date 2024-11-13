#' Tables de demandes pour l'analyse des entreprises antillaises
#'
#' @format ## table_demandes_rmd
#' Un data frame avec 20 lignes et 13 colonnes:
#' \describe{
#'   \item{id_demande}{Identifiant unique de la demande}
#'   \item{table}{Nom de la table source (EP_FI_AG)}
#'   \item{condition}{Conditions de filtrage}
#'   \item{var_croisement}{Variables de croisement pour l'analyse}
#'   \item{var_croisement_relative}{Variable relative pour le croisement}
#'   \item{var_evolution}{Variable d'évolution temporelle}
#'   \item{var_quanti}{Variables quantitatives à analyser}
#'   \item{unite}{Unité de mesure ('millier' ou 'unite')}
#'   \item{fonctions_agregations}{Fonctions d'agrégation utilisées (sum, mean, max)}
#'   \item{titre}{Titre de l'analyse}
#'   \item{partie}{Section principale du rapport}
#'   \item{sous_partie}{Sous-section du rapport}
#'   \item{type_figure}{Type de visualisation ('kable' ou 'graphique')}
#' }
#' @source Données internes INSEE
"table_demandes_rmd"

#' Données de relations Entreprises-Filiales en Antilles-Guyane
#'
#' @format ## EP_FI_AG
#' Un data frame avec 60 lignes et 13 colonnes:
#' \describe{
#'   \item{annee}{Année d'observation (2022 ou 2023)}
#'   \item{siren_EP}{Numéro SIREN de l'entreprise principale}
#'   \item{denom_EP}{Dénomination de l'entreprise principale}
#'   \item{dep_EP}{Département de l'entreprise principale (971, 972)}
#'   \item{s4_EP}{Secteur d'activité de l'EP (Industrie, Services marchands)}
#'   \item{redi_r310_EP}{Montant aléatoire pour l'EP (1000-5000)}
#'   \item{siren_FI}{Identifiant de la filiale (FI1 à FI60)}
#'   \item{denom_FI}{Dénomination de la filiale}
#'   \item{dep_FI}{Département de la filiale (971-974)}
#'   \item{s4_FI}{Secteur d'activité de la filiale}
#'   \item{redi_r310_FI}{Montant aléatoire pour la filiale (500-2000)}
#'   \item{poids}{Poids statistique (1)}
#'   \item{dep_FI_relatif}{Position relative de la filiale (interieur, antilles-guyane, autres)}
#' }
"EP_FI_AG"

#' Table des demandes valides pour l'analyse
#'
#' @format ## table_demandes_valides
#' Un data frame avec 20 lignes et 11 colonnes:
#' \describe{
#'   \item{id_demande}{Identifiant unique de la demande (1-20)}
#'   \item{table}{Nom de la table source (EP_FI_AG)}
#'   \item{condition}{Conditions de filtrage (vide)}
#'   \item{var_croisement}{Variables de croisement pour l'analyse}
#'   \item{var_croisement_relative}{Variable de croisement relative (dep_FI_relatif)}
#'   \item{var_evolution}{Variable d'évolution temporelle}
#'   \item{var_quanti}{Variables quantitatives à analyser}
#'   \item{unite}{Unités de mesure (millier, unite)}
#'   \item{fonctions_agregations}{Fonctions d'agrégation à appliquer}
#'   \item{nom_fichier_xls}{Nom du fichier Excel de sortie}
#'   \item{nom_onglet}{Nom de l'onglet (analyse)}
#'   \item{titre}{Titre de l'analyse}
#' }
"table_demandes_valides"

#' Table des demandes avec erreurs pour tests
#'
#' @format ## table_demandes_erreurs
#' Un data frame avec 20 lignes et 11 colonnes de structure identique à table_demandes_valides:
#' \describe{
#'   \item{id_demande}{Identifiant unique de la demande (1-20)}
#'   \item{table}{Nom de la table source (EP_FI_AG)}
#'   \item{condition}{Conditions de filtrage (vide)}
#'   \item{var_croisement}{Variables de croisement incluant des erreurs}
#'   \item{var_croisement_relative}{Variables relatives avec erreurs}
#'   \item{var_evolution}{Variables d'évolution avec erreurs}
#'   \item{var_quanti}{Variables quantitatives avec erreurs}
#'   \item{unite}{Unités de mesure avec incohérences}
#'   \item{fonctions_agregations}{Fonctions d'agrégation invalides}
#'   \item{nom_fichier_xls}{Nom du fichier Excel de sortie}
#'   \item{nom_onglet}{Noms d'onglets (erreur1-20)}
#'   \item{titre}{Titres des tests d'erreur}
#' }
#' 
#' @note Cette table contient intentionnellement des erreurs pour tester la robustesse des fonctions d'analyse
"table_demandes_erreurs"


#' Métadonnées des tables de l'étude EP-FI Antilles-Guyane
#'
#' @format Un data.frame contenant les descriptions des variables:
#' \describe{
#'   \item{nom_etude}{Nom de l'étude}
#'   \item{nom_table}{Nom de la table de données}
#'   \item{nom_variable}{Identifiant technique de la variable}
#'   \item{libelle}{Description complète de la variable}
#'   \item{libelle_court}{Version abrégée du libellé}
#'   \item{description}{Description détaillée de la variable}
#' }
"metadonnees_tables"

#' Métadonnées générales de l'étude EP-FI Antilles-Guyane
#'
#' @format Une liste imbriquée contenant:
#' \describe{
#'   \item{nom}{Nom de l'étude}
#'   \item{description}{Présentation générale}
#'   \item{public_cible}{
#'     \itemize{
#'       \item type: Public visé
#'       \item instructions_communication: Directives de communication
#'       \item vocabulaire_adapte: Mapping des termes techniques
#'     }
#'   }
#' }
#' @examples
#' metadonnees_etude$public_cible$vocabulaire_adapte$entreprise_profilée
"metadonnees_etude"

#' Métadonnées de rôle pour la communication
#'
#' Un jeu de données contenant les paramètres de communication adaptés 
#' pour un public cible spécifique.
#'
#' @format Une liste avec 2 éléments principaux :
#' \describe{
#'   \item{type}{Type de public cible}
#'   \item{instructions_communication}{Liste contenant :
#'     \itemize{
#'       \item style : Style de communication à adopter
#'       \item directives : Vecteur des règles de communication
#'       \item vocabulaire_adapte : Liste des traductions de termes techniques
#'     }
#'   }
#' }
"metadonnees_role"

#' Métadonnées de l'étude
#'
#' Un jeu de données contenant les informations générales sur l'étude.
#'
#' @format Une liste avec 2 éléments :
#' \describe{
#'   \item{nom}{Nom de l'étude}
#'   \item{description}{Description détaillée de l'étude}
#' }
"metadonnees_etude"