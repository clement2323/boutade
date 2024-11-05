#' Gerer une demande d'agregation a partir d'un vecteur de parametres
#'
#' Cette fonction traite une demande specifique d'agregation en utilisant un vecteur de parametres. Elle recupere les informations necessaires, calcule l'agregat en fonction des parametres specifies et retourne le resultat.
#'
#' @param vecteur_demande Un vecteur contenant les parametres de la demande. Les elements du vecteur doivent être dans l'ordre suivant :
#' \enumerate{
#'   \item \code{id_demande} : Identifiant de la demande.
#'   \item \code{table} : Nom de la table de donnees a utiliser.
#'   \item \code{var_croisement} : Variables de croisement, separees par des tirets.
#'   \item \code{var_croisement_relative} : Variable de croisement relative (unique).
#'   \item \code{var_quanti} : Variable quantitative sur laquelle effectuer les agregations.
#'   \item \code{fonctions_agregations} : Fonctions d'agregation a appliquer, separees par des tirets.
#'   \item \code{condition} : Condition a appliquer pour filtrer les donnees (expression R sous forme de texte).
#'   \item \code{type_output} : Type de sortie souhaite (par exemple, \code{"table"}, \code{"graph"}).
#'   \item \code{nom_fichier_xls} : Nom du fichier Excel de sortie.
#'   \item \code{nom_onglet} : Nom de l'onglet dans le fichier Excel.
#'   \item \code{titre} : Titre pour la sortie (par exemple, titre du graphique).
#'   \item \code{unite} : Unites de mesure, separees par des tirets.
#' }
#' @param for_ollama Logical. Si TRUE, retourne uniquement la table agregee sans traitement supplementaire.
#'
#' @return Selon les parametres :
#' \itemize{
#'   \item Un \code{data.frame} contenant le resultat de l'agregation si \code{type_output = "table"} ou \code{for_ollama = TRUE}
#'   \item Un objet \code{ggplot} si \code{type_output = "graphique"}
#' }
#'
#' @details
#' La fonction effectue les etapes suivantes :
#' \enumerate{
#'   \item Decompose le \code{vecteur_demande} en variables individuelles.
#'   \item Charge la table de donnees correspondante en utilisant \code{get()}.
#'   \item Extrait les variables de croisement et les fonctions d'agregation.
#'   \item evalue la condition fournie pour filtrer les donnees si necessaire.
#'   \item Calcule l'agregat en appelant la fonction \code{calculer_agregat_sur_croisement()} avec les parametres extraits.
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple d'utilisation de gerer_une_demande
#' # Supposons que vecteur_demande soit un vecteur correctement formate
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
#'   nom_onglet = "Synthese",
#'   titre = "Rapport des ventes 2023",
#'   unite = "euros"
#' )
#' resultat <- gerer_une_demande(vecteur_demande)
#' }
#'
#' @seealso \code{\link{calculer_agregat_sur_croisement}}
#'
#' @importFrom zeallot %<-%
#' @export
gerer_une_demande <- function(vecteur_demande,metadata = NULL,for_ollama=FALSE) {
  
  # EP_FI_AG <-creer_table_minimale(20)
  # setDT(EP_FI_AG)
  # table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_valides
  # i <-1
  # vecteur_demande <- table_demandes[i,]%>% unlist()
  c(
    id_demande,
    nom_table,
    var_croisement,
    var_croisement_relative,
    var_quanti,
    fonctions_agregations,
    condition,
    type_output,
    nom_fichier_xls,
    nom_onglet,
    titre,
    unite,
    var_evolution
  ) %<-% vecteur_demande
  # Charger la table de donnees correspondante
  table <- get(nom_table)
  
  
  # Extraire les variables de croisement
  var_croisement <- strsplit(var_croisement, "-")[[1]]
  var_quanti <- strsplit(var_quanti, "-")[[1]]
  unite <- strsplit(unite, "-")[[1]]
  # Extraire les fonctions d'agregation
  vecteur_nom_fonction <- strsplit(fonctions_agregations, "-")[[1]]  
  liste_fonction_agregation <- lapply(vecteur_nom_fonction, get)
  names(liste_fonction_agregation) <- vecteur_nom_fonction
  
  # evaluer la condition si elle existe
  condition_texte <- condition
  condition <- with(table, eval(parse(text = condition_texte)))
  
  if (!is.null(condition) && sum(is.na(condition))!=0) condition <- NULL
  if(nchar(nom_fichier_xls)==0) nom_fichier_xls <- NULL

  if(nchar(var_croisement_relative)==0) var_croisement_relative <- c()
  
  # Calculer l'agregat
  table_agrege <- calculer_agregat_sur_croisement(
    table = table,
    var_croisement = var_croisement,
    var_croisement_relative = var_croisement_relative,
    var_quanti = var_quanti ,
    liste_fonction_agregation = liste_fonction_agregation,
    condition = condition,
    unites = unite
  )

  if(var_annee != "") print("CODER CETTE PARTIE POUR EVOLUTION")
  # TODO
  

  # Transformer les noms de colonnes si metadata existe dans l'environnement
  if (!is.null(metadata)) {
    nouveaux_noms <- transformer_noms_colonnes(
      noms_colonnes = colnames(table_agrege),
      metadata = metadata,
      nom_table = nom_table
    )
    colnames(table_agrege) <- nouveaux_noms
    
    # Transformer les noms des variables de croisement
    var_croisement <- transformer_noms_colonnes(
      noms_colonnes = var_croisement,
      metadata = metadata,
      nom_table = nom_table
    )
    
    if (length(var_croisement_relative) > 0) {
      var_croisement_relative <- transformer_noms_colonnes(
        noms_colonnes = var_croisement_relative,
        metadata = metadata,
        nom_table = nom_table
      )
    }
  }

  # for_ollama = FALSE
  # Retourner le resultat si pas de graphique demande
  if(type_output == "table" & !for_ollama ){
    if (!is.null(nom_fichier_xls)) {
      if(type_output == "graphique") stop("pas de graphique dans les fichiers xls")
        ecrire_xls(
          nom_fichier_xls=nom_fichier_xls,
          nom_onglet = nom_onglet,
          table = table_agrege,
          titre = titre,
          var_group_by = c(var_croisement,var_croisement_relative)
        )  
    }
    return(table_agrege)
  }

  if(for_ollama) return(table_agrege)

  var_part <- grep("-part|Part",colnames(table_agrege),value = TRUE)
  
  p <- creer_graphique_bar(
      data = table_agrege,
      var_x = var_croisement ,
      var_y = var_part,
      var_fill = var_croisement_relative,
      lab_x = var_croisement,
      lab_y = "Pourcentage",
      titre = titre,
      labels_fill = c(),
      param_position = "dodge"
      )
  
  return(p) 
}


#' Transformer les noms de colonnes avec les métadonnées
#'
#' @description
#' Transforme les noms de colonnes bruts en libellés plus compréhensibles en utilisant
#' les métadonnées et en gérant les suffixes spéciaux (_sum, _part, etc.).
#'
#' @param noms_colonnes Un vecteur de noms de colonnes à transformer
#' @param metadata La liste des métadonnées contenant les descriptions des variables
#' @param nom_table Le nom de la table dans les métadonnées
#' @return Un vecteur de noms de colonnes transformés
#' @export
transformer_noms_colonnes <- function(noms_colonnes, metadata, nom_table) {
  # noms_colonnes <- colnames(table_agrege)
  # Trouver la table dans les métadonnées
  table_meta <- purrr::detect(metadata$tables, ~ .$nom == nom_table)
  if (is.null(table_meta)) return(noms_colonnes)
  
  # Créer un dictionnaire des suffixes
  suffixes <- list(
    "sum" = "Somme",
    "part" = "Part",
    "tot" = "Total",
    "mean" = "Moyenne",
    "max" = "Max",
    "min" = "Min"
  )
  
  # Transformer chaque nom de colonne
  nouveaux_noms <- sapply(noms_colonnes, function(col) {
    # Extraire le nom de base et le suffixe
    parties <- strsplit(col, "-")[[1]]
    nom_base <- parties[1]
    if (length(parties) > 1) {
      suffixe <- parties[length(parties)]
    } else {
      suffixe <- NULL
    }
    
    # Chercher la variable dans les métadonnées
    var_meta <- purrr::detect(
      table_meta$variables,
      ~ .$nom == nom_base
    )
    
    if (is.null(var_meta)) {
      nouveau_nom <- col
    } else {
      # Utiliser le libellé court des métadonnées
      nouveau_nom <- var_meta$libelle_court
      
      # Ajouter le suffixe traduit si présent
      if (!is.null(suffixe) && suffixe %in% names(suffixes)) {
        nouveau_nom <- paste(suffixes[[suffixe]],nouveau_nom)
      }
    }
    
    return(nouveau_nom)
  })
  
  return(nouveaux_noms)
}
