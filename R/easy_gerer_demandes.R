#' Générer une table agrégée à partir d'une demande
#'
#' @param vecteur_demande Un vecteur nommé contenant les paramètres de la demande
#' @param metadata Liste optionnelle des métadonnées pour la transformation des noms
#' @return Une liste contenant la table agrégée et ses paramètres associés
#' @export
renvoyer_table_from_demande <- function(vecteur_demande, metadata = NULL) {
  # i <- 1
  # metadata  <- metadonnees_tables
  # vecteur_demande <- table_demandes[i,]
  params <- preparer_parametres_demande(vecteur_demande)
  # Calcul de la table agrégée
  table_agrege <- calculer_agregat_sur_croisement(
    table = get(params$table),
    var_croisement = params$var_croisement,
    var_croisement_relative = params$var_croisement_relative,
    var_quanti = params$var_quanti,
    liste_fonction_agregation = params$liste_fonction_agregation,
    condition = params$condition,
    unites = params$unite
  )
  
  # Calcul de l'évolution si nécessaire
  if (nchar(params$autres$var_evolution) != 0) {
    col_select <- colnames(table_agrege)[
      !colnames(table_agrege) %in% grep("part|tot", colnames(table_agrege), value = TRUE)
    ]
    var_quanti_selec <- grep(params$var_quanti, col_select, value = TRUE)
    table_agrege <- calculer_evolution_from_table_agrege(
      table_agrege,
      params$autres$var_evolution,
      params$var_croisement,
      params$var_croisement_relative,
      var_quanti_selec
    )
  }

  # Retourner la table agrégée avec les paramètres associés
  return(list(
    table = table_agrege,
    params = params
  ))
}



#' Transformer les noms de colonnes avec les métadonnées
#'
#' @param noms_colonnes Vecteur de noms de colonnes à transformer
#' @param metadata Liste des métadonnées contenant les descriptions des variables
#' @param nom_table Nom de la table dans les métadonnées
#' @return Vecteur de noms de colonnes transformés
#' @export
transformer_noms_colonnes <- function(noms_colonnes, nom_table) {
  # Créer un dictionnaire des suffixes
  suffixes <- c(
    "sum" = "Somme",
    "part" = "Part",
    "tot" = "Total",
    "mean" = "Moyenne",
    "max" = "Max",
    "min" = "Min"
  )
  # nom_table <- "EP_FI_AG"; nom_colonnes <- colnames(table_agrege)
  # Filtrer les métadonnées pour la table concernée
  meta_vars <- metadata[metadata$nom_table == nom_table, c("nom_variable", "libelle_court")]
  
  # Transformer chaque nom
  sapply(noms_colonnes, function(col) {
    # col <- nom_colonnes[4]
    parties <- strsplit(col, "-")[[1]]
    nom_base <- parties[1]
    
    # Récupérer le libellé court
    nouveau_nom <- meta_vars$libelle_court[meta_vars$nom_variable == nom_base]
    if (length(nouveau_nom) == 0) return(col)
    
    # Ajouter le suffixe si présent
    if (length(parties) > 1 && parties[2] %in% names(suffixes)) {
      nouveau_nom <- paste(suffixes[parties[2]], nouveau_nom)
    }
    
    nouveau_nom
  })
}

#' Écrire une table agrégée dans un fichier Excel
#'
#' @param element_table_param Liste contenant la table et ses paramètres
#' @return NULL (effet de bord : création fichier Excel)
#' @export
ecrire_demande_sur_xls <- function(vecteur_demande, metadata = NULL) {

# vecteur_demande <- table_demandes[2,]
  liste_resultat <- renvoyer_table_from_demande(vecteur_demande)
  table_agrege <- liste_resultat$table
  params <- liste_resultat$params
  
  
  # Transformation des noms de colonnes avec les métadonnées
  if (!is.null(metadata)) {
    
    # Transformation unique
    nouveaux_noms <- transformer_noms_colonnes(
      noms_colonnes = colnames(table_agrege),
      metadata = metadata,
      nom_table = params$table
    )
    
    # Application des transformations
    colnames(table_agrege) <- nouveaux_noms
  }
  
  ecrire_xls(
    nom_fichier_xls = params$excel$nom_fichier_xls,
    nom_onglet = params$excel$nom_onglet,
    table = table_agrege,
    titre = params$autres$titre,
    var_group_by = c(params$var_croisement, params$var_croisement_relative),
    var_evolution = if(params$autres$var_evolution=="") NULL else params$autres$var_evolution
  )
}

#' Générer un graphique en barres à partir d'une table agrégée
#'
#' @param element_table_param Liste contenant la table et ses paramètres
#' @return Un objet ggplot2
#' @importFrom ggplot2 ggplot geom_bar
#' @export
generer_graphique_from_table <- function(element_table_param) {
  
  liste_resultat <- renvoyer_table_from_demande(vecteur_demande)
  table_agrege <- liste_resultat$table
  params <- liste_resultat$params
  
  
  # Transformation des noms de colonnes avec les métadonnées
  if (!is.null(metadata)) {
    
    # Transformation unique
    nouveaux_noms <- transformer_noms_colonnes(
      noms_colonnes = colnames(table_agrege),
      metadata = metadata,
      nom_table = params$table
    )
    
    # Application des transformations
    colnames(table_agrege) <- nouveaux_noms
  }
  

  creer_graphique_bar(
    data = table_agrege,
    var_x = params$var_croisement,
    var_y = grep("-part|Part", colnames(table_agrege), value = TRUE),
    var_fill = params$var_croisement_relative,
    lab_x = params$var_croisement,
    lab_y = "Pourcentage",
    titre = params$rmd$titre_figure %||% params$autres$titre,
    labels_fill = c(),
    param_position = "dodge"
  )
}