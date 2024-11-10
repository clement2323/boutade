#' Préparer les paramètres d'une demande de traitement
#'
#' @param vecteur_demande Un vecteur nommé contenant les paramètres de la demande
#' @return Une liste structurée des paramètres traités
#' @export
preparer_parametres_demande <- function(vecteur_demande) {
  # Définition des paramètres attendus par type
  parametres_communs <- c(
    "var_croisement",
    "var_quanti",
    "fonctions_agregations",
    "titre",
    "var_croisement_relative",
    "condition",
    "unite"
  )
  
  parametres_excel <- c(
    "nom_fichier_xls",
    "nom_onglet"
  )
  
  parametres_rmd <- c(
    "partie",
    "sous_partie",
    "titre_figure"
  )
  
  # Conversion en named vector si ce n'est pas déjà le cas
  if (is.null(names(vecteur_demande))) {
    stop("Le vecteur de demande doit avoir des noms de colonnes")
  }
  
  # Fonction helper pour extraire et formater un paramètre
  extraire_param <- function(nom, defaut = NULL) {
    valeur <- vecteur_demande[[nom]]
    if (is.null(valeur) || is.na(valeur) || valeur == "") return(defaut)
    valeur
  }
  
  # Traitement des paramètres spéciaux
  params_split <- list(
    var_croisement = strsplit(extraire_param("var_croisement", ""), "-")[[1]],
    var_quanti = strsplit(extraire_param("var_quanti", ""), "-")[[1]],
    unite = strsplit(extraire_param("unite", ""), "-")[[1]],
    var_croisement_relative = {
      val <- extraire_param("var_croisement_relative", "")
      if (nchar(val) == 0) c() else val
    }
  )
  
  # Traitement des fonctions d'agrégation
  fns <- strsplit(extraire_param("fonctions_agregations", ""), "-")[[1]]
  params_split$liste_fonction_agregation <- if (length(fns) > 0) {
    stats::setNames(lapply(fns, get), fns)
  } else {
    list()
  }
  
  # Construction de la condition
  params_split$condition <- {
    cond_text <- extraire_param("condition", "")
    if (nchar(cond_text) == 0) {
      NULL
    } else {
      tryCatch({
        cond <- with(get(extraire_param("table")), eval(parse(text = cond_text)))
        if (!is.null(cond) && sum(is.na(cond)) != 0) NULL else cond
      }, error = function(e) NULL)
    }
  }
  
  # Paramètres pour Excel
  params_excel <- list(
    nom_fichier_xls = extraire_param("nom_fichier_xls"),
    nom_onglet = extraire_param("nom_onglet")
  )
  
  # Paramètres pour RMD
  params_rmd <- list(
    partie = extraire_param("partie"),
    sous_partie = extraire_param("sous_partie"),
    titre_figure = extraire_param("titre_figure")
  )
  
  # Autres paramètres
  params_autres <- list(
    titre = extraire_param("titre"),
    type_output = extraire_param("type_output", "table"),
    var_evolution = extraire_param("var_evolution", "")
  )
  
  # Assemblage final
  c(
    params_split,
    list(
      table = get(extraire_param("table")),
      excel = params_excel,
      rmd = params_rmd,
      autres = params_autres
    )
  )
}

#' Générer une table agrégée à partir d'une demande
#'
#' @param vecteur_demande Un vecteur nommé contenant les paramètres de la demande
#' @param metadata Liste optionnelle des métadonnées pour la transformation des noms
#' @return Une liste contenant la table agrégée et ses paramètres associés
#' @export
renvoyer_table_from_demande <- function(vecteur_demande, metadata = NULL) {
  # i <- 1
  # vecteur_demande <- table_demandes[i,]
  params <- preparer_parametres_demande(vecteur_demande)
  
  # Calcul de la table agrégée
  table_agrege <- calculer_agregat_sur_croisement(
    table = params$table,
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
  
  # Transformation des noms de colonnes avec les métadonnées
  if (!is.null(metadata)) {
    nouveaux_noms <- transformer_noms_colonnes(
      noms_colonnes = colnames(table_agrege),
      metadata = metadata,
      nom_table = params$nom_table
    )
    colnames(table_agrege) <- nouveaux_noms
    
    # Transformation des variables de croisement
    params$var_croisement <- transformer_noms_colonnes(
      noms_colonnes = params$var_croisement,
      metadata = metadata,
      nom_table = params$nom_table
    )
    
    if (length(params$var_croisement_relative) > 0) {
      params$var_croisement_relative <- transformer_noms_colonnes(
        noms_colonnes = params$var_croisement_relative,
        metadata = metadata,
        nom_table = params$nom_table
      )
    }
  }
  
  # Retourner la table agrégée avec les paramètres associés
  return(list(
    table = table_agrege,
    params = params
  ))
}

#' Écrire une table agrégée dans un fichier Excel
#'
#' @param element_table_param Liste contenant la table et ses paramètres
#' @return NULL (effet de bord : création fichier Excel)
#' @export
ecrire_demande_sur_xls <- function(element_table_param) {
  table_agrege <- element_table_param$table
  params <- element_table_param$params
  
  ecrire_xls(
    nom_fichier_xls = params$excel$nom_fichier_xls,
    nom_onglet = params$excel$nom_onglet,
    table = table_agrege,
    titre = params$autres$titre,
    var_group_by = c(params$var_croisement, params$var_croisement_relative),
    var_evolution = params$autres$var_evolution
  )
}

#' Générer un graphique en barres à partir d'une table agrégée
#'
#' @param element_table_param Liste contenant la table et ses paramètres
#' @return Un objet ggplot2
#' @importFrom ggplot2 ggplot geom_bar
#' @export
generer_graphique_from_table <- function(element_table_param) {
  table_agrege <- element_table_param$table
  params <- element_table_param$params
  
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

#' Transformer les noms de colonnes avec les métadonnées
#'
#' @param noms_colonnes Vecteur de noms de colonnes à transformer
#' @param metadata Liste des métadonnées contenant les descriptions des variables
#' @param nom_table Nom de la table dans les métadonnées
#' @return Vecteur de noms de colonnes transformés
#' @importFrom purrr detect
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
