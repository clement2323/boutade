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
      table = vecteur_demande["table"]%>%unlist(),
      excel = params_excel,
      rmd = params_rmd,
      autres = params_autres
    )
  )
}
