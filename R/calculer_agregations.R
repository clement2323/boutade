# Fonction pour obtenir la fonction d'arrondi en fonction de l'unité
#' Obtenir une fonction d'arrondi selon l'unité spécifiée
#'
#' @param unite Character. L'unité d'arrondi souhaitée. Valeurs possibles : 
#'   "unite", "dixieme", "centieme", "millieme", "million"
#' @return Une fonction qui prend un nombre et retourne sa valeur arrondie
#'   selon l'unité spécifiée
#' @examples
#' f_arrondi <- get_rounding_function("dixieme")
#' f_arrondi(3.14159)  # Retourne 3.1
#'
#' f_millions <- get_rounding_function("million")
#' f_millions(1234567)  # Retourne 1
#' @export
get_rounding_function <- function(unite) {
  if (unite == "unite") {
    function(x) round(x, 0)
  } else if (unite == "dixieme") {
    function(x) round(x, 1)
  } else if (unite == "centieme") {
    function(x) round(x, 2)
  } else if (unite == "millieme") {
    function(x) round(x, 3)
  } else if (unite == "million") {
    function(x) round(x / 1e6, 0)
  } else {
    stop("Unité non reconnue: ", unite)
  }
}

#' Gérer l'agrégation avec la fonction sum
#'
#' @param table Data.table contenant les données.
#' @param var_croisement Variables de croisement.
#' @param var_croisement_relative Variables de croisement relatives (optionnel).
#' @param var_quanti Variable quantitative à agréger.
#' @return Un data.table avec les sous-totaux, totaux et parts.
#' @import data.table
#' @export
gerer_agregation_sum <- function(table, var_croisement, var_croisement_relative = c(), var_quanti) {
  # Vérification que 'table' est un data.table
  if (!is.data.table(table)) stop("La table en entrée n'est pas un objet de type data.table, veuillez la transformer au préalable")
  
  var_group_by <- c(var_croisement, var_croisement_relative)
  
  # Calculer les sous-totaux par groupe en utilisant la fonction sum
  result <- table[,
                  .(sous_tot = sum(get(var_quanti), na.rm = TRUE)), 
                  by = var_group_by
  ]
  
  # Calculer le total et la part
  result[, tot := sum(sous_tot), by = var_croisement]
  result[, part := round(100 * sous_tot / tot, 1)]
  
  return(result)
}

#' Calculer l'agrégation sur croisement avec plusieurs fonctions
#'
#' @param table Data.table contenant les données.
#' @param var_croisement Variables de croisement.
#' @param var_croisement_relative Variables de croisement relatives (optionnel).
#' @param var_quanti Vecteur de variables quantitatives à agréger.
#' @param liste_fonction_agregation Liste de fonctions d'agrégation (ex: list(sum, mean)).
#' @param unites Vecteur des unités pour arrondir les résultats (ex: c("unite", "dixieme")).
#' @param condition Condition à appliquer (optionnelle).
#' @return Un tibble avec les agrégations calculées et arrondies.
#' @import data.table
#' @import dplyr
#' @export
calculer_agregat_sur_croisement <- function(table, var_croisement, var_croisement_relative = c(), var_quanti, liste_fonction_agregation, unites, condition = NULL) {
  # unites = unite
  # Vérification que 'table' est un data.table
  if (!is.data.table(table)) stop("La table en entrée n'est pas un objet de type data.table, veuillez la transformer au préalable")
  
  # Appliquer la condition si elle est spécifiée
  if (!is.null(condition)) table <- table[condition]
  
  var_group_by <- c(var_croisement, var_croisement_relative)
  
  # Vérifier que les longueurs de var_quanti, liste_fonction_agregation et unites sont égales
  if (length(var_quanti) != length(liste_fonction_agregation) || length(var_quanti) != length(unites)) {
    stop("Les longueurs de 'var_quanti', 'liste_fonction_agregation' et 'unites' doivent être égales")
  }
  
  # Initialiser une liste pour stocker les résultats
  res_group_by <- list()
  
  # Parcourir chaque variable quantitative, sa fonction d'agrégation correspondante, et son unité
  for (i in seq_along(var_quanti)) {
    variable <- var_quanti[i]
    fonction <- liste_fonction_agregation[[i]]
    fonction_name <- names(liste_fonction_agregation)[i]
    unite <- unites[i]
    
    # Si la fonction est 'sum', utiliser gerer_agregation_sum
    if (identical(fonction, sum)) {
      result <- gerer_agregation_sum(table, var_croisement, var_croisement_relative, variable)
      # Renommer les colonnes pour inclure le nom de la variable
      setnames(result, old = c("sous_tot", "tot", "part"), 
               new = c(paste0(variable, "_sum"), paste0(variable, "_tot"), paste0(variable, "_part")))
    } else {
      # Calculer l'agrégation pour la variable avec la fonction spécifiée
      result <- table[,
                      .(value = fonction(get(variable), na.rm = TRUE)), 
                      by = var_group_by
      ]
      # Renommer la colonne 'value' avec le nom de la variable et de la fonction
      setnames(result, "value", paste0(variable, "_", fonction_name))
    }
    res_group_by[[i]] <- result
  }
  
  # Combiner les résultats dans un seul data.table
  out <- Reduce(function(x, y) merge(x, y, by = var_group_by, all = TRUE), res_group_by)
  
  # Appliquer l'arrondi
  for (i in seq_along(var_quanti)) {
    variable <- var_quanti[i]
    unite <- unites[i]
    rounding_func <- get_rounding_function(unite)
    
    # Trouver les colonnes concernées
    if (identical(liste_fonction_agregation[[i]], sum)) {
      # Colonnes: variable_sum, variable_tot
      sum_col <- paste0(variable, "_sum")
      tot_col <- paste0(variable, "_tot")
      if (sum_col %in% names(out)) {
        out[[sum_col]] <- rounding_func(out[[sum_col]])
      }
      if (tot_col %in% names(out)) {
        out[[tot_col]] <- rounding_func(out[[tot_col]])
      }
      # 'part' est déjà arrondi à une décimale dans gerer_agregation_sum
    } else {
      # Colonne: variable_fonctionname
      func_col <- paste0(variable, "_", names(liste_fonction_agregation)[i])
      if (func_col %in% names(out)) {
        out[[func_col]] <- rounding_func(out[[func_col]])
      }
    }
  }
  
  # Convertir en tibble pour une meilleure présentation
  out <- as_tibble(out) %>%
    arrange(across(all_of(var_group_by)))
  
  return(out)
}
