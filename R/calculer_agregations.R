#' Gérer l'agrégation avec la fonction sum
#'
#' @export
#' @param table Data.table contenant les données.
#' @param var_croisement Variables de croisement.
#' @param var_croisement_relative Variables de croisement relatives (optionnel).
#' @param var_quanti Variable quantitative à agréger.
#' @return Un tibble avec les sous-totaux, totaux et parts.
#' @import data.table
#' @import dplyr

gerer_agregation_sum <- function(table, var_croisement, var_croisement_relative = c(), var_quanti) {
  # Vérification que 'table' est un data.table
  if (!is.data.table(table)) stop("La table en entrée n'est pas un objet de type data.table, veuillez le transformer au préalable")
  
  var_group_by <- c(var_croisement, var_croisement_relative)
  
  # Calculer les sous-totaux par groupe en utilisant la fonction sum
  result <- table[,
                  .(sous_tot = sum(get(var_quanti), na.rm = TRUE)), 
                  by = var_group_by
  ]
  
  # Calculer le total et la part
  out <- result %>%
    as_tibble() %>%
    group_by(across(all_of(var_croisement))) %>%
    mutate(tot = sum(sous_tot), 
           part = round(100 * sous_tot / tot, 1)) %>%
    ungroup()
  
  return(out)
}


#' Calculer l'agrégation sur croisement avec plusieurs fonctions
#'
#' @export
#' @import data.table
#' @param table Data.table contenant les données.
#' @param var_croisement Variables de croisement.
#' @param var_croisement_relative Variables de croisement relatives (optionnel).
#' @param var_quanti Variable quantitative à agréger.
#' @param liste_fonction_agregation Liste de fonctions d'agrégation (ex: list(sum, mean)).
#' @param condition Condition à appliquer (optionnelle).
#' @return Un tibble avec les totaux et les parts pour chaque fonction d'agrégation.
calculer_agregat_sur_croisement <- function(table, var_croisement, var_croisement_relative = c(), var_quanti, liste_fonction_agregation, condition = NULL) {
  
  # table <- table_finale
  # condition <- table_finale$AN %in% c(2018, 2019)
  # var_croisement_relative = c("SX")
  # var_quanti <- c("REVACT")
  # var_croisement = c("dept_agreg", "AN")
  # liste_fonction_agregation = list( "mean" = mean,"median" =median) # sum se retrouvera toujourstout seul, sinon list(mean,max,etc..)
  
  if (!is.data.table(table)) stop("La table en entrée n'est pas un objet de type data.table, veuillez le transformer au préalable")
  if (!is.null(condition)) table <- table[condition]
  
  if (identical(names(liste_fonction_agregation), "sum")) {
    out <- gerer_agregation_sum(table, var_croisement, var_croisement_relative, var_quanti)
    return(out)
  }
  
  # si on est pas dans le cas sum
  var_group_by <- c(var_croisement, var_croisement_relative)
  
  res_group_by <- lapply(names(liste_fonction_agregation), function(fonction_name) {
    # fonction_name <- "mean"
    fonction <- liste_fonction_agregation[[fonction_name]]
    result <- table[,
                    .(sous_tot = fonction(get(var_quanti), na.rm = TRUE)), 
                    by = var_group_by
    ]
    # Renommer la colonne 'sous_tot' avec le nom de la fonction
    setnames(result, "sous_tot",paste0(var_quanti,"_",fonction_name))
    
    return(result)
  })
  # Combiner les résultats dans un seul data.table
  out <- Reduce(function(x, y) merge(x, y, by = var_group_by, all = TRUE), res_group_by)
  
  return(out)
}




