#' Générer une table agrégée à partir d'une demande
#' @param vecteur_demande Vecteur nommé des paramètres de la demande
#' @param metadata Liste des métadonnées pour la transformation des noms
#' @return Liste contenant la table agrégée (table) et ses paramètres (params)
#' @export
renvoyer_table_from_demande <- function(vecteur_demande) {
  # i <- 1 table_demandes <-table_demandes_rmd
  # metadata  <- metadonnees_tables
  # vecteur_demande <- table_demandes[i,]
  # setDT(EP_FI_AG)
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


#' Écrire une table agrégée dans un fichier Excel
#' @param vecteur_demande Vecteur nommé des paramètres de la demande 
#' @param metadata Liste des métadonnées pour la transformation des noms
#' @return NULL (effet de bord : création d'un fichier Excel)
#' @export
ecrire_demande_sur_xls <- function(vecteur_demande, metadata = NULL) {

# vecteur_demande <- table_demandes[1,]
# EP_FI_AG$poids <- 1
# vecteur_demande$var_quanti <- "poids"
# metadata <- metadonnees_tables
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

#' Générer une figure à partir d'une table agrégée
#' @param vecteur_demande Vecteur nommé des paramètres de la demande
#' @param metadonnees_tables Liste des métadonnées des tables
#' @param ollama Booléen indiquant si utiliser Ollama
#' @param fonction_ask Fonction de requête optionnelle
#' @param metadonnees_etude Liste des métadonnées de l'étude
#' @return Liste contenant la table agrégée, la réponse Ollama et les paramètres
#' @importFrom ggplot2 ggplot geom_bar
#' @export

generer_figure_from_demande <- function(vecteur_demande, metadonnees_tables,
                                      ollama = FALSE, fonction_ask = NULL, 
                                      metadonnees_role = NULL, metadonnees_etude = NULL) {

  #vecteur_demande <- table_demandes_rmd[20,] setDT(EP_FI_AG)
  liste_resultat <- renvoyer_table_from_demande(vecteur_demande)
  table_agrege <- liste_resultat$table
  params <- liste_resultat$params

  nouveaux_noms_colonne <- colnames(table_agrege)
  #récupération des noms de colonne courts
  if (!is.null(metadonnees_tables)) {
    # Transformation unique
    nouveaux_noms_colonnes <- transformer_noms_colonnes(
      noms_colonnes = colnames(table_agrege),
      metadata = metadonnees_tables,
      nom_table = params$table
    )
  }

  figure <- retourner_figure(table_agrege,nouveaux_noms_colonnes,params)

  rep_ollama <- NULL
  if(ollama) {
    prompt <- preparer_prompt(
      table_agrege,
      metadonnees_tables,
      metadonnees_etude,
      metadonnees_role,
      params
      )
    #cat(prompt) # fonction_ask <- function(prompt) { ask_ollama(prompt,"mistral-small")}
    rep_ollama <- fonction_ask(prompt)
    #cat(rep_ollama)  ; ask_ollama(prompt)
  }
  
  out <- list(
    figure = figure,
    rep_ollama = rep_ollama,
    params = params
  )

  out
}

#' Transformer les noms de colonnes avec les métadonnées
#' @param noms_colonnes Vecteur des noms de colonnes à transformer
#' @param nom_table Nom de la table dans les métadonnées
#' @param metadata Liste des métadonnées avec descriptions des variables
#' @return Vecteur des noms de colonnes transformés
#' @export
transformer_noms_colonnes <- function(noms_colonnes, nom_table, metadata) {
  # Créer un dictionnaire des suffixes
  suffixes <- c(
    "sum" = "Somme",
    "part" = "Part",
    "tot" = "Total",
    "mean" = "Moyenne",
    "max" = "Max",
    "min" = "Min"
  )

  remplacements <- list(
    "poids-sum" = "Effectif",
    "poids-tot" = "Effectif Total",
    "poids-part" = "Part",
    "evolution" = "Evolution"
  )

  # nom_table <- "EP_FI_AG"; nom_colonnes <- colnames(table_agrege)
  # Filtrer les métadonnées pour la table concernée
  meta_vars <- metadata[metadata$nom_table == nom_table, c("nom_variable", "libelle_court")]
  
  # Fonction pour remplacer les chaînes dans le texte
  remplacer_chaines <- function(texte, remplacements) {
    for (ancien in names(remplacements)) {
      texte <- gsub(ancien, remplacements[[ancien]], texte)
    }
    return(texte)
  }

  # Transformer chaque nom
  nouveaux_noms <- sapply(noms_colonnes, function(col) {
  # col <- noms_colonnes[3]
  # col <- "poids-sum-2022"
  # col <- "redi_r310_EP-mean-2019"
    
    if(any(sapply(names(remplacements),function(name) grepl(name, col)))){
      return(remplacer_chaines(col, remplacements))  # Retourner directement le nouveau nom
    }
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

    if(length(parties) == 3) nouveau_nom <- paste(nouveau_nom, parties[3])
    
    return(nouveau_nom)
  })
  
  nouveaux_noms
}



