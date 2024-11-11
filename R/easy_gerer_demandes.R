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
generer_figure_from_demande <- function(vecteur_demande,metadonnees_tables,
ollama = FALSE,fonction_ask = NULL, metadonnees_etude) {


  # figure, soit graphique soit kable
  #
  data("metadonnees_tables"); data("metadonnees_etude")
  # metadata si on veut des noms de colonne etelement graphiques propres, ollama si on veut un commentaire
  #vecteur_demande <- table_demandes_rmd[1,]
  
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
  # rajouter param type_graphique pour le triage et dans gérer parametres aussi
  # si plus d'une var croisement -> pads de graphique comme ça, généraliser, 2 vars => facet ?
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
#' @export
transformer_noms_colonnes <- function(noms_colonnes, nom_table,metadata) {
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



## TO DO nettoyer !!


#' Convert a data.frame to CSV text with line breaks
#' 
#' @description
#' Takes a data frame and converts it to a CSV-formatted string with line breaks,
#' where columns are separated by commas and rows by newlines.
#' 
#' @param df A data.frame to convert to text
#' @return A string containing the CSV representation of the data.frame
#' @export
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' df_to_text(df)
df_to_text <- function(df) {
    # Convertir les noms de colonnes en texte
    header <- paste(colnames(df), collapse = ",")

    # Convertir chaque ligne en texte
    rows <- apply(df, 1, function(row) {
    paste(row, collapse = ",")
    })

    # Combiner l'en-tête et les lignes avec des sauts de ligne
    text <- paste(c(header, rows), collapse = "\n")

    return(text)
}

#' Format metadata as a text string
#' 
#' @description
#' Takes a metadata list and a data table, and formats the metadata information
#' into a human-readable text string. The function processes special column name
#' suffixes
#' 
#' @param metadata A named list containing metadata elements, including a 'tables'
#' element with table descriptions and variables
#' @param table A data frame whose columns need to be documented
#' @param nom_table The name of the table to look up in the metadata
#' @return A formatted string containing the metadata description, including:
#'   - Table description (if available)
#'   - Special suffix explanations
#'   - Variable descriptions with their labels
#' @export
#' @examples
#' metadata <- list(
#'   tables = list(
#'     list(
#'       nom = "example_table",
#'       description = "An example table",
#'       variables = list(
#'         list(
#'           nom = "var1",
#'           libelle_court = "V1",
#'           description = "First variable"
#'         )
#'       )
#'     )
#'   )
#' )
#' table <- data.frame(var1_sum = 1:3)
#' format_metadata(metadata, table, "example_table")
#' @import purrr

format_metadata <- function(metadata, table, nom_table) {
    # Initialiser un vecteur pour stocker toutes les variables pertinentes
    all_relevant_vars <- list()
    
    # Chercher la table spécifique dans metadata$tables
    table_meta <- purrr::detect(metadata$tables, ~ .$nom == nom_table)
    
    if (!is.null(table_meta)) {
        # Pour chaque colonne de la table
        for (col in colnames(table)) {
            # Chercher toutes les variables dont le nom ou libelle_court est contenu dans le nom de colonne
            matching_vars <- purrr::keep(
                table_meta$variables,
                ~ grepl(.$nom, col, fixed = TRUE) || 
                  grepl(.$libelle_court, col, fixed = TRUE)
            )
            
            # Sélectionner la correspondance la plus longue si plusieurs matches
            if (length(matching_vars) > 0) {
                matching_var <- matching_vars[[
                    which.max(sapply(matching_vars, function(v) 
                        max(nchar(v$nom), nchar(v$libelle_court))))
                ]]
                
                # Garder le nom de colonne original
                matching_var$nom <- col
                all_relevant_vars <- c(all_relevant_vars, list(matching_var))
            }
        }
    }
    
    formatted <- "MÉTADONNÉES : \n"
    
    # Ajouter la description de la table si elle existe
    if (!is.null(table_meta$description)) {
        formatted <- paste0(
            formatted,
            "Description de la table : ", table_meta$description, "\n\n"
        )
    }
    
    # Ajouter l'explication des suffixes spéciaux
    formatted <- paste0(
        formatted,
        "Note sur les suffixes : \n",
        "- _tot : indique le total sur les variables de croisement\n\n",
        "Total : indique le total sur les variables de croisement\n\n"
    )
    
    # Pour chaque variable pertinente, ajouter sa description
    for (var in all_relevant_vars) {
        formatted <- paste0(
            formatted,
            "- ", var$nom, " (", var$libelle_court, ") : ", 
            var$description, "\n"
        )
    }
    return(formatted)
}

#' Prepare a prompt for data analysis
#' 
#' Creates a formatted prompt combining table data and metadata for analysis
#' 
#' @param table A data.frame containing the data to analyze
#' @param metadata A named list of metadata elements describing the data
#' @param prompt_header A string containing the header text for the prompt (default provided)
#' @param prompt_instruction A string containing the instruction text for the prompt (default provided)
#' @return A formatted string containing the complete prompt
#' @export
#' @examples
#' table <- data.frame(x = 1:3, y = letters[1:3])
#' metadata <- list(source = "example", date = "2024-03-20")
#' preparer_prompt(table, metadata)
preparer_prompt <- function(table, nom_table,metadata,
    prompt_header = "Je souhaite une analyse concise d'un tableau de données. \n\n",
    prompt_instruction = "INSTRUCTION : \nVeuillez fournir une analyse très brève des faits saillants de ce tableau. Juste un paragraphe. Attention 
    je me servirai de ce commentaire pour commenter le graphique asocié aussi donc ne parle pas de tableau ni graphique, dis juste qu'on observe que..."
) {
    #  metadata <- list( 
    #  dep_EP = "département de l'entreprise profilée",
    #  poids_sum = "somme des poids des entreprises (=1 ici)",
    #  redi_r310_FI = "chiffre d'affaires des filiales",
    #  depf_FI_relatif = "département relatif (\"interieur\" si même département que l'entreprise)"
    #)

    # Composants du prompt
    prompt_metadata <- format_metadata(metadata, table, nom_table)
    prompt_data <- paste0("DONNÉES : \n", df_to_text(table), "\n\n")
        # Assemblage du prompt final
        prompt_final <- paste0(
        prompt_header,
        prompt_metadata,
        prompt_data,
        prompt_instruction
    )

    prompt_final
    # cat(prompt_final)
}




generer_prompt_role <- function(metadonnees_role) {
  # Début du prompt
  prompt <- "Tu es un assistant spécialisé dans la communication adaptée au public suivant :\n"
  
  # Ajouter les caractéristiques de base du public
  for(nom in names(metadonnees_role)) {
    if(nom != "instructions_communication") {
      prompt <- paste0(prompt, "- ", metadonnees_role[[nom]], "\n")
    }
  }
  
  # Ajouter les règles de communication
  prompt <- paste0(prompt, "\nTu dois suivre ces règles de communication :\n")
  prompt <- paste0(prompt, "- ", metadonnees_role$instructions_communication$style, "\n")
  
  # Ajouter les directives
  if(!is.null(metadonnees_role$instructions_communication$directives)) {
    prompt <- paste0(prompt, "\nDirectives spécifiques :\n")
    for(directive in metadonnees_role$instructions_communication$directives) {
      prompt <- paste0(prompt, "- ", directive, "\n")
    }
  }
  
  # Ajouter le vocabulaire adapté
  if(!is.null(metadonnees_role$instructions_communication$vocabulaire_adapte)) {
    prompt <- paste0(prompt, "\nTu utiliseras un vocabulaire adapté pour les termes techniques suivants :\n")
    for(terme in names(metadonnees_role$instructions_communication$vocabulaire_adapte)) {
      prompt <- paste0(prompt, "- ", terme, " -> ", 
                      metadonnees_role$instructions_communication$vocabulaire_adapte[[terme]], "\n")
    }
  }
  
  # Ajouter la conclusion
  prompt <- paste0(prompt, "\n Dans ta réponse, assure-toi de :
1. Respecter le niveau de langage approprié
2. Suivre toutes les directives de communication
3. Utiliser systématiquement le vocabulaire adapté fourni
4. Maintenir la cohérence avec le type de public cible")
  
  return(prompt)
}


