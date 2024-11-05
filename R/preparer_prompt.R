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