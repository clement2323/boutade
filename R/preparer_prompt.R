#' Convert a data.frame to CSV text with line breaks
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
#' @param metadata A named list of metadata elements
#' @return A formatted string containing the metadata
#' @export
#' @examples
#' metadata <- list(name = "value", other = "other value")
#' format_metadata(metadata)
format_metadata <- function(metadata) {
    formatted <- "MÉTADONNÉES : \n"
    for(name in names(metadata)) {
    formatted <- paste0(formatted, "- ", name, " : ", metadata[[name]], " \n")
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
preparer_prompt <- function(table, metadata,
    prompt_header = "Je souhaite une analyse concise d'un tableau de données. \n\n",
    prompt_instruction = "INSTRUCTION : \nVeuillez fournir une analyse très brève des faits saillants de ce tableau. Juste un paragraphe, pas
    de référence au tableau en soit mais juste aux faits délivrés par ce dernier."
) {
    #  metadata <- list( 
    #  dep_EP = "département de l'entreprise profilée",
    #  poids_sum = "somme des poids des entreprises (=1 ici)",
    #  redi_r310_FI = "chiffre d'affaires des filiales",
    #  depf_FI_relatif = "département relatif (\"interieur\" si même département que l'entreprise)"
    #)

    # Composants du prompt
    prompt_metadata <- format_metadata(metadata)
    prompt_data <- paste0("DONNÉES : \n", df_to_text(table), "\n\n")
        # Assemblage du prompt final
        prompt_final <- paste0(
        prompt_header,
        prompt_metadata,
        prompt_data,
        prompt_instruction
    )

    prompt_final
}