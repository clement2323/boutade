#' Write a table to an Excel file with formatting, appending to existing sheets
#'
#' @param nom_fichier_xls Path to the Excel file.
#' @param nom_onglet Name of the worksheet.
#' @param table Data frame to write.
#' @param titre Title to write above the table.
#'
#' @description
#' This function writes a data frame to an Excel file (.xlsx) with specific formatting. It places the given title in bold above the table, writes the column names in bold, and formats the first column (excluding the header) in italic. If the worksheet already exists and contains data, the new table will be written to the right of the existing data, leaving one column of space.
#'
#' @details
#' - Opens an `openxlsx` workbook object pointing to the file at `chemin_xlsx`.
#' - If `nom_onglet` exists in the workbook:
#'   - Reads existing data to determine the starting column.
#'   - Writes the `titre` in bold one column after the existing data.
#'   - Writes the `table` below the title with column names in bold.
#' - If `nom_onglet` does not exist:
#'   - Adds a new worksheet named `nom_onglet`.
#'   - Writes the `titre` in bold at the first column.
#'   - Writes the `table` below the title with column names in bold.
#' - Formats the first column of the new table (excluding header) in italic.
#' - Saves the workbook, overwriting the existing Excel file.
#'
#' @examples
#' \dontrun{
#' ecrire_xls("data.xlsx", "Sheet3", data.frame(b=seq(1,5)), "My Table Title")
#' }
#'
#' @import openxlsx
#' @export
ecrire_xls <- function(nom_fichier_xls, nom_onglet, table, titre, dir="output/") {
  
  # Vérifie si le package openxlsx est installé
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Le package 'openxlsx' est requis mais n'est pas installé.")
  }
  
  # Crée le répertoire s'il n'existe pas
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Chemin complet du fichier
  chemin_complet <- paste0(dir, nom_fichier_xls)
  
  # Charge ou crée le classeur
  if (file.exists(chemin_complet)) {
    wb <- openxlsx::loadWorkbook(chemin_complet)
  } else {
    wb <- openxlsx::createWorkbook()
  }
  
  # Détermine la colonne et la ligne de départ
  if (nom_onglet %in% openxlsx::sheets(wb)) {
    # Lit les données existantes de la feuille
    existing_data <- openxlsx::readWorkbook(wb, sheet = nom_onglet, colNames = FALSE, rowNames = FALSE)
    
    if (!is.null(existing_data) && ncol(existing_data) > 0) {
      last_col <- ncol(existing_data)
      startCol <- last_col + 2 + 1 # Laisse une colonne d'espace
    } else {
      startCol <- 1
    }
  } else {
    # Ajoute la feuille si elle n'existe pas
    openxlsx::addWorksheet(wb, nom_onglet)
    startCol <- 1
  }
  
  # Ligne de départ pour l'écriture
  startRow <- 1
  
  # Écrit le titre en gras
  titleStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, sheet = nom_onglet, x = titre, startCol = startCol, startRow = startRow)
  openxlsx::addStyle(wb, sheet = nom_onglet, style = titleStyle, rows = startRow, cols = startCol, gridExpand = TRUE)
  
  # Style pour les en-têtes avec bordures noires
  headerStyle <- openxlsx::createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "black")
  
  # Écrit la table avec les en-têtes en gras et bordures
  openxlsx::writeData(wb, sheet = nom_onglet, x = table, startCol = startCol, startRow = startRow + 1, headerStyle = headerStyle)
  
  # Applique le style italique à la première colonne de la nouvelle table (hors en-tête)
  italicStyle <- openxlsx::createStyle(textDecoration = "italic")
  numRows <- nrow(table)
  if (numRows > 0) {
    openxlsx::addStyle(
      wb,
      sheet = nom_onglet,
      style = italicStyle,
      rows = (startRow + 2):(startRow + numRows + 1),
      cols = startCol,
      gridExpand = TRUE
    )
    
    # Style avec bordures noires pour les données de la table
    borderStyle <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "black")
    
    # Applique le style de bordure aux cellules de la table
    openxlsx::addStyle(
      wb,
      sheet = nom_onglet,
      style = borderStyle,
      rows = (startRow + 2):(startRow + numRows + 1),
      cols = startCol:(startCol + ncol(table) - 1),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  # Sauvegarde le classeur avec le chemin complet
  openxlsx::saveWorkbook(wb, chemin_complet, overwrite = TRUE)
}
