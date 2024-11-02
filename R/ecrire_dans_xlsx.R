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
#' ecrire_xls("data.xlsx", "Sheet3", data.frame(b=seq(1,5)), "My Table Title",)
#' }
#'
#' @import openxlsx
#' @export
ecrire_xls <- function(nom_fichier_xls, nom_onglet, table, titre, var_group_by, dir = "output/") {
  
  # Vérifier si le package openxlsx est installé
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Le package 'openxlsx' est requis mais n'est pas installé.")
  }
  
  # Créer le répertoire s'il n'existe pas
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Chemin complet du fichier dir <- "output/"
  chemin_complet <- file.path(dir, nom_fichier_xls)
  
  # Charger ou créer le classeur
  if (file.exists(chemin_complet)) {
    wb <- openxlsx::loadWorkbook(chemin_complet)
  } else {
    wb <- openxlsx::createWorkbook()
  }
  
  
  # Déterminer la position de départ
  startRow <- 1
  
    # Si l'onglet existe déjà, trouver la dernière colonne utilisée
  if (nom_onglet %in% openxlsx::sheets(wb)) {
    # Lire les données existantes
    existing_data <- openxlsx::read.xlsx(wb, sheet = nom_onglet,skipEmptyCols = FALSE)
    # Accéder à la feuille de calcul nom_onglet <- "côté filiales"

    if (!is.null(existing_data) && ncol(existing_data) > 0) {
      # Ajouter une colonne d'espace
      startCol <- ncol(existing_data) + 2
    }
  }
  
  # Ajouter ou sélectionner la feuille
  if (!(nom_onglet %in% openxlsx::sheets(wb))) {
    openxlsx::addWorksheet(wb, nom_onglet)
    startCol <- 1
  }
  

  # Écrire le titre en gras
  titleStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, nom_onglet, x = titre, startCol = startCol, startRow = startRow)
  openxlsx::addStyle(wb, nom_onglet, style = titleStyle, rows = startRow, cols = startCol)
  
  # Écrire les données avec les noms de colonnes
  openxlsx::writeData(wb, nom_onglet, x = table, startCol = startCol, startRow = startRow + 2,colNames = TRUE,
  borders = "surrounding",borderStyle="thick")
  
  # Nombre de lignes et de colonnes
  numRows <- nrow(table)
  numCols <- ncol(table)
  

  
  # Vérifier que toutes les colonnes de var_group_by existent dans la table
  if (!all(var_group_by %in% colnames(table))) {
    stop("Les colonnes spécifiées dans 'var_group_by' ne sont pas toutes présentes dans 'table'.")
  }
  
  # Indices des colonnes pour var_group_by
  var_group_by_indices <- match(var_group_by, colnames(table))
  
  # 1. Appliquer le style italique et aligné à gauche aux colonnes var_group_by
  italicLeftStyle <- openxlsx::createStyle(textDecoration = "italic", halign = "left")
  
  openxlsx::addStyle(
    wb,
    sheet = nom_onglet,
    style = italicLeftStyle,
    rows = (startRow + 3):(startRow + numRows + 2),  # Lignes de données
    cols = (startCol + var_group_by_indices - 1),
    gridExpand = TRUE
  )
  
  # 5. Centrer les valeurs des autres cellules (hors var_group_by)
  centerStyle <- openxlsx::createStyle(halign = "center")
  other_cols_indices <- setdiff(seq_len(numCols), var_group_by_indices)
  
  if (length(other_cols_indices) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = nom_onglet,
      style = centerStyle,
      rows = (startRow + 2):(startRow + numRows + 2),
      cols = startCol + other_cols_indices - 1,
      gridExpand = TRUE
    )
  }
  
  
    # Style gras pour l'en-tête
  headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(
    wb,
    sheet = nom_onglet,
    style = headerStyle,
    rows = startRow + 2,  # Ligne d'en-tête
    cols = startCol:(startCol + numCols - 1),
    gridExpand = TRUE
  )
  
  # Sauvegarder le classeur
  openxlsx::saveWorkbook(wb, chemin_complet, overwrite = TRUE)
}


