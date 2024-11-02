#' Write a table to an Excel file with formatting, appending to existing sheets
#'
#' @param chemin_xlsx Path to the Excel file.
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
ecrire_xls <- function(chemin_xlsx, nom_onglet, table, titre, dir="/output") {
  # Check if openxlsx package is installed
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required but not installed.")
  }
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Build full file path
  chemin_complet <- file.path(dir, chemin_xlsx)
  
  # Load or create the workbook
  if (file.exists(chemin_complet)) {
    wb <- openxlsx::loadWorkbook(chemin_complet)
  } else {
    wb <- openxlsx::createWorkbook()
  }
  
  # Determine the starting column and row
  if (nom_onglet %in% openxlsx::sheets(wb)) {
    # Read existing data from the sheet
    existing_data <- openxlsx::readWorkbook(wb, sheet = nom_onglet, colNames = FALSE, rowNames = FALSE)
    
    if (!is.null(existing_data) && ncol(existing_data) > 0) {
      last_col <- ncol(existing_data)
      startCol <- last_col + 2  # Leave one column of space
    } else {
      startCol <- 1
    }
  } else {
    # Add the worksheet if it doesn't exist
    openxlsx::addWorksheet(wb, nom_onglet)
    startCol <- 1
  }
  
  # Starting row for writing
  startRow <- 1
  
  # Write the title in bold
  titleStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, sheet = nom_onglet, x = titre, startCol = startCol, startRow = startRow)
  openxlsx::addStyle(wb, sheet = nom_onglet, style = titleStyle, rows = startRow, cols = startCol, gridExpand = TRUE)
  
  # Write the table with bold headers
  headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::writeData(wb, sheet = nom_onglet, x = table, startCol = startCol, startRow = startRow + 1, headerStyle = headerStyle)
  
  # Apply italic style to the first column of the new table (excluding header)
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
  }
  
  # Save the workbook with the complete path
  openxlsx::saveWorkbook(wb, chemin_complet, overwrite = TRUE)
}
