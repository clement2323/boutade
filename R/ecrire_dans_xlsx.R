#' Écrire un Tableau avec Formatage et Titre dans un Fichier Excel
#'
#' Cette fonction écrit un tableau de données dans un fichier Excel en ajoutant un titre formaté 
#' et en appliquant des styles de formatage au tableau. Elle gère également la création du 
#' répertoire de sortie si nécessaire et permet d'ajouter des données à un onglet existant 
#' en décalant les nouvelles données.
#'
#' @param nom_fichier_xls \code{character} : Le nom du fichier Excel à créer ou modifier (par exemple, "rapport.xlsx").
#' @param nom_onglet \code{character} : Le nom de l'onglet (feuille de calcul) où les données seront écrites.
#' @param table \code{data.frame} : Le tableau de données à écrire dans le fichier Excel.
#' @param titre \code{character} : Le titre à insérer au-dessus du tableau dans l'onglet Excel.
#' @param var_group_by \code{numeric} : Le nombre de premières colonnes à formater spécifiquement 
#'   (par exemple, pour des regroupements ou des croisements).
#' @param dir \code{character} : Le répertoire où le fichier Excel sera enregistré. Par défaut, "output/".
#'
#' @return \code{NULL}. La fonction écrit les données dans le fichier Excel spécifié.
#'
#' @details
#' La fonction réalise les opérations suivantes :
#' \itemize{
#'   \item Vérifie si le package \code{openxlsx} est installé et le charge.
#'   \item Crée le répertoire de sortie s'il n'existe pas.
#'   \item Charge un classeur Excel existant ou en crée un nouveau.
#'   \item Détermine la position de départ pour écrire les nouvelles données en fonction 
#'         des données existantes dans l'onglet spécifié.
#'   \item Ajoute ou sélectionne l'onglet spécifié.
#'   \item Écrit le titre en gras et rouge au-dessus du tableau.
#'   \item Appelle la fonction \code{\link{ecrire_tableau_formate}} pour écrire et formater le tableau.
#'   \item Sauvegarde le classeur Excel.
#' }
#'
#' @import openxlsx
#'
#' @examples
#' \dontrun{
#' library(openxlsx)
#'
#' # Créer un exemple de tableau
#' donnees <- data.frame(
#'   Nom = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35),
#'   Score = c(85, 90, 95)
#' )
#'
#' # Définir le titre et les variables de groupement
#' titre <- "Rapport des Scores"
#' var_group_by <- 2
#'
#' # Utiliser la fonction pour écrire le tableau dans un fichier Excel
#' ecrire_xls(
#'   nom_fichier_xls = "rapport_scores.xlsx",
#'   nom_onglet = "Feuille1",
#'   table = donnees,
#'   titre = titre,
#'   var_group_by = var_group_by,
#'   dir = "output/"
#' )
#' }
#'
#' @export
ecrire_xls <- function(nom_fichier_xls, nom_onglet, table, titre, var_group_by, var_evolution = NULL, dir = "output/") {
  
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
  

  # Écrire le titre en gras et rouge
  titleStyle <- openxlsx::createStyle(textDecoration = "bold", fontColour = "blue")
  openxlsx::writeData(wb, nom_onglet, x = titre, startCol = startCol, startRow = startRow)
  openxlsx::addStyle(wb, nom_onglet, style = titleStyle, rows = startRow, cols = startCol)
  
  # données
  n_colonnes_en_italique_gauche <- length(var_group_by)
  if(!is.null(var_evolution)) n_colonnes_en_italique_gauche <- n_colonnes_en_italique_gauche - 1
  wb <- ecrire_tableau_formate(wb,nom_onglet, table,startRow+2, startCol,n_colonnes_en_italique_gauche)
  
  # Sauvegarder le classeur
  openxlsx::saveWorkbook(wb, chemin_complet, overwrite = TRUE)
}



#' Écrire et Formater un Tableau dans un Classeur Excel (Parce que les CSV c'est trop 2010)
#'
#' Cette fonction transforme vos données ternes en un magnifique chef-d'œuvre Excel ! 
#' Elle applique des styles qui feraient pâlir de jalousie votre designer préféré : 
#' en-têtes en gras (pour impressionner votre chef), bordures stratégiques (parce que les 
#' frontières c'est important), et un centrage parfait (comme une pizza bien cuite).
#'
#' @param wb Un objet \code{\link[openxlsx]{Workbook}} - Le canvas de votre future œuvre d'art Excel.
#' @param nom_onglet \code{character} : Le nom de l'onglet où la magie va opérer.
#' @param tableau \code{data.frame} : Vos données, qui attendent impatiemment leur makeover.
#' @param startRow \code{numeric} : La ligne de départ (parce qu'il faut bien commencer quelque part).
#' @param startCol \code{numeric} : La colonne de départ (à gauche, comme pour l'écriture, sauf si vous êtes fan de manga).
#' @param n_var_croisement \code{numeric} : Le nombre de colonnes qui méritent un traitement VIP.
#'
#' @return Un objet \code{\link[openxlsx]{Workbook}} transformé, plus beau qu'avant (promis).
#'
#' @details
#' La fonction fait tout ça (et même plus) :
#' \itemize{
#'   \item Écrit vos données (sans les faire tomber)
#'   \item Ajoute des en-têtes en gras (pour faire sérieux)
#'   \item Met des bordures (comme un château fort, mais en plus moderne)
#'   \item Centre le texte (parce que l'alignement à gauche, c'est has-been)
#'   \item Ajuste les colonnes automatiquement (comme par magie ✨)
#' }
#'
#' @import openxlsx
#'
#' @examples
#' \dontrun{
#' library(openxlsx)
#' 
#' # Créons un tableau pas très original
#' donnees <- data.frame(
#'   Superheros = c("Batman", "Superman", "Wonder Woman"),
#'   Pouvoir = c("Être riche", "Tout faire", "Lasso de vérité"),
#'   Score = c(42, 100, 95)  # Batman a un score de 42 car c'est la réponse à tout
#' )
#' 
#' # La magie commence ici...
#' wb <- createWorkbook()  # Abracadabra !
#' wb <- ecrire_tableau_formate(
#'   wb = wb,
#'   nom_onglet = "Super_Tableau",
#'   tableau = donnees,
#'   startRow = 1,
#'   startCol = 1,
#'   n_var_croisement = 2
#' )
#' 
#' # Sauvegardons cette merveille
#' saveWorkbook(wb, "chef_doeuvre.xlsx", overwrite = TRUE)
#' }
#'
#' @export

ecrire_tableau_formate <- function(wb,nom_onglet, tableau, startRow, startCol, n_var_croisement) {
  
  # Écrire le tableau dans la feuille de calcul
  writeData(wb, nom_onglet, tableau,startCol = startCol, startRow = startRow)
  
  # Créer un style pour les en-têtes : texte en gras et bordure en bas
  style_entete <- createStyle(textDecoration = "bold", border = "bottom")
  
  # Appliquer le style aux en-têtes (première ligne)
  addStyle(wb, nom_onglet, style_entete, rows = startRow, cols = startCol:(startCol+ncol(tableau)-1), gridExpand = TRUE)
  
  # Créer un style pour la bordure à droite
  style_bordure_droite <- createStyle(
    border = "right",
    textDecoration = "italic", 
    halign = "left"
    )
  
  # Déterminer le nombre total de lignes (y compris les en-têtes)
  total_lignes <- nrow(tableau) + 1  # +1 pour les en-têtes
  
  # Appliquer la bordure à droite aux deux premières colonnes
  addStyle(wb, nom_onglet, style_bordure_droite, 
           rows = startRow:(startRow+total_lignes -1), cols = startCol:(startCol+n_var_croisement-1), gridExpand = TRUE, stack = TRUE)
  
  style_centre <- createStyle(halign = "center") 
  
  # Appliquer le style centré aux colonnes restantes (hors en-têtes)
  addStyle(
    wb, nom_onglet, style_centre,
    rows = (startRow+1):(startRow + nrow(tableau) ),  # Exclure la première ligne (en-têtes)
    cols = (startCol+n_var_croisement):(startCol+n_var_croisement+ncol(tableau)-1),        # Colonnes 3 à n
    gridExpand = TRUE,
    stack = TRUE
  )
  # Ajuster automatiquement la largeur des colonnes
  
  # 6. Ajuster automatiquement la largeur des colonnes avec une largeur minimale
  min_width <- 10  # Définir la largeur minimale souhaitée
  
  for (i in 1:ncol(tableau)) {
    # Calculer la longueur maximale des données dans la colonne
    max_length <- max(nchar(as.character(tableau[[i]])), na.rm = TRUE)
    header_length <- nchar(colnames(tableau)[i])
    max_length <- max(max_length, header_length)
    
    # Approximer la largeur Excel (facteur d'ajustement, par exemple 1.2)
    width <- max(max_length * 1.2, min_width)
    # Définir la largeur de la colonne
    setColWidths(wb, nom_onglet, cols = startCol + i -1, widths = width)
  }
  
  wb
}

