#' Installer un package s'il n'est pas déjà installé
#'
#' Cette fonction vérifie si le package spécifié est installé. Si ce n'est pas le cas, elle l'installe depuis CRAN.
#'
#' @param pkg Un caractère unique indiquant le nom du package à installer.
#' @return Aucun retour de valeur. Le package est installé si nécessaire.
#' @examples
#' \dontrun{
#' installer_package("ggplot2")
#' }
#' @export
installer_package <- function(pkg) {
  print(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

