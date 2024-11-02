#' Installer un package s'il n'est pas dejà installe
#'
#' Cette fonction verifie si le package specifie est installe. Si ce n'est pas le cas, elle l'installe depuis CRAN.
#'
#' @param pkg Un caractere unique indiquant le nom du package à installer.
#' @return Aucun retour de valeur. Le package est installe si necessaire.
#' @examples
#' \dontrun{
#' installer_package("ggplot2")
#' }
#' @export
installer_package <- function(pkg) {
  # print(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

