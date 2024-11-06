Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

# Installation si nécessaire
install.packages("pkgdown")

# Initialisation de pkgdown dans votre package
pkgdown::clean_site()
usethis::use_pkgdown()
pkgdown::build_site()

# Crée le workflow GitHub Actions pour pkgdown
usethis::use_github_action("pkgdown")
