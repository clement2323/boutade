Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

#devtools::build_vignettes()
# Installation si nécessaire
install.packages("pkgdown")

# Initialisation de pkgdown dans votre package
pkgdown::clean_site()
usethis::use_pkgdown()
pkgdown::build_site()

# vignettes
devtools::build()
devtools::install()
browseVignettes("BoutadE")

# Crée le workflow GitHub Actions pour pkgdown
usethis::use_github_action("pkgdown")

# Installer learnr si nécessaire
install.packages("learnr")


usethis::use_tutorial("titou_tuto", "Tutoriel de Titou")


# Créer un nouveau tutoriel interactif :o :o



# dans inst/tutorials


