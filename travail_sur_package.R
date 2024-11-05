install.packages("devtools")
devtools::document()
devtools::check()
devtools::load_all()
getwd()
setwd('codes/BoutadE')
usethis::use_package('openxlsx')

[System.Net.WebRequest]::DefaultWebProxy = New-Object System.Net.WebProxy("http://proxy-rie.http.insee.fr:8080")
$env:http_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:https_proxy = "http://proxy-rie.http.insee.fr:8080"
$env:no_proxy = ""


# Mettre tous les éléments de la liste dans l'environnement global
rm(list=ls())

#to DO gérer le markdown
# metadonnées
# controle demande 
EP_FI_AG <-creer_table_minimale(20)
tables_demandes <- creer_tables_demandes(EP_FI_AG)

controler_demandes(tables_demandes$table_demandes_valides)
controler_demandes(tables_demandes$table_demandes_erreurs)

