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

EP_FI_AG <-creer_table_minimale(20)
table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_valides
#table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_erreurs

erreurs <- controler_demandes(table_demandes)
if(nrow(erreurs)!=0) erreurs; stop("il ya des erreurs dans les demandes")

setDT(EP_FI_AG)
for (i in 1:nrow(table_demandes)){
    print(i)
    gerer_une_demande(table_demandes[i,]%>% unlist())
}

out <- apply(table_demandes,1,gerer_une_demande)
generer_markdown_auto(table_demandes,out)


rmarkdown::render(
    input = "output/rapport_automatique.Rmd",
    envir = globalenv(),
)

