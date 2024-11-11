Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")
library(pbapply)

# Mettre tous les éléments de la liste dans l'environnement global
rm(list=ls())

data(package = "BoutadE")
devtools::load_all()

data("metadonnees_etude") #  metadonnee d'une etude
str(metadonnees_etude)

data("metadonnees_tables") #  metadonnee d'une etude
metadonnees_tables %>% head(10)

data("EP_FI_AG")
EP_FI_AG %>% head(5)
setDT(EP_FI_AG)

data("table_demandes_valides") 
data("table_demandes_erreurs")

table_demandes <- table_demandes_valides
erreurs <- controler_demandes(table_demandes)
if(nrow(erreurs)!=0) erreurs; stop("il ya des erreurs dans les demandes")

# gestion excel ok <- 
table_demandes_valides

for (i in 1:nrow(table_demandes)){
    print(i)
    ecrire_demande_sur_xls(table_demandes[i,],metadonnees_tables)
    }

data("table_demandes_rmd")
