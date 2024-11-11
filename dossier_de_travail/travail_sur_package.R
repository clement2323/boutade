Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

devtools::install_github("clement2323/ollamax")
library(ollamax) #??ollamax
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
for (i in 1:nrow(table_demandes)){
    print(i)
    ecrire_demande_sur_xls(table_demandes[i,],metadonnees_tables)
    }

# sans ollama

data("table_demandes_rmd")
liste_info_chunk <- lapply(
        1:nrow(table_demandes_rmd),
        function(i) generer_figure_from_demande(table_demandes_rmd[i,],metadonnees_tables)
    )

liste_figures <- lapply(out,function(element)element$figure)

generer_markdown_auto_simple(liste_info_chunk)
 
# avec ollama 
data("table_demandes_rmd")
liste_info_chunk <- pblapply(
        1:nrow(table_demandes_rmd),
        function(i) generer_figure_from_demande(
            table_demandes_rmd[i,],
            metadonnees_tables,
            ollama  = TRUE,
            fonction_ask = function(prompt) ask_ollama(prompt,"mistral-small"),
            metadonnees_etude = metadonnees_etude
            )
    )




