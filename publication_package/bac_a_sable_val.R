Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

install.packages("devtools") # aide au developpement de package
devtools::load_all() # elle permet de charger toutes les fonctions contenues dans le dossier R, comme si tu avais fait un "library(Boutade)"

EP_FI_AG <- BoutadE::creer_table_minimale(50) # créer une fausse table
setDT(EP_FI_AG)
table_demandes <- (BoutadE:::creer_tables_demandes(EP_FI_AG))$table_demandes_valides # créer la fausse table de demande associée

BoutadE::controler_demandes(table_demandes) # rréalise des contrôles sur la table demandes pour vérifier qu'elle est bien écrite


# boucle qui applique la fonction gere_une_demande à toutes les demandes
for (i in 1:nrow(table_demandes)){
    print(i)
    BoutadE::gerer_une_demande(table_demandes[i,]%>% unlist())
}

out <- apply(table_demandes,1,gerer_une_demande)

devtools::install_github("clement2323/ollamax")
library(ollamax) #??ollamax

table <- out[[1]]
metadata <- jsonlite::fromJSON("input/metadonnees.json", simplifyDataFrame = FALSE)  # Ajout de la lecture du JSON
nom_table <- table_demandes[1,]$table

prompt <- preparer_prompt(table,nom_table,metadata)
#cat(prompt)
ask_ollama(prompt)
