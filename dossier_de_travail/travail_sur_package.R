Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")
library(pbapply)

# Mettre tous les éléments de la liste dans l'environnement global
rm(list=ls())

devtools::load_all()
EP_FI_AG <-creer_table_minimale(20)
setDT(EP_FI_AG)
table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_valides
#table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_erreurs

erreurs <- controler_demandes(table_demandes)
if(nrow(erreurs)!=0) erreurs; stop("il ya des erreurs dans les demandes")

for (i in 1:nrow(table_demandes)){
    # i <-1
    print(i)
    renvoyer_table_from_demande(table_demandes[i,])
}

out <- pbapply(table_demandes,1,gerer_une_demande)
generer_markdown_auto(table_demandes,out)

rmarkdown::render(
    input = "output/rapport_automatique.Rmd",
    envir = globalenv(),
)


#### Lancement OLLAMA
rm(list=ls())
devtools::load_all()
# Then install the package from GitHub
devtools::install_github("clement2323/ollamax")
library(ollamax); #??ollamax
library(jsonlite)
library(pbapply)

EP_FI_AG <-creer_table_minimale(20)
table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_valides
#table_demandes <-(creer_tables_demandes(EP_FI_AG))$table_demandes_erreurs
erreurs <- controler_demandes(table_demandes)
if(nrow(erreurs)!=0){erreurs; stop("il ya des erreurs dans les demandes")}

setDT(EP_FI_AG)
metadata <- jsonlite::fromJSON("input/metadonnees.json", simplifyDataFrame = FALSE)  # Ajout de la lecture du JSON

out <- pbapply(table_demandes,1,gerer_une_demande,metadata = metadata)
out_ollama <- pbapply(table_demandes,1,gerer_une_demande,metadata = metadata,for_ollama = TRUE)


#cat(preparer_prompt(out[[1]],"EP_FI_AG",metadata))

generer_markdown_auto(
  table_demandes,
  out,
  out_ollama = out_ollama,
  fonction_ask_ollama = ask_ollama, metadata = metadata,
  prompt_header=NULL,prompt_instruction = NULL,model_name = "mistral-small"
)

# TO DO sélectionner la metadata nécessaire  seulement ?

rmarkdown::render(
  input = "output/rapport_automatique.Rmd",
  envir = globalenv(),
)



#devtools::document()
#devtools::check()

#[System.Net.WebRequest]::DefaultWebProxy = New-Object System.Net.WebProxy("http://proxy-rie.http.insee.fr:8080")
#$env:http_proxy = "http://proxy-rie.http.insee.fr:8080"
#$env:https_proxy = "http://proxy-rie.http.insee.fr:8080"
#$env:no_proxy = ""



jsonlite::fromJSON("inst/extdata/Etudes_jouet/etude_jouet_1/input/metadonnees_etudes.json")
# Création d'une liste

metadonnees_etude <- list(
  nom = "Analyse EP-FI Antilles-Guyane",
  description = "Étude des relations entre entreprises profilées et leurs filiales dans les Antilles-Guyane",
  public_cible = list(
    type = "enfant de 8-12 ans",
    instructions_communication = list(
      style = "pédagogique",
      directives = c(
        "Réponds comme si tu parlais à un enfant",
        "Utilise des métaphores simples",
        "Évite le jargon technique",
        "Inclus des exemples du quotidien",
        "Garde les phrases courtes"
      ),
      vocabulaire_adapte = list(
        entreprise_profilée = "grande entreprise",
        filiale = "petite entreprise qui appartient à la grande",
        chiffre_affaires = "argent gagné"
      )
    )
  )
)

# Sauvegarde dans data/
usethis::use_data(metadonnees_etude, overwrite = TRUE)
metadonnees_tables <- read.csv("dossier_de_travail/metadata_tables.csv")


