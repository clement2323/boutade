Sys.setenv(no_proxy = "")
Sys.setenv(https_proxy ="http://proxy-rie.http.insee.fr:8080")
Sys.setenv(http_proxy ="http://proxy-rie.http.insee.fr:8080")

install.packages("devtools") # aide au developpement de package
devtools::load_all() # simuler un library(BoutadE) aveec les codes du dossier R modifiés.


EP_FI_AG <- creer_table_minimale(50) # créer une fausse table
setDT(EP_FI_AG) # transfoen data.table obligatoire

# test calculer agregat
calculer_agregat_sur_croisement(
  EP_FI_AG,
  "dep_FI",
  "dep_EP",
  "redi_r310_FI",
  list("sum"=sum),
  "unite",
  condition = NULL
)

table_demandes <- read.csv("input/table_demandes.csv")

#write.csv(table_demandes_valide,"input/table_demandes.csv",quote = FALSE,row.names=FALSE)
if(controler_demandes(table_demandes) |> nrow() >0) stop("Des demandes ne sont pas valides")
out <- pbapply(table_demandes,1,gerer_une_demande)

# Premiere option de lancement pour debugage (on prit le numero de la demande à chaque fois au cas ou ça bugg, on sait laquelle déconne et on essaie de "debugger la demande")
out <- list()
for (i in 1:nrow(table_demandes)){
    print(i) # si une commande bug je vois où ça debugge 
    res <- gerer_une_demande(table_demandes[i,]%>% unlist())
    out <- c(out,res)
}

# Attention peut etre qu'on a pas controler tous les cas de merde possible donc ça ne veut pas forcement direr que gerer_une_demande va passer (mais ça nous rassur un peu)
# gerer les demandes valides : 
#Option plus compact en mode non debugage
