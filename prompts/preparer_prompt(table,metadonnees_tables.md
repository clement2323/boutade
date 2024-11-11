preparer_prompt(table,metadonnees_tables,metadonnees_etude)

metadonnees_tables
                       nom_etude nom_table   nom_variable
1  Analyse EP-FI Antilles-Guyane  EP_FI_AG          annee
2  Analyse EP-FI Antilles-Guyane  EP_FI_AG       siren_EP
3  Analyse EP-FI Antilles-Guyane  EP_FI_AG       denom_EP
4  Analyse EP-FI Antilles-Guyane  EP_FI_AG         dep_EP
5  Analyse EP-FI Antilles-Guyane  EP_FI_AG          s4_EP
6  Analyse EP-FI Antilles-Guyane  EP_FI_AG   redi_r310_EP
7  Analyse EP-FI Antilles-Guyane  EP_FI_AG       siren_FI
8  Analyse EP-FI Antilles-Guyane  EP_FI_AG       denom_FI
9  Analyse EP-FI Antilles-Guyane  EP_FI_AG         dep_FI
10 Analyse EP-FI Antilles-Guyane  EP_FI_AG          s4_FI
11 Analyse EP-FI Antilles-Guyane  EP_FI_AG   redi_r310_FI
12 Analyse EP-FI Antilles-Guyane  EP_FI_AG          poids
13 Analyse EP-FI Antilles-Guyane  EP_FI_AG dep_FI_relatif
                                       libelle libelle_court
1                           Année de référence         Année
2        Numéro SIREN de l'entreprise profilée      SIREN EP
3        Dénomination de l'entreprise profilée        Nom EP
4         Département de l'entreprise profilée       Dép. EP
5  Secteur d'activité de l'entreprise profilée    Secteur EP
6  Chiffre d'affaires de l'entreprise profilée         CA EP
7                    Identifiant de la filiale         ID FI
8                   Dénomination de la filiale        Nom FI
9                    Département de la filiale       Dép. FI
10            Secteur d'activité de la filiale    Secteur FI
11            Chiffre d'affaires de la filiale         CA FI
12                      Poids de l'observation         Poids
13         Localisation relative de la filiale Loc. relative
                                                                       
              description
1                                                               Année des données (2022 ou 2023)
2                                       Identifiant unique à 9 chiffres de l'entreprise profilée
3                                                        Raison sociale de l'entreprise profilée
4                                     Code département de l'entreprise profilée (971, 972, etc.)
5                           Secteur d'activité en 4 postes (Industrie, Services marchands, etc.)
6                                         Montant du chiffre d'affaires de l'entreprise profilée
7                                                               Identifiant unique de la filiale
8                                                                   Raison sociale de la filiale
9                                            Code département de la filiale (971, 972, 973, 974)
10       Secteur d'activité en 4 postes (Industrie, Construction, Commerces, Services marchands)
11                                                   Montant du chiffre d'affaires de la filiale
12                                    Coefficient de pondération de l'observation (1 par défaut)
13 Position géographique de la filiale par rapport à son EP (interieur, antilles-guyane, autres)

> metadonnees_etude 
$nom
[1] "Analyse EP-FI Antilles-Guyane"

$description
[1] "Étude des relations entre entreprises profilées et leurs filiales dans les Antilles-Guyane"

$public_cible
$public_cible$type
[1] "enfant de 8-12 ans"

$public_cible$instructions_communication
$public_cible$instructions_communication$style
[1] "pédagogique"

$public_cible$instructions_communication$directives
[1] "Réponds comme si tu parlais à un enfant"
[2] "Utilise des métaphores simples"
[3] "Évite le jargon technique"
[4] "Inclus des exemples du quotidien"
[5] "Garde les phrases courtes"

$public_cible$instructions_communication$vocabulaire_adapte
$public_cible$instructions_communication$vocabulaire_adapte$entreprise_profilée
[1] "grande entreprise"

$public_cible$instructions_communication$vocabulaire_adapte$filiale    
[1] "petite entreprise qui appartient à la grande"

$public_cible$instructions_communication$vocabulaire_adapte$chiffre_affaires
[1] "argent gagné"

A partir de ces éléments, ecris moi la fonctio préparer prompt(tu ^peux ladecouper en module si tuveux).

Cette fonction prend les meta do,nnees_etude pour adapter la réponse du language moodelee auquel on va demander une description des données.
Attention dans la demande de description car je vaisevnvoyer un tableau de données qui dans certains cas nourrira un graphique et dans certains autrres une table. Ainsi quand tu crées le prompt qui demande la description du tableau, laisses subtilement le flou sur ce que tu décris (tableau ou graphique).