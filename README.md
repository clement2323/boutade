## Informations générales

1) toujours choisir entre type_output graphique ou tableau quitte à dupliquer les lignes
2) quand aucun nom_fichier_xls n'est renseigné, je suppose que l'on part sur markdown avec kable
3) Ne pas renseigner de foichier xlsx pour les graphiques
4) workflow :
    - autant rester sur github : 
        a) telecharger le zip du package dans le repo github
        b) mettre ce zip dans un dossier sur aus
        c) installation manuelle du package agrace au zip ?
5) il y a autant de fonctions que de variables (l'application est parallele), la somme est gérée differemmment par rapport aux autres
6) pareil, autant d'unites que de variables unités possibles -> centieme,dixieme,unite,millier,million


## TO DO 
- prompt avec l'angle d'attaque rajouter titre et sous titre dans le contexte -> idée à partir d'un plan détaillé d'étude bien structuré avec markdown, générer le fichier demande et donc l'étude demandée ?
- calculer evolution
- parametre type graphique 
- Autoriser graphique meme pour indicateurs simples, si plusieurs indicateurs demandés faire le graphique associé avec un facet ou un patchwork?
- Prompt global de synthèe par nom onglet sous titre de rmarkdown pour éviter les répétitions detc..
- Travailler les éléments de langage, on peut centrer des postes sur l'analyse seulement ! par exemple les personnes qui ne souhaitent pas coder
- chargement et decoupage d'un dictionnaire de dionnée (lien avec hermes) pour générer la metadonnées
- ecrire les regles de remplissage du fichier demande bien au cliar, pour engendrer la fonction associée
- mettre en place des vignettes d'utilisation
- chaénger la référence au repertoire dans esane_ent ou ttianic par un getwd() ?
