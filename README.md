# BoutadE : Boîte à outils à disposition des Etudes :
# BoutadE 🤖 

BoutadE est un package R qui automatise la génération de rapports d'analyse de données avec différents styles narratifs, en utilisant des modèles de langage (LLMs).

## 🎯 Objectif

Réduire la redondance dans les codes de production de tableaux et graphiques tout en rendant les analyses plus accessibles grâce à des narrateurs virtuels personnalisés.

## ✨ Fonctionnalités

- **Automatisation des analyses** : Transforme des données structurées en rapports narratifs
- **Narrateurs multiples** : 5 styles différents (yoda, ado, beauf, insee, blasé)
- **Intégration LLM** : Utilise Ollama en local pour la génération de texte
- **RAG artisanal** : Système de Retrieval Augmented Generation adapté aux besoins statistiques

## 🛠️ Installation
```r
# installation des dépendances

devtools::install_github("clement2323/ollamax")
devtools::install_github("clement2323/boutade")
```

```r
ecrire_etude("titanic",
ollama = TRUE,
model_name = "mistral-small",
role = "yoda")
```
## Structure de dossiers

boutade/
├── metadonnees/
│ ├── demandes/ # Configurations des analyses
│ ├── roles/ # Définitions des personnages
│ └── metadonnees_tables/ # Structure des données
├── R/
│ └── fonctions.R # Fonctions principales
└── inst/
└── templates/ # Templates de rapports


## 🔒 Confidentialité

- Utilisation de LLMs hébergés localement (via Ollama)
- Contrôle total sur les données et leur traitement
- Pas de dépendance aux services cloud externes

## 📚 Documentation

- [Documentation complète](https://clement2323.github.io/boutade/)
- [Package ollamax](https://github.com/clement2323/ollamax)

## 🤝 Contribution

Les contributions sont les bienvenues ! N'hésitez pas à :
- Ouvrir une issue
- Proposer une pull request
- Suggérer de nouveaux styles de narrateurs

## ✍️ Auteur

Clément Guillo - INSEE

