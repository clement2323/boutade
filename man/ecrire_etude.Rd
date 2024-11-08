% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecrire_etude.R
\name{ecrire_etude}
\alias{ecrire_etude}
\title{Génère une étude automatique avec analyses statistiques}
\usage{
ecrire_etude(
  dir_etude,
  dir_input = "input/",
  nom_fichier_demandes = "fichier_demandes.csv",
  nom_fichier_metadonnees = "metadonnees.json",
  dir_codes = "codes/",
  nom_code_setup = "Setup.R",
  nom_code_preparation = "Preparation_donnees.R",
  dir_output = "output/",
  nom_rapport = "rapport.Rmd",
  use_ia = FALSE,
  fonction_ask_ia = NULL
)
}
\arguments{
\item{dir_etude}{Chemin vers le dossier de l'étude}

\item{dir_input}{Chemin vers le dossier d'entrée contenant les fichiers de données. Par défaut "input/"}

\item{nom_fichier_demandes}{Nom du fichier CSV contenant les demandes d'analyse. Par défaut "fichier_demandes.csv"}

\item{nom_fichier_metadonnees}{Nom du fichier JSON contenant les métadonnées. Par défaut "metadonnees.json"}

\item{dir_codes}{Chemin vers le dossier contenant les scripts R. Par défaut "codes/"}

\item{nom_code_setup}{Nom du fichier de setup. Par défaut "Setup.R"}

\item{nom_code_preparation}{Nom du fichier de préparation. Par défaut "Preparation_donnees.R"}

\item{dir_output}{Chemin vers le dossier de sortie. Par défaut "output/"}

\item{nom_rapport}{Nom du fichier rapport Rmd à générer. Par défaut "rapport.Rmd"}

\item{use_ia}{Booléen indiquant si utiliser l'IA pour l'analyse. Par défaut FALSE}

\item{fonction_ask_ia}{Fonction à utiliser pour les requêtes IA. Par défaut NULL}

\item{model_name}{Nom du modèle IA à utiliser. Par défaut "mistral-small"}
}
\value{
Le chemin vers le rapport généré
}
\description{
Génère une étude automatique avec analyses statistiques
}
\examples{
ecrire_etude()
ecrire_etude(nom_fichier_demandes = "autres_demandes.csv", use_ia = FALSE)
}
