% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creer_exemple_minimal.R
\name{creer_tables_demandes}
\alias{creer_tables_demandes}
\title{Créer des tables de demandes pour les tests}
\usage{
creer_tables_demandes()
}
\value{
Une liste contenant deux data.frames :
\describe{
\item{table_demandes_valides}{20 exemples de demandes valides avec différentes combinaisons de :
\itemize{
\item Variables de croisement et relatives
\item Variables quantitatives
\item Fonctions d'agrégation
\item Types de sortie (table/graphique)
}
}
\item{table_demandes_erreurs}{20 exemples de demandes invalides illustrant différents cas d'erreur :
\itemize{
\item Variables inexistantes
\item Combinaisons invalides
\item Fonctions d'agrégation incorrectes
\item Formats de sortie incompatibles
}
}
}
}
\description{
Génère deux tables de données pour tester le traitement des demandes :
\itemize{
\item Une table de demandes valides avec différentes combinaisons de paramètres
\item Une table de demandes invalides pour tester la gestion des erreurs
}
}
\examples{
# Créer les tables de test
tables <- creer_tables_demandes()

# Accéder aux demandes valides
demandes_valides <- tables$table_demandes_valides

# Accéder aux demandes avec erreurs
demandes_erreurs <- tables$table_demandes_erreurs

}
