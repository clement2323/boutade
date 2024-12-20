% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renvoyer_figure.R
\name{creer_graphique}
\alias{creer_graphique}
\title{Créer un graphique avec ggplot2}
\usage{
creer_graphique(
  data,
  var_x,
  var_y,
  var_fill,
  lab_x,
  lab_y,
  titre_legende_fill,
  titre,
  type = "vbar",
  color_theme = c("#FF4858", "#1B7F79", "#00CCC0", "#72F2EB", "#747F7F"),
  param_position = "stack",
  save = FALSE
)
}
\arguments{
\item{data}{data.frame contenant les données}

\item{var_x}{Nom de la variable pour l'axe X}

\item{var_y}{Nom de la variable pour l'axe Y}

\item{var_fill}{Nom de la variable pour le remplissage (couleurs)}

\item{lab_x}{Label de l'axe X}

\item{lab_y}{Label de l'axe Y}

\item{titre_legende_fill}{Titre de la légende des couleurs}

\item{titre}{Titre du graphique}

\item{type}{Type de graphique ("vbar", "hbar", "pie"). Défaut: "vbar"}

\item{color_theme}{Vecteur de couleurs hexadécimales}

\item{param_position}{Position des barres ("stack", "dodge"). Défaut: "stack"}

\item{save}{Booléen pour sauvegarder le graphique. Défaut: FALSE}
}
\value{
Un objet ggplot2
}
\description{
Génère différents types de graphiques (barres verticales/horizontales, camembert)
à partir d'un jeu de données en utilisant ggplot2.
}
\examples{
\dontrun{
# Graphique en barres verticales
creer_graphique_bar(
  data = df,
  var_x = "categorie",
  var_y = "valeur",
  var_fill = "groupe",
  lab_x = "Catégories",
  lab_y = "Valeurs",
  titre_legende_fill = "Groupes",
  titre = "Mon graphique"
)

# Graphique en barres horizontales
creer_graphique_bar(
  data = df,
  var_x = "categorie",
  var_y = "valeur", 
  var_fill = "groupe",
  type = "hbar"
)

# Camembert
creer_graphique_bar(
  data = df,
  var_x = "categorie",
  var_y = "valeur",
  var_fill = "groupe", 
  type = "pie"
)
}

}
