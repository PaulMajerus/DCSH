% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scriptTemplate.R
\name{scriptTemplate}
\alias{scriptTemplate}
\title{Crée un script R structuré avec en-tête et sections prédéfinies}
\usage{
scriptTemplate(
  auteur = "Paul Majerus",
  titre = "",
  fileName = character(),
  version = "1",
  path = "."
)
}
\arguments{
\item{auteur}{Character. Nom de l auteur a inserer dans l’en-tete. Par defaut \code{"Paul Majerus"}.}

\item{titre}{Character. Titre affiche en haut du fichier. Par defaut \code{""}.}

\item{fileName}{Character. Element textuel a inclure dans le nom de fichier, nettoye avec \code{make_clean_names(..., "small_camel")}. Peut etre vide.}

\item{version}{Character. Numero de version du script. Par defaut \code{"1"}.}

\item{path}{Character. Chemin du repertoire ou le script sera enregistre. Par defaut \code{"."}.}
}
\value{
Aucun objet retourne, mais un fichier \code{.R} est cree sur disque. Affiche un message avec le chemin absolu du fichier.
}
\description{
Cette fonction genere automatiquement un fichier \code{.R} avec une structure standardisee,
incluant un en-tete (titre, auteur, date, version) et des sections de travail classiques.
Le fichier est nomme a partir de la date et du nom fourni, nettoye au format camelCase.
}
\examples{
\dontrun{
create_new_script(auteur = "Paul Majerus",
                  titre = "Analyse des sejours hospitaliers",
                  fileName = "sejours_2025",
                  version = "2",
                  path = "R/scripts")
}

}
