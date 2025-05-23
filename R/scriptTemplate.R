#' Crée un script R structuré avec en-tête et sections prédéfinies
#'
#' Cette fonction genere automatiquement un fichier `.R` avec une structure standardisee,
#' incluant un en-tete (titre, auteur, date, version) et des sections de travail classiques.
#' Le fichier est nomme a partir de la date et du nom fourni, nettoye au format camelCase.
#'
#' @param auteur Character. Nom de l auteur a inserer dans l’en-tete. Par defaut `"Paul Majerus"`.
#' @param titre Character. Titre affiche en haut du fichier. Par defaut `""`.
#' @param fileName Character. Element textuel a inclure dans le nom de fichier, nettoye avec `make_clean_names(..., "small_camel")`. Peut etre vide.
#' @param version Character. Numero de version du script. Par defaut `"1"`.
#' @param path Character. Chemin du repertoire ou le script sera enregistre. Par defaut `"."`.
#'
#' @return Aucun objet retourne, mais un fichier `.R` est cree sur disque. Affiche un message avec le chemin absolu du fichier.
#'
#' @importFrom janitor make_clean_names
#' @importFrom tools toTitleCase
#' @importFrom utils installed.packages
#'
#' @examples
#' \dontrun{
#' create_new_script(auteur = "Paul Majerus",
#'                   titre = "Analyse des sejours hospitaliers",
#'                   fileName = "sejours_2025",
#'                   version = "2",
#'                   path = "R/scripts")
#' }
#'
#' @export

scriptTemplate <- function(auteur = "Paul Majerus", titre="",
                              fileName = character(),
                              version = "1", path = ".") {
  date_obj <- Sys.Date()
  mois <- format(date_obj, "%B")
  mois <- tools::toTitleCase(mois) # Pour que le mois ait une majuscule
  annee <- format(date_obj, "%Y")
  date_txt <- format(date_obj, "%Y%m%d")
  fileName <- janitor::make_clean_names(fileName,"small_camel")

  # Nom du fichier : script_YYYY-MM-DD.R
  file_name <- paste0(date_txt,fileName,".R")
  file_path <- file.path(path, file_name)

  header <- paste0(
    "# ===================================================================== \n",
    "# ",  titre, "\n",
    "# Auteur: ", auteur, "\n",
    "# Date: ", mois, " ", annee, "\n",
    "# Version: Version ", version, "\n",
    "# ===================================================================== ----\n\n\n",

    "# Contexte ============================================================ ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# Input ============================================================= ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# Output ============================================================ ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# Index =============================================================== ----\n",
    "# 1. Parametrage du programme\n\n",
    "# 2. Import des packages et fonctions\n\n",
    "# ===================================================================== ----\n\n\n",

    "# Remarques =========================================================== ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# Pistes d amelioration du programme ================================== ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# 1. Parametrage du programme ========================================= ----\n\n",
    "# ===================================================================== ----\n\n\n",

    "# 2. Import des packages et fonctions ================================= ----\n\n",
    "DCSH::importPackages(c(\"tidyverse\",\n",
    "                 \"readr\",\n",
    "                 \"readxl\",\n",
    "                 \"janitor\",\n",
    "                 \"rlang\",\n",
    "                 \"purrr\",\n",
    "                 \"questionr\",\n",
    "                 \"countrycode\",\n",
    "                 \"odbc\"))\n\n",
    "# ===================================================================== ----\n\n\n"
  )

  writeLines(header, con = file_path)
  message("Script cree : ", normalizePath(file_path))
}
