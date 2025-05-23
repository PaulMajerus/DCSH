#' Crée un script R structuré avec en-tête et sections prédéfinies
#'
#' Cette fonction génère automatiquement un fichier `.R` avec une structure standardisée,
#' incluant un en-tête (titre, auteur, date, version) et des sections de travail classiques.
#' Le fichier est nommé à partir de la date et du nom fourni, nettoyé au format camelCase.
#'
#' @param auteur Character. Nom de l’auteur à insérer dans l’en-tête. Par défaut `"Paul Majérus"`.
#' @param titre Character. Titre affiché en haut du fichier. Par défaut `""`.
#' @param fileName Character. Élément textuel à inclure dans le nom de fichier, nettoyé avec `make_clean_names(..., "small_camel")`. Peut être vide.
#' @param version Character. Numéro de version du script. Par défaut `"1"`.
#' @param path Character. Chemin du répertoire où le script sera enregistré. Par défaut `"."`.
#'
#' @return Aucun objet retourné, mais un fichier `.R` est créé sur disque. Affiche un message avec le chemin absolu du fichier.
#'
#' @importFrom janitor make_clean_names
#' @importFrom tools toTitleCase
#' @importFrom utils installed.packages
#'
#' @examples
#' \dontrun{
#' create_new_script(auteur = "Paul Majérus",
#'                   titre = "Analyse des séjours hospitaliers",
#'                   fileName = "sejours_2025",
#'                   version = "2",
#'                   path = "R/scripts")
#' }
#'
#' @export

scriptTemplate <- function(auteur = "Paul Majérus", titre="",
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

    "# Pistes d'amelioration du programme ================================== ----\n\n",
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
  message("Script créé : ", normalizePath(file_path))
}
