#' Addin pour créer un script R standardisé
#'
#' @return Ouvre un script dans le répertoire courant avec structure type
#' @export
create_new_script_addin <- function() {
  rstudioapi::showDialog("Création script",
                         "Un script structuré va être créé dans le répertoire courant.")
  scriptTemplate(
    auteur = "Paul Majérus",
    titre = "",
    fileName = "nouveau_script"
  )
}
