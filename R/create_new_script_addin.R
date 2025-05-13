#' Add-in interactif pour créer un script standardisé
#'
#' @return Crée un fichier script dans le répertoire courant
#' @export
create_new_script_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("RStudio API non disponible.")
  }

  # Demander les valeurs à l'utilisateur
  titre <- rstudioapi::showPrompt(title = "Titre", message = "Titre du script", default = "Titre")
  if (is.null(titre)) return(invisible(NULL))

  fileName <- rstudioapi::showPrompt(title = "Nom de fichier", message = "Nom du fichier (sans extension)", default = "nouveau_script")
  if (is.null(fileName)) return(invisible(NULL))

  auteur <- rstudioapi::showPrompt(title = "Auteur", message = "Nom de l'auteur", default = Sys.info()[["user"]])
  if (is.null(auteur)) return(invisible(NULL))

  version <- rstudioapi::showPrompt(title = "Version", message = "Version du script",
                                    default = 1)
  if (is.null(auteur)) return(invisible(NULL))

  # Appel à ta fonction principale
  create_new_script(
    auteur = auteur,
    titre = titre,
    fileName = fileName,
    version = version
  )
}
