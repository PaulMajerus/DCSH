#' Load Required Packages
#'
#' This function loads a vector of package names. It checks if the packages are
#' installed and throws an error if not. Intended for use within packages to
#' load dependencies that are already declared.
#'
#' @param packages Character vector of package names to load.
#' @return Invisible. Loads the packages into the session.
#' @export
#' @examples
#' \dontrun{
#' loadPackages(c("dplyr", "ggplot2"))
#' }

importPackages <- function(packages = character()) {
  # check if packages are installed
  missing <- packages[!packages %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         ". Please install them manually before proceeding.")
  }

  # load if not already loaded
  loaded <- sub("^package:", "", grep("^package:", search(), value = TRUE))

  invisible(lapply(packages, function(pkg) {
    if (!pkg %in% loaded) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      message("Loaded package: ", pkg)
    }
  }))
}
