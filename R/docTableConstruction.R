#' Jeu de données : tableConstructionDB
#'
#' Description du contenu de la table de construction.
#'
#' @format Un tibble de 442 lignes et 7 colonnes :
#' \describe{
#'   \item{table}{Nom de la table dans la base de données.}
#'   \item{column}{Nom de la colonne dans la table de données.}
#'   \item{version}{Version du format de données.}
#'   \item{taxonomie}{Taxonomie de la variable.}
#'   \item{restriction}{Condition de sélection de l'information pour accéder à la variable.}
#'   \item{abrev}{Abréviation de nomination des tables dans sql.}
#'   \item{typeTable}{Type de table.}
#' }
#' @source Basé sur la base de données DCSH de la Disa.
#' @usage data(tableConstructionDB)
#' @docType data
#' @keywords datasets
#' @name tableConstructionDB
"tableConstructionDB"
