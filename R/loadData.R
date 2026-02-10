#' Charge des données depuis une base via une requête SQL
#'
#' Cette fonction établit une connexion à la base de données via ODBC (DSN = "dcshdb"), exécute la requête SQL passée en argument, récupère les résultats sous forme de tibble, puis ferme la connexion.
#'
#' @param query Chaîne de caractères contenant une requête SQL.
#'
#' @return Un objet `tibble` contenant les résultats de la requête.
#'
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#'   req <- "SELECT * FROM ma_table"
#'   data <- loadData(req)
#' }
#'
#' @export


loadData <- function(query = character()){
  con <- DBI::dbConnect(odbc::odbc(), dsn = "dcshdb")

  dataLoaded <- DBI::dbGetQuery(con,query) |>
    tibble::as_tibble()


  DBI::dbDisconnect(con)


  return(dataLoaded)
}
