#' Chargement des tables dimensionnelles standardisées depuis la base `dcshdb`
#'
#' Cette fonction se connecte à la base de données `dcshdb` via ODBC, récupère les tables dont le nom commence par `dim_`, applique une table de conversion pour sélectionner et renommer certaines tables selon une taxonomie standard, puis les importe toutes sous forme de liste de data.frames nommés.
#'
#'
#' @return Une liste nommée de `data.frame`, chaque élément correspondant à une table dimensionnelle importée, avec des noms nettoyés en camelCase via `janitor::make_clean_names`.
#'
#' @details
#' La fonction effectue les étapes suivantes :
#' \itemize{
#'   \item Connexion à la base de données `dcshdb` via le DSN ODBC.
#'   \item Liste toutes les tables disponibles.
#'   \item Filtre celles dont le nom commence par `dim_`.
#'   \item Applique une table de conversion pour mapper les noms internes vers une taxonomie ciblée (et filtre les noms à exclure).
#'   \item Exécute un `SELECT *` pour chaque table sélectionnée.
#'   \item Retourne les résultats dans une liste nommée.
#' }
#'
#' @import DBI
#' @import odbc
#' @import stringr
#' @import dplyr
#' @import janitor
#'
#' @examples
#' \dontrun{
#' dims <- dimLoad()
#' names(dims)
#' head(dims$dimPrta)
#' }
#'
#' @export
dimLoad <- function(){

  con <- DBI::dbConnect(odbc::odbc(), dsn = "dcshdb")

  # Lister toutes les tables
  tables <- DBI::dbListTables(con)

  #Sélectionne les tables DIM
  tablesDim <- tables[which(stringr::str_detect(tables,"^dim_")==TRUE)]

  # Table de conversion des noms vers la taxonomie standard
  tableConversionNom <- data.frame(nameIn = tablesDim,
                                   nameOut = c("remove","prta","grds",
                                               "soms","dids","remove",
                                               "idcf","prlp","grms",
                                               "grmc","adme","adad",
                                               "prcp","adpp","grrs",
                                               "nnsn","shsh","shsi",
                                               "grss","prsm"))

  tableConversionNom <- dplyr::filter(tableConversionNom,nameOut != "remove")

  # Construction de la query d'import des tables

  query <- paste0("SELECT * FROM ",tableConversionNom$nameIn)

  #Import des tables

  tablesDimension <- setNames(lapply(1:length(query),
                                     function(n){
                                       DBI::dbGetQuery(con,query[n])
                                     }),
                              janitor::make_clean_names(paste0("dim",tableConversionNom$nameOut),"small_camel"))


  DBI::dbDisconnect(con)

  return(tablesDimension)

}
