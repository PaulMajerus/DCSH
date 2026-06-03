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
#' @import tibble
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
  tableCodeDiag <- c("dim_apr_drg",
                     "dim_diagnostics",
                     "dim_mdc",
                     "dim_procedures")

  # Table de conversion des noms vers la taxonomie standard
  tableConversionNom <- data.frame(nameIn = tableCodeDiag,
                                   nameOut = c("grds","dids","grms",
                                               "prcp"))

  # Construction de la query d'import des tables

  query <- paste0("SELECT * FROM ",tableConversionNom$nameIn)

  #Import des tables

  tablesDimension <- setNames(lapply(1:length(query),
                                     function(n){
                                       DBI::dbGetQuery(con,query[n]) |>
                                         tibble::as_tibble() |>
                                         dplyr::mutate(Dimension = tableConversionNom$nameOut[n])
                                     }),
                              janitor::make_clean_names(paste0("dim ",tableConversionNom$nameOut),"small_camel"))


  tablesDimensionGroupees <- DBI::dbGetQuery(con,"SELECT * FROM curative.dim_2025") |>
    tibble::as_tibble() |>
    dplyr::mutate(Dimension = dplyr::case_when(
      Dimension == "admission" ~ "adma",
      Dimension == "anesthesie" ~ "prta",
      Dimension == "assurabilite" ~ "paas",
      Dimension == "destination" ~ "soms",
      Dimension == "etablissement" ~ "idcf",
      Dimension == "lieu_procedure" ~ "prlp",
      Dimension == "med_surg" ~ "grmc",
      Dimension == "modalite_entree" ~ "adme",
      Dimension == "mode_adressage" ~ "adad",
      Dimension == "nationalite" ~ "pana",
      Dimension == "passage_urgence" ~ "adpu",
      Dimension == "presence_admission" ~ "dipa",
      Dimension == "provenance" ~ "adpp",
      Dimension == "rom" ~ "grrs",
      Dimension == "seqnaissance" ~ "nnsn",
      Dimension == "service" ~ "shsh",
      Dimension == "site" ~ "shsi",
      Dimension == "soi" ~ "grss",
      Dimension == "specialite" ~ "prsm",
    )) |>
    dplyr::group_by(Dimension) |>
    dplyr::group_split() |>
    (\(x) {
      names(x) <- janitor::make_clean_names(
        paste0("dim_", sapply(x, \(df) unique(df$Dimension))),
        case = "small_camel"
      )
      x
    })()


  tablesDimension <- c(tablesDimensionGroupees,tablesDimension)


  DBI::dbDisconnect(con)

  return(tablesDimension)
}
