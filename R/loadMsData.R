#' Build SQL queries for MS extraction
#'
#' Constructs a set of SQL queries used to extract medical stay data
#' (sejours, services, prises en charge, diagnostics secondaires)
#' for one or several years.
#'
#' @param annee Numeric vector of years (e.g. \code{2022}, \code{c(2021, 2022)}).
#'   These years are used to filter on \code{Fin_Admission}.
#'
#' @return A named list of SQL query strings with elements:
#'   \itemize{
#'     \item \code{sej}: stays (séjours)
#'     \item \code{sh}: services
#'     \item \code{sm}: prises en charge
#'     \item \code{ds}: diagnostics secondaires
#'   }
#'
#' @details
#' The function does not execute the queries. It only builds SQL strings
#' that can be passed to a database connector (e.g. DBI::dbGetQuery).
#'
#' The filtering on years is performed using:
#' \code{LEFT(Fin_Admission, 4) IN (...)}.
#'
#' @examples
#' loadMsData(2022)
#' loadMsData(c(2021, 2022, 2023))
#'
#' @export
loadMsData <- function(annee = numeric()) {
  ...
}

loadMsData <- function(annee = numeric()){

  query <- list(sej = paste0("SELECT [RowIndex] as idri
      ,[ParentKeyField] as idpk
      ,[Code_fournisseur] as idcf
      ,[Matricule] as pama
      ,[Sexe] as pase
      ,[Date_Naissance] as padn
      ,[Pays] as papr
      ,[Code_Postal] as pacp
      ,[Assurabilite] as paas
      ,[Numero_Sejour] as adna
      ,[Debut_Admission] as adda
      ,[Provenance] as adpp
      ,[Etabl_Provenance] as adep
      ,[Fin_Admission] as sods
      ,[Destination] as soms
      ,[Etabl_Destination] as soed
      ,[Diagnostic_Sejour] as dids
  FROM [DCSHDB].[curative].[vlisteSejours]
  WHERE left([Fin_Admission],4) IN ('",paste(annee,collapse = "','"),"')"),
                sh = paste0("SELECT b.[RowIndex] as shri
      ,b.[ParentKeyField] as shpk
      ,b.[Service] as shsh
      ,b.[Date_Debut] as shdd
      ,b.[Date_Fin] as shdf
      ,b.[Site] as shsi
  FROM [DCSHDB].[curative].[vlisteServices] b
  LEFT JOIN [DCSHDB].[curative].[vlisteSejours] c on c.[RowIndex] =  b.[ParentKeyField]
 WHERE left(c.[Fin_Admission],4) IN ('",paste(annee,collapse = "','"),"') AND b.[YEAR] = 2024"),
                sm = paste0("SELECT b.[RowIndex] as smri
      ,b.[ParentKeyField] as smpk
      ,b.[Date_Debut] as smdd
      ,b.[Date_Fin] as smdf
      ,b.[Specialite] as smsm
      ,b.[Code] as didp
      ,b.[Present_Admission] as dipa
  FROM [DCSHDB].[curative].[vlistePriseEnCharges] b
  LEFT JOIN [DCSHDB].[curative].[vlisteSejours] c on c.[RowIndex] =  b.[ParentKeyField]
 WHERE left(c.[Fin_Admission],4) IN ('",paste(annee,collapse = "','"),"') AND b.[YEAR] = 2024"),
                ds = paste0("SELECT b.[RowIndex] as diri
      ,b.[ParentKeyField] as dipk
      ,b.[Code] as dise
      ,b.[Present_Admission] as dipa
  FROM [DCSHDB].[curative].[vlisteDiagnosticSecondaires] b
  LEFT JOIN [DCSHDB].[curative].[vlisteSejours] c on c.[RowIndex] =  b.[ParentKeyField]
 WHERE left(c.[Fin_Admission],4) IN ('",paste(annee,collapse = "','"),"') AND b.[YEAR] = 2024"))

  return(query)

  }



