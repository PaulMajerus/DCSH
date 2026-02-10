#' Générateur de requêtes SQL pour extractions DCSH
#'
#' Cette fonction construit dynamiquement une ou plusieurs requêtes SQL selon des variables DCSH (taxonomie)
#' et les années concernées. Elle s’appuie sur la table interne `tableConstructionDB` pour identifier les bonnes
#' colonnes, versions, tables et jointures à inclure.
#'
#' Les requêtes générées peuvent ensuite être exécutées via la fonction \code{\link{loadData}} du package \code{DCSH}.
#'
#' @param var Character vector. Liste des noms de variables/taxonomies DCSH à extraire (ex: \code{c("DP", "DAS", "Age")})
#' @param annee Character vector. Liste des années concernées (ex: \code{c("2022", "2023")})
#' @param matriculeNat Bolean value. Faut-il inclure ou non la variable "matricule national".
#'
#' @return Une liste de chaînes de caractères, chacune correspondant à une requête SQL prête à exécuter.
#'
#' @details
#' La fonction :
#' \itemize{
#'   \item Identifie les versions de codage pertinentes selon les années spécifiées.
#'   \item Sélectionne les bonnes tables de base et d’attributs via \code{typeTable}.
#'   \item Génère dynamiquement la clause \code{SELECT} incluant les bons alias.
#'   \item Gère les jointures conditionnelles avec les tables secondaires.
#'   \item Restreint le résultat à l'année d’intérêt via la clause \code{WHERE}.
#' }
#'
#' Le champ \code{DocName} est systématiquement inclus dans les extractions.
#'
#' @seealso \code{\link{loadData}} pour exécuter les requêtes construites
#'
#' @export
#'
#' @examples
#' # Génère les requêtes pour extraire DP et DAS pour 2022 et 2023
#' req <- queryBuildR(var = c("adda", "sods"), annee = c("2022", "2023"))
#' cat(req[[1]])


queryBuildR <- function(var=character(),
                        annee=character(),
                        matriculeNat = FALSE){

  if(any(stringr::str_detect(var,"^sh|^sm")==TRUE &
     2022 %in% annee)){

    conversion <- data.frame(sh = c("shsi","shdd","shdf","shsh"),
                             sm = c("smce","smdd","smdf","smsm")) |>
      dplyr::filter(sh %in% var | sm %in% var)

    var <- unique(c(var,conversion$sh,conversion$sm,"idcf","sods","adna"))
  }


  # Détermine les versions de codage impliquées dans la query
  version <- tableConstructionDB |>
    dplyr::filter(taxonomie %in% var &
                    typeTable != "ERROR" &
                    stringr::str_detect(version,
                                        paste(annee,collapse="|"))) |>
    dplyr::pull(version) |>
    unique()

  # Détermine les types de tables concernées
  typeTable <- tableConstructionDB |>
    dplyr::filter(taxonomie %in% var &
                    typeTable != "ERROR" &
                    !(typeTable == "serviceLieux" & taxonomie == "adna") &
                    stringr::str_detect(version,
                                        paste(annee,collapse="|"))) |>
    #Eviter que service lieux apparaissent juste pour adna
    #dplyr::filter(!(typeTable == "serviceLieux" & taxonomie == "adna")) |>
    dplyr::pull(typeTable) |>
    unique()

  # Construction de la query SELECT
  selectQuery <- lapply(typeTable,
                        function(i){
                          lapply(annee,
                                 function(j){
                                   varASelectionner <- var
                                   # Choix de la table d'extraction du docname (SEJOUR VS Autre table)
                                   if(i == "sejour"){
                                     docnameCall <- paste0("[sejour].",
                                                           tableConstructionDB |>
                                                             dplyr::filter(stringr::str_detect(version,as.character(j)) &
                                                                             stringr::str_detect(table,"ClinicalDoc2") &
                                                                             stringr::str_to_lower(column)=="[docname]") |>
                                                             dplyr::pull(column),
                                                           " as DocName") |>
                                       unique()
                                   }else if(i =="serviceLieux" & j != 2024){
                                     docnameCall <- paste0("[sejour].",
                                                           tableConstructionDB |>
                                                             dplyr::filter(stringr::str_detect(version,as.character(j)) &
                                                                             stringr::str_detect(table,"ClinicalDoc2") &
                                                                             stringr::str_detect(taxonomie,"adna")) |>
                                                             dplyr::pull(column)|>
                                                             unique(),
                                                           " as adna")
                                   }else{

                                     docnameCall <- paste0(tableConstructionDB |>
                                                             dplyr::filter(typeTable == i &
                                                                             stringr::str_detect(version,as.character(j)) &
                                                                             stringr::str_to_lower(column)=="[docname]") |>
                                                             dplyr::pull(abrev),
                                                           ".",
                                                           tableConstructionDB |>
                                                             dplyr::filter(typeTable == i &
                                                                             stringr::str_detect(version,as.character(j)) &
                                                                             stringr::str_to_lower(column)=="[docname]") |>
                                                             dplyr::pull(column),
                                                           " as DocName")
                                   }
                                   if("adna" %in% var &
                                      i == "serviceLieux" & j != 2024){ varASelectionner <- var[-which(var=="adna")]}

                                   selectVar <- paste0(tableConstructionDB |>
                                                         dplyr::filter(taxonomie %in% varASelectionner &
                                                                         stringr::str_detect(version,as.character(j)) &
                                                                         typeTable == i) |>
                                                         dplyr::arrange(taxonomie) |>
                                                         dplyr::distinct(taxonomie,.keep_all=TRUE) |>
                                                         dplyr::pull(abrev) ,
                                                       '.',
                                                       tableConstructionDB |>
                                                         dplyr::filter(taxonomie %in% varASelectionner &
                                                                         stringr::str_detect(version,as.character(j)) &
                                                                         typeTable==i) |>
                                                         dplyr::arrange(taxonomie) |>
                                                         dplyr::distinct(taxonomie,.keep_all=TRUE) |>
                                                         dplyr::pull(column) ,
                                                       " as ",
                                                       tableConstructionDB |>
                                                         dplyr::filter(taxonomie %in% varASelectionner &
                                                                         stringr::str_detect(version,as.character(j)) &
                                                                         typeTable==i) |>
                                                         dplyr::arrange(taxonomie) |>
                                                         dplyr::distinct(taxonomie,.keep_all=TRUE) |>
                                                         dplyr::pull(taxonomie))

                                   selectVar <- selectVar[which(selectVar != ". as ")]

                                     selectQuery <- paste(unique(c(docnameCall,
                                                                 selectVar)),
                                                        collapse = ",")

                                     if(matriculeNat == TRUE){
                                       selectQuery <- paste0("p.mat_an as mana, ",selectQuery)
                                                                      }
                                   return(selectQuery)
                                 })
                        })

  # Construction de la query FROM
  fromQuery <- lapply(typeTable,
                      function(i){
                        lapply(annee,
                               function(j){
                                 # Choix de la table d'extraction du docname (SEJOUR VS Autre table)
                                 fromCall <- paste0(tableConstructionDB |>
                                                      dplyr::filter(stringr::str_detect(version,as.character(j)) &
                                                                      stringr::str_detect(table,"ClinicalDoc2")) |>
                                                      dplyr::pull(table) |>
                                                      unique(),
                                                    " sejour")
                               })
                      })

  # Construction de la query LEFT JOIN

    leftjoinQuery <- lapply(typeTable,
                            function(i){
                              lapply(annee,
                                     function(j){
                                       if(
                                         all(stringr::str_detect((tableConstructionDB |>
                                                         dplyr::filter(stringr::str_detect(version,as.character(j)) &
                                                                       typeTable == i &
                                                                       taxonomie %in% var) |>
                                                         dplyr::pull(table) |>
                                                         unique()),
                                                         "ClinicalDoc"))
                                       ){
                                         " "
                                       }else{
                                       paste(paste0("LEFT JOIN ",
                                                    tableConstructionDB |>
                                                      dplyr::filter(taxonomie %in% var &
                                                                      stringr::str_detect(version,as.character(j)) &
                                                                      typeTable==i &
                                                                      stringr::str_detect(table,"inicalDoc")==FALSE) |>
                                                      dplyr::pull(table) |>
                                                      unique(),
                                                    " ",
                                                    tableConstructionDB |>
                                                      dplyr::filter(taxonomie %in% var &
                                                                      stringr::str_detect(version,as.character(j)) &
                                                                      typeTable==i &
                                                                      stringr::str_detect(table,"inicalDoc")==FALSE) |>
                                                      dplyr::pull(abrev) |>
                                                      unique(),
                                                    " on ",
                                                    tableConstructionDB |>
                                                      dplyr::filter(taxonomie %in% var &
                                                                      stringr::str_detect(version,as.character(j)) &
                                                                      typeTable==i &
                                                                      stringr::str_detect(table,"inicalDoc")==FALSE) |>
                                                      dplyr::pull(abrev) |>
                                                      unique(),
                                                    ".",
                                                    tableConstructionDB |>
                                                      dplyr::filter(stringr::str_detect(version,as.character(j)) == TRUE &
                                                                      typeTable==i &
                                                                      stringr::str_detect(table,"inicalDoc")==FALSE) |>
                                                      dplyr::group_by(table) |>
                                                      dplyr::filter(any(taxonomie %in% var)) |>
                                                      dplyr::ungroup() |>
                                                      dplyr::filter(
                                                        if (i == "serviceLieux" & j != 2024) column == "[NO_SEJOUR]"
                                                        else stringr::str_to_lower(column) == "[docname]") |>
                                                      dplyr::pull(column),
                                                    if(i == "serviceLieux" & j != 2024) " = [sejour].[Numero_sejour]"
                                                    else " = [sejour].[docName]"),
                                             collapse=" ")
                                       }
                                     })
                            })




  whereQuery <- lapply(typeTable,
                       function(i){
                         lapply(annee,
                                function(j){
                                  where <- paste0("WHERE LEFT([sejour].[Fin_Admission],4) IN (",
                                         paste0("'",
                                                j,
                                                "'", collapse = ","),")")

                                  if(j == 2021 &
                                     i == "sejour" &
                                     "dids" %in% var) { where <- paste0(where," AND [diagn].[Type] = 'DP'")}
                                  return(where)
                                })
                       })

  if(matriculeNat == TRUE){
    mana <- "LEFT JOIN (
    SELECT [mat_an], [pama]
    FROM (
      SELECT [mat_an], [mat_incci_an], [mat_chdn_an], [mat_hrs_an], [mat_chem_an], [mat_chl_an], [mat_cfb_an]
      FROM [curative].[key_mapping_igss]
    ) AS src
    UNPIVOT (
      pama FOR idcf IN (
        mat_incci_an, mat_chdn_an, mat_hrs_an, mat_chem_an, mat_chl_an, mat_cfb_an
      )
    ) AS unpvt
  ) AS p
  ON p.pama = [sejour].[Matricule]"
    mana <- stringr::str_replace_all(mana,"\n","")
  }else{
    mana <- ""
  }


  query <- lapply(1:length(unlist(selectQuery)),
                  function(x){
                    paste("SELECT",unlist(selectQuery)[[x]],
                          "FROM",unlist(fromQuery)[[x]],
                          unlist(leftjoinQuery)[[x]],
                          mana,
                          unlist(whereQuery)[[x]])
                  })

  query <- query[!grepl("NonDisponible", query)]

  return(query)
}
