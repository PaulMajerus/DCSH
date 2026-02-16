#' Conversion des données HRS entre services-lieux (SH/SM) et services de prise en charge
#'
#' Cette fonction convertit, pour le périmètre HRS, les données issues des tables
#' de type \code{sh*} (services-lieux) et \code{sm*} (services de prise en charge)
#' pour le second semestre 2022, en échangeant leur structure de variables.
#'
#' La conversion est réalisée uniquement pour les enregistrements :
#' \itemize{
#'   \item dont l'année est 2022 (via la variable \code{sods}),
#'   \item appartenant à l'établissement HRS (\code{idcf == "70129077"}),
#'   \item et à partir du 1er juillet 2022.
#' }
#'
#' Concrètement :
#' \itemize{
#'   \item les lignes SH du second semestre 2022 sont transformées en SM,
#'   \item les lignes SM du second semestre 2022 sont transformées en SH,
#'   \item les autres périodes et établissements ne sont pas modifiés.
#' }
#'
#' La fonction repose sur une table de correspondance interne entre noms de colonnes
#' SH et SM (ex : \code{shsi <-> smce}, \code{shdd <-> smdd}, etc.).
#'
#' @param lst_df Une liste de data.frames contenant :
#' \itemize{
#'   \item au moins une table avec des colonnes commençant par \code{sh} ou \code{sm},
#'   \item au moins une table contenant les variables \code{adna}, \code{DocName},
#'         \code{sods} et \code{idcf}.
#' }
#'
#' @details
#' La fonction :
#' \enumerate{
#'   \item détecte automatiquement les tables SH, SM et la table contenant \code{sods},
#'   \item enrichit les tables SH/SM par jointure avec la table des dates,
#'   \item identifie les sous-ensembles SH 2022 et SM 2022,
#'   \item effectue la conversion de structure via renommage de colonnes,
#'   \item remplace les lignes correspondantes dans les tables d'origine,
#'   \item restaure strictement les colonnes initiales de chaque table.
#' }
#'
#' La fonction ne modifie pas :
#' \itemize{
#'   \item les années différentes de 2022,
#'   \item les établissements autres que HRS,
#'   \item les tables ne contenant pas de colonnes SH/SM.
#' }
#'
#' @return Une liste de data.frames de même longueur que \code{lst_df},
#' avec remplacement des données SH/SM pour le second semestre 2022 (HRS uniquement).
#'
#' @note
#' Cette fonction est fortement dépendante :
#' \itemize{
#'   \item des noms de colonnes (\code{sh*}, \code{sm*}, \code{sods}, \code{idcf}),
#'   \item du format de \code{sods} (doit être coercible en date),
#'   \item du code établissement HRS fixé en dur (\code{"70129077"}).
#' }
#'
#' Toute modification du schéma de données peut rendre la fonction incorrecte
#' sans générer d'erreur explicite.
#'
#' @seealso
#' \code{\link[dplyr]{rename}}, \code{\link[dplyr]{left_join}},
#' \code{\link[lubridate]{ymd}}
#'
#' @author
#' MAJERUS Paul
#'
#' @export
#'
#'


conversionHRS <- function(lst_df=list()){

  # Si on est dans le cas où on a des données SH ou SM et qu'on s'interesse à l'année 2022
  flags <- lapply(lst_df, function(df) {
    list(
      has_sh_sm = any(stringr::str_detect(names(df), "^(sh|sm)")),
      has_202223  = "sods" %in% names(df) &&
        any(stringr::str_sub(as.character(df$sods), 1, 4) %in% c("2022","2023"), na.rm = TRUE)
    )
  })

  has_any_sh_sm <- any(sapply(flags, `[[`, "has_sh_sm"))
  has_any_202223  <- any(sapply(flags, `[[`, "has_202223"))

  if (has_any_sh_sm && has_any_202223){
    # D'abord on déterminer l'indice des tables concernées par une modification et la table avec sods
    idx_22 <- which(
      sapply(lst_df, function(df) {
        "sods" %in% names(df) &&
          any(stringr::str_sub(as.character(df$sods), 1, 4) %in%  c("2022"), na.rm = TRUE)
      })
    )

    idx_23 <- which(
      sapply(lst_df, function(df) {
        "sods" %in% names(df) &&
          any(stringr::str_sub(as.character(df$sods), 1, 4) %in%  c("2023"), na.rm = TRUE)
      })
    )

    idx_sh <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sh"))
      })
    )

    idx_sm <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sm"))
      })
    )

    # Je sauve les colonnes initialement présentes
    colInitial <- lapply(lst_df,names)

    # Ensuite on repaire les tables sm et sh qui sont celles de 2022
    lst_df <- lapply(1:length(lst_df),function(y){
      if(y %in% c(idx_sh,idx_sm)){

        lst_df[[y]] <- lst_df[[y]] |>
          dplyr::left_join(lst_df[[idx_22]] |>
                      dplyr::select(adna,DocName,sods,idcf),
                    by=c(if("adna" %in% names(lst_df[[y]] )) "adna" else "DocName"))

        if(length(unique(lst_df[[y]]$idcf)) == 1){
          if(is.na(unique(lst_df[[y]]$idcf))==TRUE){

            lst_df[[y]] <-lst_df[[y]] |>
              dplyr::select(where(~ any(!is.na(.))))

            lst_df[[y]] <-lst_df[[y]] |>
              dplyr::left_join(lst_df[[idx_23]] |>
                                 dplyr::select(adna,DocName,sods,idcf),
                               by=c(if("adna" %in% names(lst_df[[y]])) "adna" else "DocName"))
          }
        }else{
          return(lst_df[[y]])
        }
      } else {
        return(lst_df[[y]])
      }
    })

    # On repaire SM 2022 et SH2022
    idx_sh2022 <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sh")) &
          all(stringr::str_sub(df$sods,1,4) == "2022")
      })
    )

    idx_sm2022 <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sm")) &
          all(stringr::str_sub(df$sods,1,4) == "2022")
      })
    )

    # On repaire SPC 2023 et SH 2023
    idx_sh2023 <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sh")) &
          all(stringr::str_sub(df$sods,1,4) == "2023")
      })
    )

    idx_sm2023 <- which(
      sapply(lst_df, function(df) {
        any(stringr::str_detect(names(df), "^sm")) &
          all(stringr::str_sub(df$sods,1,4) == "2023")
      })
    )

    # Table de conversion de nom de colonne
    conv<-data.frame(sh = c("shsi","shdd","shdf","shsh","adna","DocName","sods","idcf"),
                     sm = c("smce","smdd","smdf","smsm","adna","DocName","sods","idcf"))

    # On extrait le second trimestre HRS de ces deux tables
    sh20200Trim2 <- lst_df[[idx_sh2022]] |>
      dplyr::filter(lubridate::ymd(stringr::str_sub(sods,1,8)) >=  lubridate::ymd(stringr::str_sub("20220701",1,8)) &
                      idcf == "70129077")

    mapppingSh <- setNames(conv$sh,conv$sm)

    sh20200Trim2 <- sh20200Trim2 |>
      dplyr::rename(any_of(mapppingSh))

    sm20200Trim2 <- lst_df[[idx_sm2022]] |>
      dplyr::filter(lubridate::ymd(stringr::str_sub(sods,1,8)) >=  lubridate::ymd(stringr::str_sub("20220701",1,8)) &
                      idcf == "70129077")

    mapppingSm <- setNames(conv$sm,conv$sh)

    sm20200Trim2 <- sm20200Trim2 |>
      dplyr::rename(any_of(mapppingSm))

    # On les remplace dans les tables de bases 2022
    lst_df[[idx_sh2022]]<-lst_df[[idx_sh2022]] |>
      dplyr::filter((lubridate::ymd(stringr::str_sub(sods,1,8)) <  lubridate::ymd(stringr::str_sub("20220701",1,8)))|(idcf != "70129077")) |>
      dplyr::bind_rows(sm20200Trim2)

    lst_df[[idx_sm2022]]<-lst_df[[idx_sm2022]] |>
      dplyr::filter((lubridate::ymd(stringr::str_sub(sods,1,8)) <  lubridate::ymd(stringr::str_sub("20220701",1,8)))|(idcf != "70129077")) |>
      dplyr::bind_rows(sh20200Trim2)

    # On récupère smsm pour le HRS année 2023
    con <- DBI::dbConnect(odbc::odbc(), dsn = "dcshdb")

    query <- "SELECT [NO_SEJOUR] as adna, [SPC] as smsm FROM [DCSHDB].[curative].[SPCHRS2022]"
    sm2023 <- DBI::dbGetQuery(con,query) |>
      tibble::as_tibble() |>
      dplyr::rename(any_of(mapppingSh))
    DBI::dbDisconnect(con)

    # On récupère shsh pour le HRS année 2023
    sh2023 <- lst_df[[idx_sm2023]] |>
      dplyr::filter(idcf == "70129077")|>
      dplyr::rename(any_of(mapppingSm))

    # On remplace les shsh et smsm de l'année 2023
    lst_df[[idx_sh2023]] <- lst_df[[idx_sh2023]] |>
      filter(idcf != "70129077") |>
      bind_rows(sh2023)

    lst_df[[idx_sm2023]] <- lst_df[[idx_sm2023]] |>
      filter(idcf != "70129077") |>
      bind_rows(sm2023)

    # Je sélection les seules colonnes initialement présente
    lst_df<-lapply(1:length(lst_df),
           function(z){
             lst_df[[z]] |>
               dplyr::select(all_of(colInitial[[z]]))
           })

    return(lst_df)

  }
}
