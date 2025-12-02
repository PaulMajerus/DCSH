utils::globalVariables(c(
  "table", "column", "version", "taxonomie", "restriction",
  "abrev", "typeTable"
))

# data-raw/build-tableConstructionDB.R

library(DBI)
library(odbc)
library(stringr)
library(dplyr)
library(usethis)

# Fonction utilitaire
liste_tables_et_colonnes <- function(con) {
  tables <- DBI::dbListTables(con)
  listeTables <- c("AdminInfo","ClinicalDoc2",
                   "Diag[12]20","EncounterInfo","Drg20","Mother2","Procedures2",
                   "Services20","PriseEnCharge20","DiagSejour2","ServiceMapping",
                   "ServicesLieux")
  tables <- stringr::str_subset(tables,
                       paste(listeTables, collapse = "|"))
  tables <- stringr::str_subset(tables,
                       "_delete|Union",
                       negate = TRUE)
  tables <- stringr::str_subset(tables,
                       "^v.{1,}2024$",
                       negate = TRUE)
  result <- lapply(tables, function(tbl) {
    colonnes <- DBI::dbListFields(con, tbl)
    data.frame(table = tbl,
               column = colonnes,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, result)
}

# --- Connexion base de données : enrobée dans tryCatch ---
safe_connect <- function() {
  tryCatch(
    DBI::dbConnect(odbc::odbc(), dsn = "dcshdb"),
    error = function(e) {
      message("⚠️ Impossible de se connecter à la base. Dataset non reconstruit.")
      NULL
    }
  )
}

con <- safe_connect()

if (!is.null(con)) {
  message("✅ Connexion réussie, construction du dataset...")

  tableConstructionDB <- liste_tables_et_colonnes(con) |>
    dplyr::mutate(version = stringr::str_sub(table, -4, -1),
           version = if_else(version == "2020",
                             "2018,2019,2020",
                             if_else(version == "2022",
                                     "2022,2023",
                                     as.character(version))),
           version = if_else(table=="ServiceMapping",
                             "2021,2022,2023",
                             version)) |>
    dplyr::mutate(taxonomie = dplyr::case_when(
      stringr::str_to_lower(column) == "docname" ~ "DocName",
      stringr::str_to_lower(column) == "nationalite" ~ "pana",
      stringr::str_to_lower(column) == "age_jours" ~ "nnaj",
      stringr::str_to_lower(column) == "assurabilite" ~ "paas",
      stringr::str_to_lower(column) == "matricule" ~ "pama",
      stringr::str_to_lower(column) == "code_postal" ~ "pacp",
      stringr::str_to_lower(column) == "pays" ~ "papr",
      stringr::str_to_lower(column) == "genre" ~ "pase",
      stringr::str_to_lower(column) == "date_naissance" ~ "padn",
      stringr::str_to_lower(column) == "code_fournisseur" ~ "idcf",
      stringr::str_to_lower(column) %in% c("numero_sejour",
                                  "no_sejour")~ "adna",
      stringr::str_to_lower(column) == "provenance" ~ "adpp",
      stringr::str_to_lower(column) == "debut_admission" ~ "adda",
      stringr::str_to_lower(column) == "fin_admission" ~ "sods",
      stringr::str_to_lower(column) == "destination" ~ "soms",
      stringr::str_to_lower(column) == "medecin_traitant" ~ "adcm",
      stringr::str_to_lower(column) == "etabl_provenance" ~ "adep",
      stringr::str_to_lower(column) == "etabl_destination" ~ "soed",
      stringr::str_to_lower(column) == "drg_admission" ~ "grda",
      stringr::str_to_lower(column) == "mdc_admission" ~ "grma",
      stringr::str_to_lower(column) == "rom_admission" ~ "grra",
      stringr::str_to_lower(column) == "soi_admission" ~ "grsa",
      stringr::str_to_lower(column) == "drg_group" ~ "grds",
      stringr::str_to_lower(column) == "mdc" ~ "grms",
      stringr::str_to_lower(column) == "soi" ~ "grss",
      stringr::str_to_lower(column) == "rom" ~ "grrs",
      stringr::str_to_lower(column) == "med_surg" ~ "grmc",
      stringr::str_to_lower(column) == "mode_admission" ~ "adma",
      stringr::str_to_lower(column) == "modalite_entree" ~ "adme",
      stringr::str_to_lower(column) == "mode_adressage" ~ "adad",
      stringr::str_to_lower(column) == "passage_urgence" ~ "adpu",
      stringr::str_to_lower(column) == "diagnostic_sejour" ~ "dids",
      stringr::str_to_lower(column) == "matricule_mere" ~ "nnmm",
      stringr::str_to_lower(column) == "seq_naissance" ~ "nnsn",
      stringr::str_to_lower(column) == "poids(g)" ~ "nnpn",
      stringr::str_to_lower(column) == "duree_gestation(wk)" ~ "nndg",
      stringr::str_to_lower(column) == "apgar1" ~ "nna1",
      stringr::str_to_lower(column) == "apgar5" ~ "nna5",
      stringr::str_to_lower(column) == "saturation1" ~ "nnc1",
      stringr::str_to_lower(column) == "saturation2" ~ "nnc2",
      stringr::str_to_lower(column) == "saturation3" ~ "nnc3",
      stringr::str_to_lower(column) == "present_admission" ~ "dipa",
      stringr::str_to_lower(column) == "date_procedure" ~ "prdp",
      stringr::str_to_lower(column) == "code_medecin_consultant" ~ "prcm",
      stringr::str_to_lower(column) == "lieu_procedure" ~ "prlp",
      stringr::str_to_lower(column) == "technique_anesthesie" ~ "prta",
      stringr::str_to_lower(column) == "code_procedure" ~"prcp",
      stringr::str_detect(table,"Procedures") &
        stringr::str_to_lower(column) == "specialite" ~ "prsm",
      stringr::str_detect(table,"Diag1202[124]") &
        stringr::str_to_lower(column) == "code" ~ "didp",
      stringr::str_detect(table,"Diag12020") &
        stringr::str_to_lower(column) == "code" ~ "dids",
      stringr::str_detect(table,"Diag22") &
        stringr::str_to_lower(column) == "code" ~ "dise",
      stringr::str_detect(table,"Services2") &
        stringr::str_to_lower(column) == "site_hospitalier" ~ "smsi",
      stringr::str_detect(table,"Services2") &
        stringr::str_to_lower(column) == "service" ~ "smsh",
      stringr::str_detect(table,"Services2") &
        stringr::str_to_lower(column) == "date_debut" ~ "smdd",
      stringr::str_detect(table,"Services2") &
        stringr::str_to_lower(column) == "date_fin" ~ "smdf",
      stringr::str_detect(table,"PriseEnCharge") &
        stringr::str_to_lower(column) == "date_debut" ~ "mtdd",
      stringr::str_detect(table,"PriseEnCharge") &
        stringr::str_to_lower(column) == "date_fin" ~ "mtdf",
      stringr::str_detect(table,"PriseEnCharge") &
        stringr::str_to_lower(column) == "specialite" ~ "mtsp",
      stringr::str_detect(table,"PriseEnCharge") &
        stringr::str_to_lower(column) == "degre_urgence" ~ "mtdu",
      stringr::str_detect(table,"DiagSejour") &
        version == "2021" &
        stringr::str_to_lower(column) == "code" ~ "dids",
      stringr::str_detect(table,"ServicesLieux") &
        stringr::str_to_lower(column) == "service" ~ "shsh",
      stringr::str_detect(table,"ServicesLieux") &
        stringr::str_to_lower(column) == "date_debut" ~ "shdd",
      stringr::str_detect(table,"ServicesLieux") &
        stringr::str_to_lower(column) == "date_fin" ~ "shdf",
      stringr::str_detect(table,"ServicesLieux") &
        stringr::str_to_lower(column) == "site_hospitalier" ~ "shsi",
      stringr::str_detect(table,"Mapping") &
        stringr::str_to_lower(column) == "sh" ~ "shsh",
      stringr::str_detect(table,"Mapping") &
        stringr::str_to_lower(column) == "entree" ~ "shdd",
      stringr::str_detect(table,"Mapping") &
        stringr::str_to_lower(column) == "sortie" ~ "shdf",
      stringr::str_detect(table,"Mapping") &
        stringr::str_to_lower(column) == "site" ~ "shsi",
    )) |>
    dplyr::mutate(restriction = if_else(taxonomie == "dids" &
                                   version == "2021",
                                 "WHERE diag.Type = 'DP'",
                                 NA)) |>
    dplyr::mutate(abrev = case_when(
      stringr::str_detect(table,"ClinicalDoc") ~ "[sejour]",
      stringr::str_detect(table,"Procedures") ~ "[proced]",
      stringr::str_detect(table,"Diag22") ~ "[diags]",
      stringr::str_detect(table,"Diag12020") ~ "[diag]",
      stringr::str_detect(table,"Diag1202[124]") ~ "[diagp]",
      stringr::str_detect(table,"AdminInfo") ~ "[admin]",
      stringr::str_detect(table,"vAdminInfo") ~ "[vadmin]",
      stringr::str_detect(table,"DiagSejour") ~ "[diagn]",
      stringr::str_detect(table,"ServiceMapping") ~ "[servm]",
      stringr::str_detect(table,"Services2") ~ "[sepc]",
      stringr::str_detect(table,"Mother") ~ "[mothe]",
      stringr::str_detect(table,"ServicesLieux") ~ "[servm]",
      stringr::str_detect(table,"Encounter") ~ "[encou]",
      stringr::str_detect(table,"PriseEnCharge") ~ "[medtr]",
      stringr::str_detect(table,"Drg") ~ "[groupage]",
      TRUE ~ "ERROR"
    )) |>
    dplyr::mutate(abrev = if_else(stringr::str_detect(table,"^v.{1,}2024$") == TRUE,
                           paste0("[","v",stringr::str_sub(abrev,2,-1)),
                           abrev)) |>
    dplyr::mutate(typeTable = case_when(
      stringr::str_detect(table,"ClinicalDoc") ~ "sejour",
      stringr::str_detect(table,"Procedures") ~ "procedure",
      stringr::str_detect(table,"Diag22") ~ "diagnosticSecondaire",
      stringr::str_detect(table,"Diag12020") ~ "sejour",
      stringr::str_detect(table,"Diag1202[124]") ~ "diagnosticPrincipal",
      stringr::str_detect(table,"AdminInfo") ~ "sejour",
      stringr::str_detect(table,"DiagSejour") ~ "sejour",
      stringr::str_detect(table,"ServiceMapping") ~ "serviceLieux",
      stringr::str_detect(table,"Services2") ~ "servicePriseEnCharge",
      stringr::str_detect(table,"Encounter") ~ "sejour",
      stringr::str_detect(table,"Mother") ~ "sejour",
      stringr::str_detect(table,"ServicesLieux") ~ "serviceLieux",
      stringr::str_detect(table,"PriseEnCharge") ~ "MedecinTraitant",
      stringr::str_detect(table,"Drg") ~ "sejour",
      TRUE ~ "ERROR"
    )) |>
    dplyr::as_tibble() |>
    dplyr::mutate(column = paste0("[",column,"]")) |>
    dplyr::bind_rows(data.frame(table = "NonDisponible",
                         column = "NonDisponible",
                         version = "2018,2019,2020",
                         taxonomie = "didp",
                         restriction = NA,
                         abrev = "NonDisponible",
                         typeTable = "diagnosticPrincipal"))

  usethis::use_data(tableConstructionDB, overwrite = TRUE)

  DBI::dbDisconnect(con)

} else {
  message("⚠️ Le script est ignoré (pas de connexion disponible).")
}

usethis::use_data(tableConstructionDB, overwrite = TRUE)
