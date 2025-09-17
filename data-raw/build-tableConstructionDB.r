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
  tables <- dbListTables(con)
  listeTables <- c("AdminInfo","ClinicalDoc2",
                   "Diag[12]20","EncounterInfo","Drg20","Mother2","Procedures2",
                   "Services20","PriseEnCharge20","DiagSejour2","ServiceMapping",
                   "ServicesLieux")
  tables <- str_subset(tables,
                       paste(listeTables, collapse = "|"))
  tables <- str_subset(tables, "_delete|Union|v.{1,}2024", negate = TRUE)
  result <- lapply(tables, function(tbl) {
    colonnes <- dbListFields(con, tbl)
    data.frame(table = tbl, column = colonnes, stringsAsFactors = FALSE)
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
    mutate(version = str_sub(table, -4, -1),
           version = if_else(version == "2020",
                             "2018,2019,2020",
                             if_else(version == "2022",
                                     "2022,2023",
                                     as.character(version))),
           version = if_else(table=="ServiceMapping",
                             "2021,2022,2023",
                             version)) |>
    mutate(taxonomie = case_when(
      str_to_lower(column) == "docname" ~ "DocName",
      str_to_lower(column) == "nationalite" ~ "pana",
      str_to_lower(column) == "age_jours" ~ "nnaj",
      str_to_lower(column) == "assurabilite" ~ "paas",
      str_to_lower(column) == "matricule" ~ "pama",
      str_to_lower(column) == "code_postal" ~ "pacp",
      str_to_lower(column) == "pays" ~ "papr",
      str_to_lower(column) == "genre" ~ "pase",
      str_to_lower(column) == "date_naissance" ~ "padn",
      str_to_lower(column) == "code_fournisseur" ~ "idcf",
      str_to_lower(column) %in% c("numero_sejour",
                                  "no_sejour")~ "adna",
      str_to_lower(column) == "provenance" ~ "adpp",
      str_to_lower(column) == "debut_admission" ~ "adda",
      str_to_lower(column) == "fin_admission" ~ "sods",
      str_to_lower(column) == "destination" ~ "soms",
      str_to_lower(column) == "medecin_traitant" ~ "adcm",
      str_to_lower(column) == "etabl_provenance" ~ "adep",
      str_to_lower(column) == "etabl_destination" ~ "soed",
      str_to_lower(column) == "drg_admission" ~ "grda",
      str_to_lower(column) == "mdc_admission" ~ "grma",
      str_to_lower(column) == "rom_admission" ~ "grra",
      str_to_lower(column) == "soi_admission" ~ "grsa",
      str_to_lower(column) == "drg_group" ~ "grds",
      str_to_lower(column) == "mdc" ~ "grms",
      str_to_lower(column) == "soi" ~ "grss",
      str_to_lower(column) == "rom" ~ "grrs",
      str_to_lower(column) == "med_surg" ~ "grmc",
      str_to_lower(column) == "mode_admission" ~ "adma",
      str_to_lower(column) == "modalite_entree" ~ "adme",
      str_to_lower(column) == "mode_adressage" ~ "adad",
      str_to_lower(column) == "passage_urgence" ~ "adpu",
      str_to_lower(column) == "diagnostic_sejour" ~ "dids",
      str_to_lower(column) == "matricule_mere" ~ "nnmm",
      str_to_lower(column) == "seq_naissance" ~ "nnsn",
      str_to_lower(column) == "poids(g)" ~ "nnpn",
      str_to_lower(column) == "duree_gestation(wk)" ~ "nndg",
      str_to_lower(column) == "apgar1" ~ "nna1",
      str_to_lower(column) == "apgar5" ~ "nna5",
      str_to_lower(column) == "saturation1" ~ "nnc1",
      str_to_lower(column) == "saturation2" ~ "nnc2",
      str_to_lower(column) == "saturation3" ~ "nnc3",
      str_to_lower(column) == "present_admission" ~ "dipa",
      str_to_lower(column) == "date_procedure" ~ "prdp",
      str_to_lower(column) == "code_medecin_consultant" ~ "prcm",
      str_to_lower(column) == "lieu_procedure" ~ "prlp",
      str_to_lower(column) == "technique_anesthesie" ~ "prta",
      str_to_lower(column) == "code_procedure" ~"prcp",
      str_detect(table,"Procedures") &
        str_to_lower(column) == "specialite" ~ "prsm",
      str_detect(table,"Diag1202[12]") &
        str_to_lower(column) == "code" ~ "didp",
      str_detect(table,"Diag12020") &
        str_to_lower(column) == "code" ~ "dids",
      str_detect(table,"Diag22") &
        str_to_lower(column) == "code" ~ "dise",
      str_detect(table,"Services2") &
        str_to_lower(column) == "site_hospitalier" ~ "smsi",
      str_detect(table,"Services2") &
        str_to_lower(column) == "service" ~ "smsh",
      str_detect(table,"Services2") &
        str_to_lower(column) == "date_debut" ~ "smdd",
      str_detect(table,"Services2") &
        str_to_lower(column) == "date_fin" ~ "smdf",
      str_detect(table,"PriseEnCharge") &
        str_to_lower(column) == "date_debut" ~ "mtdd",
      str_detect(table,"PriseEnCharge") &
        str_to_lower(column) == "date_fin" ~ "mtdf",
      str_detect(table,"PriseEnCharge") &
        str_to_lower(column) == "specialite" ~ "mtsp",
      str_detect(table,"PriseEnCharge") &
      #   str_to_lower(column) == "degre_urgence" ~ "mtdu",
      # str_detect(table,"DiagSejour") &
      #   version == "2021" &
      #   str_to_lower(column) == "code" ~ "dids",
      str_detect(table,"ServicesLieux") &
        str_to_lower(column) == "service" ~ "shsh",
      str_detect(table,"ServicesLieux") &
        str_to_lower(column) == "date_debut" ~ "shdd",
      str_detect(table,"ServicesLieux") &
        str_to_lower(column) == "date_fin" ~ "shdf",
      str_detect(table,"ServicesLieux") &
        str_to_lower(column) == "site_hospitalier" ~ "shsi",
      str_detect(table,"Mapping") &
        str_to_lower(column) == "sh" ~ "shsh",
      str_detect(table,"Mapping") &
        str_to_lower(column) == "entree" ~ "shdd",
      str_detect(table,"Mapping") &
        str_to_lower(column) == "sortie" ~ "shdf",
      str_detect(table,"Mapping") &
        str_to_lower(column) == "site" ~ "shsi",
    )) |>
    mutate(restriction = if_else(taxonomie == "dids" &
                                   version == "2021",
                                 "WHERE diag.Type = 'DP'",
                                 NA)) |>
    mutate(abrev = case_when(
      str_detect(table,"ClinicalDoc") ~ "sejour",
      str_detect(table,"Procedures") ~ "proced",
      str_detect(table,"Diag22") ~ "diags",
      str_detect(table,"Diag12020") ~ "diag",
      str_detect(table,"Diag1202[12]") ~ "diagp",
      str_detect(table,"AdminInfo") ~ "admin",
      str_detect(table,"DiagSejour") ~ "diagn",
      str_detect(table,"ServiceMapping") ~ "servm",
      str_detect(table,"Services2") ~ "sepc",
      str_detect(table,"Mother") ~ "mothe",
      str_detect(table,"ServicesLieux") ~ "servm",
      str_detect(table,"Encounter") ~ "encou",
      str_detect(table,"PriseEnCharge") ~ "medtr",
      str_detect(table,"Drg") ~ "groupage",
      TRUE ~ "ERROR"
    )) |>
    mutate(typeTable = case_when(
      str_detect(table,"ClinicalDoc") ~ "sejour",
      str_detect(table,"Procedures") ~ "procedure",
      str_detect(table,"Diag22") ~ "diagnosticSecondaire",
      str_detect(table,"Diag12020") ~ "sejour",
      str_detect(table,"Diag1202[12]") ~ "diagnosticPrincipal",
      str_detect(table,"AdminInfo") ~ "sejour",
      str_detect(table,"DiagSejour") ~ "sejour",
      str_detect(table,"ServiceMapping") ~ "serviceLieux",
      str_detect(table,"Services2") ~ "servicePriseEnCharge",
      str_detect(table,"Encounter") ~ "sejour",
      str_detect(table,"Mother") ~ "sejour",
      str_detect(table,"ServicesLieux") ~ "serviceLieux",
      str_detect(table,"PriseEnCharge") ~ "MedecinTraitant",
      str_detect(table,"Drg") ~ "sejour",
      TRUE ~ "ERROR"
    )) |>
    as_tibble() |>
    bind_rows(data.frame(table = rep("vDiag12021",2),
                         column = c("Code","docName"),
                         version = rep("2021",2),
                         taxonomie = c("dids","DocName"),
                         restriction = rep(NA,2),
                         abrev = c("diagn","diagn"),
                         typeTable = c("sejour","sejour")))

  usethis::use_data(tableConstructionDB, overwrite = TRUE)

  DBI::dbDisconnect(con)

} else {
  message("⚠️ Le script est ignoré (pas de connexion disponible).")
}

usethis::use_data(tableConstructionDB, overwrite = TRUE)
