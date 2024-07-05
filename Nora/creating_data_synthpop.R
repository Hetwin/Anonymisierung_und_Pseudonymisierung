# 1. Load required package
library(synthpop)

# 2. Load data
filepath <- "C:/Daten/DAV/AG_Anonymisierung/Datenbasis_V4.csv" # insert your path
data <- data <- readr::read_csv(
  file = "C:/Daten/DAV/AG_Anonymisierung/Datenbasis_V4.csv"
  ,col_types = readr::cols(
    "Identifikationsnummer" = readr::col_integer()
    ,"Religioese_Zugehoerigkeit" = readr::col_character()
    ,"Religion_allgemein" = readr::col_character()
    ,"Geburtsdatum" = readr::col_date()
    ,"Geburtsjahr" = readr::col_integer()
    ,"Geburtsjahr_Jahr_Monat" = readr::col_character()
    ,"Krankheit_KZ" = readr::col_integer()
    ,"Geschlecht" = readr::col_character()
    ,"Ethnizitaet" = readr::col_character()
    ,"Vorname" = readr::col_character()
    ,"Nachname" = readr::col_character()
    ,"Postleitzahl_5st" = readr::col_character()
    ,"Postleitzahl_2st" = readr::col_character()
    ,"Ort_5st" = readr::col_character()
    ,"Ort_2st" = readr::col_character()
    ,"Einwohner_5st" = readr::col_integer()
    ,"Einwohner_2st" = readr::col_integer()
    ,"Kaufkraft_pro_Einwohner_in_EUR_5st" = readr::col_double()
    ,"Kaufkraft_pro_Einwohner_in_EUR_2st" = readr::col_double()
    ,"Kaufkraft_pro_Haushalt_in_EUR_5st" = readr::col_double()
    ,"Kaufkraft_pro_Haushalt_in_EUR_2st" = readr::col_double()
    ,"Branche" = readr::col_character()
    ,"Beruf" = readr::col_character()
    ,"Gehalt_in_EUR" = readr::col_double()
    ,"Range_Gehalt_in_EUR_Ebene_0" = readr::col_character()
    ,"Range_Gehalt_in_EUR_Ebene_1" = readr::col_character()
    ,"Code" = readr::col_character()
    ,"Interval_Ebene_0" = readr::col_character()
    ,"Interval_Ebene_1" = readr::col_character()
    ,"Icd10_code" = readr::col_character()
    ,"Kapitel_Ebene_0" = readr::col_character()
    ,"Titel_Ebene_0" = readr::col_character()
    ,"Titel_Ebene_1" = readr::col_character()
    ,"Titel_Ebene_2" = readr::col_character()
  )
)

data <- as.data.frame(data)

# 3. create synthetic data
my_seed <- 123
mysyn_cart <- syn(data, seed = my_seed, method = "cart")
mysyn_parametric <- syn(data, seed = my_seed, method = "parametric")


# 4. save data for further analysis
synthetic_data <- list()





