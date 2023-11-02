# %%%%%%%%%%%%%%%%%%%%%%%
#
# Daten korrekt einlesen
#
# Beschreibung: 
# Die .csv-Datei "Datenbasis_V4.csv" wird als data.frame eingelesen. Dabei haben
# alle Spalten den Datentyp "character" um fehlerhaften Konvertierungen des Datentyps
# zuvor zukommen.
#
# %%%%%%%%%%%%%%%%%%%%%%%


data <- readr::read_csv(
   file = file.path("Daten", "Datenbasis_V4.csv")
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
