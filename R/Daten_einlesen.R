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


data <- read.csv(file = file.path("Daten", "Datenbasis_V4.csv"), colClasses = "character")