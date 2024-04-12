# %%%%%%%%%%%%%%%%%%%%%%%

modified
# Beschreibung: 
# Hier werden die Daten eingelesen und die verschiedenen Modelle und Methoden getestet.
# Dabei wird bei jeder Modelltestung ein neues sdcMicro Objekt erzeugt; also geht man nach jedem Modell wieder zu der Ausgangsdatensituation zurÜck.
# Somit dient dieses Skript als schnelle MethodenÜbersicht. Eine eigene Case Study zur Anonymisierung muss extra vorgenommen werden. 
#
# %%%%%%%%%%%%%%%%%%%%%%%

#' @----------------------------Einrichten_&_Installieren_des_sdcMicro_Pakets----------------------------
# Gebe die aktuelle R Version aus
version <- R.version$version.string    
print(version)                                      # Hier: R version 4.3.2 (2023-10-31 ucrt)

# PrÜfe den Library-Pfad
.libPaths()                                         # Hier: "C:/Users/User/AppData/Local/R/win-library/4.3"

# Methode, um sdcMicro direkt aus GitHub zu installieren
library(devtools)
library(sdcMicro)

#' @-----------------------------------------Benötigte_Pakete_laden----------------------------------------
# devtools::install_github("r-lib/conflicted")
# library(conflicted)                              # Falls es Konflikte gibt, kann man damit diese umgehen 
library(sdcMicro)
library(ggplot2)
library(plyr)
library(ggpubr)
library(tidyverse)
library(lubridate)

#' @---------------------------------------------Daten_einlesen--------------------------------------------
source("R/Daten_einlesen.R")
ncol(data)
nrow(data)
str(data)                                          # Untereinanderauflistung der Spaltennamen + Datentypen --> names(data) fÜr nur Spaltennamen
head(data)                                         # Gebe die ersten 6 Zeilen aus  
sum(is.na(data))                                   # Anzahl leerer Einträge im df
colSums(is.na(data))                               # Anzahl leerer Einträge fÜr jede Spalte
#SchnellÜbersicht Über die numerischen Daten
numeric_cols <- sapply(data, function(x) !is.character(x))    # Bestimme alle Spalten, die nicht vom Datentyp "character" sind 
summary(data[numeric_cols])                                   # Boxplot-Daten von allen numerischen Spalten                                   


#' @------------------------------------------Variablen_einordnen------------------------------------------
# diskriminierende Variablen                      # Sollten aus dem Datensatz entfernt werden
data$Religioese_Zugehoerigkeit  
data$Religion_allgemein                           # Allgemeinere Ebene von Religioese_Zugehoerigkeit
data$Ethnizitaet

# direkte Identifkatoren
data$Vorname
data$Nachname
data$Identifikationsnummer                        # Hier wird diese als Variable als Vertragsnummer interpretiert

# sensitive Variablen
# 1) Krankheit: "Kein Eintrag" wird als nicht krank angesehen und nicht als Information nicht vorhanden
data$Krankheit_KZ                                 # 1: Person krank, 0: Person ist nicht krank
data$Icd10_code                                   # Krankheitscode          --> z.B. R19.80
data$Code                                         # Abgeschnitten           --> R19
data$Interval_Ebene_1                             # Einstufung in Intervall --> R10-R19
data$Interval_Ebene_0                             # Größeres Intervall      --> R00-R99      
data$Kapitel_Ebene_0                              # Römische Zahlen         --> Gleich, wenn Einordnung in Interval_Ebene_0 gleich ist
data$Titel_Ebene_0                                # Krankheitsbeschreibung  --> Gleich, wenn Einordnung in Interval_Ebene_0 gleich ist
data$Titel_Ebene_1                                # Titel_Ebene_0 spezifischer 
data$Titel_Ebene_2                                # Titel_Ebene_1 spezifischer

# 2) Gehalt
data$Gehalt_in_EUR
data$Range_Gehalt_in_EUR_Ebene_1                  # Intervalleinordnung 2300 -> [2290,2570)
data$Range_Gehalt_in_EUR_Ebene_0                  # Allgemeinere Einordnung  -> 2300 -> [2000, 2500)


# kategorielle Variablen
data$Geschlecht                                   # m/w
data$Postleitzahl_5st                             # PLZ Leitbereich                  --> z.B. 56170 (Bendorf)
data$Postleitzahl_2st                             # PLZ Leitregion                   --> 56 (Koblenz)
data$Ort_5st                                      # Zum Leitbereich gehörender Ort   --> Bendorf
data$Ort_5st                                      # Leitregion des Ortes             --> Region Koblenz
data$Beruf                                        # z.B. Maurer
data$Branche                                      # Allgemeiner als Beruf: Mauerer --> Handwerk

# stetige Variablen
data$Geburtsdatum
data$Geburtsjahr                                  # Allgemeinere Ebene zu Geburtsdatum 
data$Geburtsjahr_Jahr_Monat                       # Allgemeinere Ebene zu Geburtsdatum
data$Alter <- interval(as.Date(data$Geburtsdatum), Sys.Date())%/%years(1)
data$Einwohner_5st                                # Einwohnerzahl Leitbereich
data$Einwohner_2st                                # Einwohnerzahl Leitregion
data$Kaufkraft_pro_Einwohner_in_EUR_5st           # Kaufkraft/Einwohner Leitbereich
data$Kaufkraft_pro_Einwohner_in_EUR_2st           # Kaufkraft/Einwohner Leitregion
data$Kaufkraft_pro_Haushalt_in_EUR_5st            # Kaufkraft/Haushalt Leitbereich
data$Kaufkraft_pro_Haushalt_in_EUR_2st            # Kaufkraft/Haushalt Leitregion

# Sofern durch mehrere Variablen dasselbe beschrieben wird, sollte nur die Variable der höchsten Ebene fÜr die Anonymisierung betrachtet werden 
# --> z.B. "Icd-Code" zu "Interval_Ebene_0" soll durch Recoding erreicht werden. Das Gleiche gilt fÜr Beruf --> Branche

# Eine gute Vorgehensweise fÜr kategorielle Variablen ist, mit Recoding die Werte in Übergruppen einzuteilen, um weniger "unique"-Werte zu haben. Anschließend kann man auf die 
# allgemeineren Daten gut eine Local Suppression anwenden, um eine selbst definierte k-Anonymität (z.B. bei so wenigen Beobachtungen 3 oder 5) zu erreichen.


#' @------------------------------------------Plots:DatenÜbersicht------------------------------------------
# Plots zur Person
histGeschl <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Geschlecht), fill=c("blue3", "dodgerblue")) + xlab("Geschlecht") + ylab("Anzahl")
histAlter <- ggplot(data = data) + geom_bar(mapping = aes(x = cut(data$Alter, breaks = 10)), fill=1:10) + xlab("Alter") + ylab("Anzahl")
histReli <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Religion_allgemein), fill=c("blue3", "dodgerblue", "cyan3", "aquamarine")) + xlab("Religion") + ylab("Anzahl")
histEthni <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Ethnizitaet), fill=c("blue3", "dodgerblue", "cyan3", "aquamarine", "blueviolet", "azure")) + xlab("Ethnizitaet") + ylab("Anzahl")

ggarrange(histGeschl, histAlter, histReli,histEthni, ncol = 2, nrow = 2)

# Plots zum Ort
histPost <- ggplot(data = data) + geom_bar(mapping = aes(x = str_sub(data$Postleitzahl_5st, 1, 1)), fill = 1:10) + xlab("Postleitzahl") + ylab("Anzahl")
histEinw <- ggplot(data = data) + geom_bar(mapping = aes(x = cut(data$Einwohner_5st, breaks = 5)), fill=c("blue3", "dodgerblue", "cyan3", "aquamarine", "blueviolet")) + xlab("Einwohner") + ylab("Anzahl")
histKauf <- ggplot(data = data) + geom_bar(mapping = aes(x = cut(data$Kaufkraft_pro_Einwohner_in_EUR_5st, breaks = 5)), fill=c("blue3", "dodgerblue", "cyan3", "aquamarine", "blueviolet")) + xlab("Kaufkraft") + ylab("Anzahl")
densKauf <- ggplot(data = data, aes(x = data$Kaufkraft_pro_Einwohner_in_EUR_5st)) + geom_density()+ xlab("Kaufkraft") + ylab("Häufigkeit")

ggarrange(histPost, histEinw, histKauf, densKauf)

# Plots zur Krankheit
histKrank <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Krankheit_KZ), fill=c("blue3", "dodgerblue")) + xlab("Krank") + ylab("Anzahl")
histIcd10 <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Icd10_code), fill=1:20) + xlab("Icd10-Code") + ylab("Anzahl")

ggarrange(histKrank, histIcd10)


#' @------------------------------------------Auswahl_der_Variablen------------------------------------------
# Kategorische Variablen
data$Geschlecht <- factor(data$Geschlecht)
data$Postleitzahl_5st <- factor(data$Postleitzahl_5st)
data$Ort_5st <- factor(data$Ort_5st)
data$Vorname <- factor(data$Vorname)
data$Nachname <- factor(data$Nachname)
data$Beruf <- factor(data$Beruf)
data$Krankheit_KZ <- factor(data$Krankheit_KZ)

KeyVars <- c('Geschlecht', 'Postleitzahl_5st', 'Ort_5st', 'Beruf', 'Krankheit_KZ', 'Gehalt_in_EUR') 
# Gehalt sollte eigentlich nur eine sensitive Variable sein. In diesem Fall ist aber Gehalt auch hier enthalten, da das Gehalt zur Methodendemonstration in Intervalle gerecodet werden soll.
# Grund dafÜr ist, dass man keine sensitiven Variablen recoden kann. Bei einer Case Study sollte man das beachten, dass sensitive Variablen auch nur in SensVars enthalten sind.

# Stetige Variablen
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Kaufkraft_pro_Haushalt_in_EUR_5st', 'Einwohner_5st', 'Alter')
# Das Alter kann man aus dem Geburtsdatum direkt berechnen wie oben mit: data$Alter <- interval(as.Date(data$Geburtsdatum), Sys.Date())%/%years(1)
# Dabei hat Sys.Date() als return das heutige Datum. Somit ist man nicht auf das aktuelle Jahr angewiesen.

# ID-Variable
IdVars <- c('Identifikationsnummer')

# Sensitive Variablen
data$Icd10_code <- substr(data$Icd10_code, 1, 1)                  # Hier wird nur der Buchstabe fÜr die Kapiteleinteilung des Icd10-codes wird betrachtet: R19.80 --> R
SensVars <- c('Gehalt_in_EUR', 'Icd10_code')

#' @------------------------------------------sdc_Micro_Objekt------------------------------------------
sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, numVar= NumVars,hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')

# Sample frequency of individuals key
freq(sdcInitial, type = 'fk')

# Population frequency of individuals key
freq(sdcInitial, type = 'Fk')

# initial disclosure risk
sdcInitial@risk$individual

#' @----------------------------------------------Metriken----------------------------------------------
# k-anonymity
print(sdcInitial, 'kAnon')
sum(sdcInitial@risk$individual[,2] < 3)

# l-diversity
sdcInitial <- ldiversity(obj = sdcInitial, ldiv_index = c("Gehalt_in_EUR"), l_recurs_c = 2, missing = NA)       # wird in diesem Bsp. Fehler werfen, da Gehalt - wie oben beschrieben - nicht nur in SensVars steht
sdcInitial@risk$ldiversity
sdcInitial@risk$ldiversity[,'Gehalt_in_EUR_Distinct_Ldiversity']

# SUDA
sdcInitial <- suda2(obj = sdcInitial, missing = NA)
sdcInitial@risk$suda2$score
sdcInitial@risk$suda2

# plot density of DIS-SUDA scores
density <- density(sdcInitial@risk$suda2$disScore)
plot(density, main = 'Density plot of DIS-SUDA scores')

#' @----------------------------------------------Methoden----------------------------------------------
#' @1)Recoding
#' @1.1)Global_Recoding

# Recode Beruf (kategorische Variable): Das, was in der Excel Tabelle durch Branche beschrieben wird, soll im Folgenden mit Recoding erreicht werden
# Anzahl Beruf vor Recoding
table(sdcInitial@manipKeyVars$Beruf)
berufBefore <- ggplot(data = data) + geom_bar(mapping = aes(x = data$Beruf), fill=1:35) + xlab("Beruf") + ylab("Anzahl")

sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Schulleiter", "Grundschul-Lehrer"), after = "oeffentlicher Dienst")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Architekt", "Coach", "Personalberater", "Sekretär"), after = "Dienstleistungen & Freiberufe")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Sozialarbeiter", "Gerichtsmediziner", "Hebamme", "Sozialassistent", "Pharmareferent", "Med. Fachangestellter","Psychiater","Krankenschwester", "Arzt"), after = "Gesundheit & Soziales")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Industriekaufleute", "Juwelier", "Logistiker"), after = "Handel, Vertrieb & Logistik")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Maurer", "Zimmerleute", "Dachdecker", "Bäcker", "Friseur", "Elektriker", "Fliesenleger"), after = "Handwerk")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Wirtschafts-Informatiker", "Industrie-Mechaniker", "Lokführer", "Kfz-Mechatroniker", "Zerspanungs-Mechaniker", "Ingenieure"), after = "Ingenieurwesen, IT & Technik")
sdcInitial  <-  groupAndRename(obj = sdcInitial, var = c("Beruf"), before = c("Unternehmensberater", "Bankkaufleute", "Prokurist", "Investmentbanker"), after = "Management, Finanzen & Beratung")

#Anzahl Beruf nach Recoding
table(sdcInitial@manipKeyVars$Beruf)
berufAfter <- ggplot(data = data) + geom_bar(mapping = aes(x = sdcInitial@manipKeyVars$Beruf), fill=1:7) + xlab("Beruf") + ylab("Anzahl")
ggarrange(berufBefore, berufAfter)

# Recode Gehalt (numerische Variable): Das Gehalt soll in 15 Intervalle eingeteilt werden. Die Intervalle haben die Länge 500
par(mfrow = c(1, 2))
hist(data$Gehalt_in_EUR, main = "Gehalt vor Recoding", xlab = "Gehalt", ylab= "Anzahl", breaks=100)

sdcInitialRec <- globalRecode(sdcInitial, column = "Gehalt_in_EUR", breaks = 500 * c(0:15))

table(sdcInitialRec@manipKeyVars$Gehalt_in_EUR)
barplot(table(sdcInitialRec@manipKeyVars$Gehalt_in_EUR), main = "gehalt nach Recoding", xlab = "Gehaltintervalle", ylab = "Anzahl")

# Numerische Variablen: Recode PLZ mit der Funktion globalRecode 
# Hinweis: Die Intervalle sind als links-offen implementiert. Mögliche Workarounds (https://urldefense.com/v3/__https://sdcpractice.readthedocs.io/en/latest/anon_methods.html__;!!OsrnOkA!xnjR2e1o8Uu7bbLqWF-yADhQVOCVLEGf9ECcfL8Rbx_fKYlkSdYA36LoEC0s0tcCbJPxMl6yQO7i83O6JO3C70OMJu1aihsw$ 
#     - Anpassung bsp. durch Wahl der Intervallgrenzen marginal vor linker Grenze, z.B. statt "10" "9.9")
#     - Alternativ kann auch der zu Grunde liegende Code angepasst werden


#' @1.2)Top&Bottom_Coding
# Top coding bei Alter 60
summary(data$Alter)
sdcInitialRec <- topBotCoding(obj = sdcInitialRec, value = 60, replacement = 60, kind = 'top', column = 'Alter')

# Bottom coding bei Alter 15
sdcInitialRec <- topBotCoding(obj = sdcInitialRec, value = 15, replacement = 15, kind = 'bottom', column = 'Alter')
summary(sdcInitialRec@manipNumVars$Alter)

# Rundung
# Bsp:Kaufkraft auf 100 runden

#' @2)Suppression______________________________________________________________________________________
#' @2.1)Local_Suppression_ohne_Ghost_Variablen
# Hinweis:  Damit die Methoden eine Lösung finden und um die Rechenzeit zu reduzieren,
#           empfiehlt es sich im Vorfeld die Anzahl der Quasi-Identifikatoren und der Anzahl der Kategorien zu reduzieren.
#           Wie oben beschrieben ist es dafÜr eine gute Vorgehensweise, zunächst ein Recoding durchzufÜhren

### Local Suppression ohne "Importance Vector"
sdcInitialSup <- localSuppression(sdcInitialRec, k = 5)
print(sdcInitialSup, 'ls')

# Supression RÜckgängig machen
sdcInitialSup <- undolast(sdcInitialSup)

#### Local Suppression mit "Importance Vector" 
# Angabe der Variablen, fÜr die eine Variation fÜr den Datennutzen wichtig ist. Wenn der Wert sehr hoch ist, wird die Variable nur unterdrÜckt, wenn es keine andere Möglichkeit gibt
# Der Importance Vektor entählt die Priorität der zu unterdrÜckenden Variablen (d.h. er enthält eine Zahl zwischen 1 und derAnzahl der Key Variablen)
# Wahl von 5-Anonymität als Ziel
sdcInitialSup <- localSuppression(sdcInitialRec, importance = c(1, 1, 5, 4, 1, 1), k = 5)
print(sdcInitialSup, 'ls')

sdcInitialSup <- undolast(sdcInitialSup)

#' @2.2)Local_Suppression_m_approch
# Anmwendung von Local Suppression auf alle möglichen Teilbestände der Größe m
# Nutzung der Eigenschaft "combs"

# Bsp.: 5-Anonymität fÜr alle Teilbestände mit drei Keyvariablen und anschließend fÜr den ganzen Datensatz
# Um den ganzen Datenbestand zu wählen, wird der zweite Eintrag in "combs" auf die Anzahl der Keyvariablen gesetzt
sdcInitialSup <- localSuppression(sdcInitialRec, k = 5, combs = c(3, 6))
print(sdcInitialSup, 'ls')
sdcInitialSup <- undolast(sdcInitialSup)

# Bsp.: 3-Anonymität auf den ganzen Datensatz und 5-Anonymität fÜr Teilbestände mit zwei Keyvariablen
sdcInitialSup <- localSuppression(sdcInitialRec, k = c(2, 5), combs = c(6, 3))
print(sdcInitialSup, 'ls')
sdcInitialSup <- undolast(sdcInitialSup)

# manuelle Suppression --> Könnte man sich Überlegen, ob man das machen will, also: Wenn Voraussetzung fÜr Beobachtung erfÜllt, setze Wert auf NA

#' @2.3)Local_Suppression_mit_Ghost_Variablen
# Mit einander verbundene Variablen können durch Spezifikation der "Ghost"-Variablen angegeben werden
# Ghost (verbundene) Variablen werden durch eine Liste spezifiziert und als "linked" angesehen
# z.B. Wenn man weiß, in welchem Ort jemand wohnt, weiß man automatisch auch in welcher Region diese Person lebt

ghostVars <- list()
# Jede Verbindung wird wiederrum durch eine Liste abgebildet, deren erstes Element die Keyvariable ist 
# das zweite Element ist die verbundene Variable
# Suppression auf die die Keyvariable hat auch eine Suppression auf die Ghost Variable zur Folge 
ghostVars[[1]] <- list()
ghostVars[[1]][[1]] <- "Ort_5st"
ghostVars[[1]][[2]] <- c("Ort_2st")

## Erstelle neues sdcMicro Objekt
sdcInitial2 <- createSdcObj(dat= data, keyVars = KeyVars, numVars = NumVars, weightVar = 'Gehalt_in_EUR', ghostVars = ghostVars)

# Ausgabe der manipulierten Ghost Variablen
sdcInitial2@manipGhostVars

# Alternative Funktion fÜr Local Suppression, die die Angabe eines Schwellenwertes erlaubt: localSupp()
# --> Statt gewÜnschte k-Anonymität vorzugeben, gibt man Schwellenwert fÜr das individuelles Risiko an.
summary(sdcInitial2@risk$individual[,1])

# Anzahl an Datensätzen mit einem individuellen Risiko größer als 0.003
sum(sdcInitial@risk$individual[,1] > 0.003)

# Local Suppression
sdcInitial2 <- localSupp(sdcInitial2, threshold = 0.003, keyVar = 'Postleitzahl_5st')

sdcInitial2
print(sdcInitial2)


#' @3)PRAM_(Post_RAndomization_Method)_______________________________________________________________________
# Zufällige Ãnderung der Variablenwerte. Dabei gibt eine n x n Transformationsmatrix (Bei PRAM mit n Variablen) die Wahrscheinlichkeiten vor, welchen Wert eine Variable aus den n Werten annimmt.
# Zeile i der Matrix enthält dabei die Ãnderungswahrscheinlichkeiten der Zeile i. Die Zeilensumme ist somit fÜr jede Zeile 1.
# Wenn zusätzlich die Spaltensummen alle 1 sind, verändern sich (im Idealfall) nicht die Anzahl der verschiedenen Werte fÜr eine Variable, sondern nur deren Position. 

# Anwendung von PRAM auf alle Variablen
set.seed(123)
names(data)
data$Ort_2st <- factor(data$Ort_2st)
data$Branche <- factor(data$Branche)
PramVars <- c('Branche', 'Ort_2st') 
sdcPRAM <- createSdcObj(dat= data, keyVars = KeyVars, pramVars = PramVars,numVar= NumVars, hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')

# Hinweis: Es nicht möglich die Ãbergangsmatrix vorzugeben
# "variables" gibt die Variablen an, auf die PRAM angewendet werden soll (mÜssen nicht davor als PRAM-Variablen definiert werden) 
# Mit "pd" ist es möglich die Minima fÜr die "Diagonalen Einträge" anzugeben, diese geben die Wahrscheinlichkeiten an, dass ein bestimmter Wert sich nach der Anwendung von PRAM nicht ändert.
# Durch Angabe von "strata_variables" können unwahrscheinliche Kombinationen ausgeschlossen werden 

sdcPRAM <- pram(obj = sdcPRAM, variables = c('Branche', 'Ort_2st'), strata_variables = c('Gehalt_in_EUR'), pd = c(0.9, 0.5))
sdcPRAM

#' @4)Microaggregation______________________________________________________________________________________
# Die ausgewählte Spalte wird in gleich große Gruppen aufgeteilt. Die Größe gibt man mit "aggr" an.
# Innerhalb einer Gruppe werden alle Werte durch den Mean-Wert ersetzt
# Statt Mean ist auch der Median als Konfigurationsmöglichkeit in microaggregation() enthalten
par(mfrow = c(1, 2))
hist(data$Alter, main = "Alter vor Microaggregation", xlab = "Alter", ylab= "Anzahl", breaks=20)
summary(data$Alter)

sdcMicroAgg <- createSdcObj(dat= data, keyVars = KeyVars, numVar= NumVars, hhId = IdVars, sensibleVar = SensVars)
sdcMicroAgg <- microaggregation(obj = sdcMicroAgg, variables = 'Alter', aggr = 5, method = "mdav", measure = "mean")

hist(sdcMicroAgg@manipNumVars$Alter, main = "Alter nach Microaggregation", xlab = "Alter", ylab= "Anzahl", breaks=20)
summary(sdcMicroAgg@manipNumVars$Alter)


#' @5)Noise_Addition_______________________________________________________________________________________
set.seed(123)

KeyVars <- c('Geschlecht', 'Postleitzahl_5st', 'Ort_5st', 'Beruf', 'Krankheit_KZ')
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Kaufkraft_pro_Haushalt_in_EUR_5st', 'Einwohner_5st', 'Alter', 'Gehalt_in_EUR')

sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, pramVars = PramVars,numVar= NumVars, hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')

# Unkorreliert
sdcNoise <- addNoise(obj = sdcInitial,variables = c('Gehalt_in_EUR'),noise = 100, method = "additive")

summary(data$Gehalt_in_EUR)
hist(data$Gehalt_in_EUR, col = 4,main = "Gehalt vor Noise Addition",xlab = "Gehalt in Euro", ylab = "Anzahl")

summary(sdcNoise@manipNumVars$Gehalt_in_EUR)
hist(sdcNoise@manipNumVars$Gehalt_in_EUR, col = 4,main = "Gehalt nach Noise Addition",xlab = "Gehalt in Euro", ylab = "Anzahl")

qqnorm(sdcNoise@manipNumVars$Gehalt_in_EUR)
qqline(sdcNoise@origData$Gehalt_in_EUR)
sdcNoise@manipNumVars$Gehalt_in_EUR - sdcNoise@origData$Gehalt_in_EUR

ggplot() + 
  stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
  stat_qq(aes(sample = sdcNoise@manipNumVars$Gehalt_in_EUR), colour = "blue") + labs(title="Noise Addition unkorreliert")
geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

# Korreliert
# Die Methode âcorrelatedâ nimmt an, dass die Variablen normalverteilt sind.
# "correlated2" ist eine robuste Version der Methode " "correlated"
sdcNoiseKorr <- addNoise(obj = sdcInitial, variables = c('Gehalt_in_EUR'), noise = 100, method = "correlated2")

ggplot() + 
  stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
  stat_qq(aes(sample = sdcNoiseKorr@manipNumVars$Gehalt_in_EUR), colour = "blue") + labs(title="Noise Addition korreliert")
geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

# Ausreiser (Funktioniert nicht fÜr das Beispiel)
# Methode zum besseren Schutz von Ausreißern (Verwendung des Mahalanobis Abstands berechnet mit dem MCD Schätzer)
# sdcNoiseAus <- addNoise(obj = sdcInitial, variables = c('Gehalt_in_EUR'), noise = 100, method = "outdect")

# ggplot() + 
# stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
# stat_qq(aes(sample = sdcNoiseAus@manipNumVars$Gehalt_in_EUR), colour = "blue") + labs("Noise Addition mit Ausreißerschutz)
# geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

#' @5)Rank_Swapping_______________________________________________________________________________________
# Man tauscht Werte ausgewählter Variablen 

# ÃberprÜfung der Korrelationen
cor(data$Gehalt_in_EUR, data$Kaufkraft_pro_Einwohner_in_EUR_5st)

sdcInitialSwap <- createSdcObj(dat= data, keyVars = KeyVars, 
                               pramVars = PramVars,
                               numVar= NumVars, 
                               hhId = IdVars, 
                               sensibleVar = SensVars, 
                               weightVar = 'Gehalt_in_EUR')
# Rank Swapping
rankSwap(sdcInitialSwap, variables = c("Alter", "Gehalt_in_EUR"), missing = NA)

#' @6)Shuffling_______________________________________________________________________________________
# Die generelle Idee vom Shuffling ist es, neue Werte sensibler Variablen mit Hilfe eines Regressionsmodells zu finden, ohne statistische Maße zu verzerren.
# Dabei sollen mit Hilfe von nicht-sensiblen Daten, Werte fÜr die sensiblen Daten vorhergesagt werden --> Man braucht mindestens zwei numerische sensitive Variablen
# Beispiel: form <- formula(expend + income + savings Ë age + urbrur + water + electcon + age + sex, data=testdata), wobei income und savings sensitiv sind und vorhergesagt werden sollen


# Shuffling mit linearer Regression, um Zusammenhänge 
# Lineares Model anpassen und R^2 auswerten
KeyVars <- c('Geschlecht', 'Postleitzahl_5st', 'Ort_5st', 'Beruf', 'Krankheit_KZ', 'Gehalt_in_EUR') 
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Kaufkraft_pro_Haushalt_in_EUR_5st', 'Einwohner_5st')
SensVars <- c('Gehalt_in_EUR', 'Alter')
sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, numVar= NumVars,hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')

df <- data
summary(lm(Gehalt_in_EUR + Alter ~ Geschlecht + Beruf, data = df))
form <- formula(Gehalt_in_EUR + Alter ~ Geschlecht + Beruf, data = df)

# Shuffling mit spezifizierter Regressionsgleichung
sdcShuffle <- shuffle(sdcInitial, method='ds', form = form)

#' @----------------------------------------------Bestimme_Informationsverlust&Datenqualität----------------------------------------------
#' @Informationsverlust
# 1) Anzahl unterdrÜckter Werte (funktioniert nur, nachdem man auch wirklich local Suppression benutzt hat)
print(sdcInitial, 'ls')

# 2) Ãbersicht veränderter Werte + Darsellung der Veränderung durch Mosaic-Plots
ct <- c("Branche", "Ort_2st")                                            # ct enthält veränderte Variablen
data_manip <- extractManipData(sdcPRAM)                                  # Manipulierte Werte aus ausgewähltem sdc Objekt extrahieren

Tx <- table(data[, ct])
Ty <- table(Y[, ct])
par(mfrow=c(1,2))

mosaic(Tx)                                                               # Mosaic der ursprÜnglichen Daten
mosaic(Ty)                                                               # Mosaic der veränderten Daten

# 3) Wie oben bei den meisten Methoden bereits implementiert: Histogramme und Dichteplots, um Veränderungen der Daten zu sehen

#' @Datenqualität
# 1) Statistische Maße: Mean, Kovarianz, Korrelation
# Mean, Kovarianz, Korrelation der ursprÜnglichen Daten
colMeans(sdcInitial@origData[, NumVars], na.rm = TRUE)                  # NumVars enthält die numerische Variablen. Man könnte hier aber auch sensible numerische Variablen dazunehmen --> Bsp. Einkommen als sensitive Variable
cov(sdcInitial@origData[, NumVars])
cor(sdcInitial@origData[, NumVars])

# Mean, Kovarianz, Korrelation der anonymisierten Daten
colMeans(sdcInitial@manipNumVars[, NumVars], na.rm = TRUE)
cov(sdcInitial@manipNumVars[, NumVars])
cor(sdcInitial@manipNumVars[, NumVars])

# 2) Abstand zwischen dem ursprÜnglichen Datensatz und dem anonymisierten
# --> Je höher der Abstand, desto geringer ist die Datenqualität - dafÜr aber auch ein geringeres Disclosure Risiko

# Bestimme Abstand fÜr alle Variablen im sdcMicro Objekt sdcInital 
sdcInitial <- dUtility(sdcInitial)
sdcInitial@utility$il1

# Abstand bezogen auf ausgewählte Variablen und nicht auf den gesamten Datensatz
subset <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Alter', 'Gehalt_in_EUR')
dUtility(obj = sdcInitial@origData[,subset], xm = sdcInitial@manipNumVars[,subset], method = 'IL1')


#' @----------------------------------------------Abschließende_Risikokalkulation----------------------------------------------
# Nachdem man seine Daten anonymisiert hat, sollte man nochmals nach den oben genannten Metriken schauen:

# k-anonymity
print(sdcInitial, 'kAnon')
sum(sdcInitial@risk$individual[,2] < 3)

# l-diversity
sdcInitial <- ldiversity(obj = sdcInitial, ldiv_index = c("Gehalt_in_EUR"), l_recurs_c = 2, missing = NA)       
sdcInitial@risk$ldiversity
sdcInitial@risk$ldiversity[,'Gehalt_in_EUR_Distinct_Ldiversity']

# SUDA
sdcInitial <- suda2(obj = sdcInitial, missing = NA)
sdcInitial@risk$suda2$score
sdcInitial@risk$suda2

# plot density of DIS-SUDA scores
density <- density(sdcInitial@risk$suda2$disScore)
plot(density, main = 'Density plot of DIS-SUDA scores')

# ursprÜngliches Risiko
sdcInitial@risk$individual

# Risiko nach der Anonymisierung
sdcInitial@risk$global$risk

# Erwartete Anzahl an Reidentifizierungen
sdcInitial@risk$global$risk_ER

# Anzahl an Beobachtungen, die ein höheres Risiko als ein bestimmter Schwellenwert haben
sum(sdcInitial@risk$individual[,1] > 0.05)





