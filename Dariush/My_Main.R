# %%%%%%%%%%%%%%%%%%%%%%%
#
# My_Main-Skript
#
# Beschreibung: 
# Hier werden die Daten eingelesen und anonymisiert. 
#
# %%%%%%%%%%%%%%%%%%%%%%%


#install.packages("mltools")
library(sdcMicro)

library(ggplot2)
library(plyr)


# Daten einlesen
setwd ("/Users/Dariush.Sadeghi-Yam/sdcMicro")
data <- read.csv("Datenbasis_V4.csv", header = T)

# Variablennamen 
names(data) 

# Erstes sdcMicro Objekt mit ausgewählten Quasiidentifikatoren und Gewichten erzeugen
data$Geschlecht<-factor(data$Geschlecht)
#data$Postleitzahl_2st<-factor(data$Postleitzahl_2st)
data$Vorname<-factor(data$Vorname)
data$Nachname<-factor(data$Nachname)
data$Alter<-2023-data$Geburtsjahr

KeyVars <- c( 'Geschlecht', 'Ethnizitaet', 'Krankheit_KZ', 'Postleitzahl_2st')
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Gehalt_in_EUR', 'Alter')
IdVars <- c('Identifikationsnummer')
SensVars <- c('Gehalt_in_EUR')

sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, numVar= NumVars,hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')

# Sample frequency of individual’s key
freq(sdcInitial, type = 'fk')

# Population frequency of individual’s key
freq(sdcInitial, type = 'Fk')

sdcInitial@risk$individual

########### Metriken #####################
# k-Anonymität
print(sdcInitial, 'kAnon')
sum(sdcInitial@risk$individual[,2] < 3)

# l-diversity
sdcInitial <- ldiversity(obj = sdcInitial, ldiv_index = c("Gehalt_in_EUR"), l_recurs_c = 2, missing = NA)
sdcInitial@risk$ldiversity
sdcInitial@risk$ldiversity[,'Gehalt_in_EUR_Distinct_Ldiversity']

# SUDA

########### Methoden #####################
# Recoding
## Global recoding
# Anzahlen von Postleitzahl_2st vor Recoding
table(sdcInitial@manipKeyVars$Postleitzahl_2st)

PLZVor <- sdcInitial@manipKeyVars$Postleitzahl_2st
PLZNach <- floor(PLZVor/10)

# Numerische Variablen: Recode PLZ mit der Funktion globalRecode 
# Hinweis: Die Intervalle sind als links-offen implementiert. Mögliche Workarounds (https://sdcpractice.readthedocs.io/en/latest/anon_methods.html
#     - Anpassung bsp. durch Wahl der Intervallgrenzen marginal vor linker Grenze, z.B. statt "10" "9.9")
#     - Alternativ kann auch der zu Grunde liegende Code angepasst werden

sdcInitialRec <- globalRecode(sdcInitial, column = "Postleitzahl_2st", breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = 1:9)

table(sdcInitialRec@manipKeyVars$Postleitzahl_2st)

# Kategorielle Variablen: Recode Geschlecht mit der Funktion groupAndRename
sdcInitialRec  <-  groupAndRename(obj = sdcInitialRec, var = c("Geschlecht"),
                               before = c("maennlich", "weiblich"),
                               after = c(0,1))
table(sdcInitialRec@manipKeyVars$Geschlecht)

# Top and Bottom Coding
# Top coding bei Alter 60
summary(data$Alter)
sdcInitialRec <- topBotCoding(obj = sdcInitialRec, value = 60, replacement = 60,
                           kind = 'top', column = 'Alter')

# Bottom coding bei Alter 15
sdcInitialRec <- topBotCoding(obj = sdcInitialRec, value = 15, replacement = 15,
                           kind = 'bottom', column = 'Alter')

summary(sdcInitialRec@manipNumVars$Alter)

# Rundung
## Beispiel notwendig???

# Suppression
## Local Suppression
# Hinweis:  Damit die Methoden eine Lösung finden und um die Rechenzeit zu reduzieren,
#           empfiehlt es sich im Vorfeld die Anzahl der Quasi-Identifikatoren und der Anzahl der Kategorien zu reduzieren.
### Local Suppression ohne "Importance Vector"
sdcInitialSup <- localSuppression(sdcInitialRec, k = 5)

print(sdcInitialSup, 'ls')

# Supression Rückgängig machen
sdcInitialSup <- undolast(sdcInitialSup)

#### Local Suppression mit "Importance Vector" 
# Angabe der Variablen, für die eine Variation für den Datennutzen wichtig ist.
# Der Importance Vektor entählt die Priorität der zu unterdrückenden Variablen (d.h. er enthält eine Zahl zwischen 1 und derAnzahl der Key Variablen)
# Wahl von 5-Anonymität als Ziel
sdcInitialSup <- localSuppression(sdcInitialRec, importance = c(1, 4, 1, 4), k = 5)
print(sdcInitialSup, 'ls')

sdcInitialSup <- undolast(sdcInitialSup)

# m-Appraoch:
# Anmwendung von Local Suppression auf alle möglichen Teilbestände der Größe m
# Nutzung der Eigenschaft "combs"

# Bsp.: 5-Anonymität für alle Teilbestände mit zwei Keyvariablen und anschließend für den ganzen Datensatz
# Um den ganzen Datenbestand zu wählen, wird der zweite Eintrag in "combs" auf die Anzahl der Keyvariablen gesetzt
sdcInitialSup <- localSuppression(sdcInitialRec, k = 5, combs = c(2, 4))
print(sdcInitialSup, 'ls')
sdcInitialSup <- undolast(sdcInitialSup)

# Bsp.: 3-Anonymität auf den ganzen Datensatz und 5-Anonymität für Teilbestände mit zwei Keyvariablen
sdcInitialSup <- localSuppression(sdcInitialRec, k = c(2, 5), combs = c(4, 2))
print(sdcInitialSup, 'ls')
sdcInitialSup <- undolast(sdcInitialSup)

# manuelle Suppression --> ToDo???
#file[is.na(sdcInitialSup@manipKeyVars$Ethnizitaet) &
#       !is.na(sdcInitialSup@origData$Ethnizitaet),'Beruf'] <- NA

# Mit einander verbundene Variablen können durch Spezifikation der "Ghost"-Variablen angegeben werden
# Ghost (verbundene) Variablen werden durch eine Liste spezifiziert
ghostVars <- list()

# Jede Verbindung wird wiederrum durch eine Liste abgebildet, deren erstes Element die Kevariable ist 
# das zweite Element ist die verbundene Variable
# Suppression auf die die Keyvariable hat auch eine Suppression auf die Ghost Variable zur Folge 
ghostVars[[1]] <- list()
ghostVars[[1]][[1]] <- "Ort_5st"
ghostVars[[1]][[2]] <- c("Ort_2st")

## Create the sdcMicroObj

sdcInitial2 <- createSdcObj(dat= data, keyVars = KeyVars, numVars = NumVars,
                           weightVar = 'Gehalt_in_EUR', ghostVars = ghostVars)

# The manipulated ghost variables are in the slot manipGhostVars
sdcInitial2@manipGhostVars

# Alternative Funktion für Lokal Supperssion, die die Angabe eines Schwellenwertes erlaubt: localSupp() 
summary(sdcInitial2@risk$individual[,1])

# Anzahl an Datensätzen mit einem individuellen Risiko größer als 0.003
sum(sdcInitial@risk$individual[,1] > 0.003)

# Local Suppression
sdcInitial2 <- localSupp(sdcInitial2, threshold = 0.003, keyVar = 'Postleitzahl_2st')

sdcInitial2
print(sdcInitial2)

# Pertubation
## PRAM (Post RAndomization Method)

set.seed(123)

# Anwendung vorn PRAM auf alle Variablen
names(data)
data$Ort_2st <- factor(data$Ort_2st)
data$Branche <- factor(data$Branche)
PramVars <- c('Branche', 'Ort_2st') 
sdcPRAM <- createSdcObj(dat= data, keyVars = KeyVars, 
                        pramVars = PramVars,
                        numVar= NumVars, 
                        hhId = IdVars, 
                        sensibleVar = SensVars, 
                        weightVar = 'Gehalt_in_EUR')

# Hinweis: Es nicht möglich die Übergangsmatrix vorzugeben
# "variables" gibt die Variablen an, auf die PRAM angewendet werden soll (müssen nicht vor als PRAM-Variablen definiert werden) 
# Mit "pd" ist es möglich die Minima für die "Diagonalen Einträge" anzugeben, diese geben die Wahrscheinlichkeiten an, dass ein bestimmter Wert sich nach der Anwendung von PRAM nicht ändert.
# Durch Angabe von "strata_variables" können unwahrscheinliche Kombinationen ausgeschlossen werden 
#sdcPRAM <- pram(obj = sdcPRAM, variables = c('Branche', 'Ort_2st') , pd = c(0.9, 0.5))
sdcPRAM <- pram(obj = sdcPRAM, variables = c('Branche', 'Ort_2st'), 
                                strata_variables = c('Gehalt_in_EUR')
                                , pd = c(0.9, 0.5))

sdcPRAM

## Microaggregation
sdcMicroAgg <- createSdcObj(dat= data, keyVars = KeyVars, 
                        numVar= NumVars, 
                        hhId = IdVars, 
                        sensibleVar = SensVars)

sdcMicroAgg <- microaggregation(obj = sdcMicroAgg, variables = c('Gehalt_in_EUR'),
                               aggr = 5, method = "mdav", measure = "mean")


#sdcMicroAgg2 <- microaggregation(obj = sdcMicroAgg, variables = c('Gehalt_in_EUR'),
#                                aggr = 5, method = "simple", measure = "mean")

## Noise Addition
set.seed(123)
sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, 
                        pramVars = PramVars,
                        numVar= NumVars, 
                        hhId = IdVars, 
                        sensibleVar = SensVars, 
                        weightVar = 'Gehalt_in_EUR')

# Unkorreliert
sdcNoise <- addNoise(obj = sdcInitial,
                       variables = c('Gehalt_in_EUR'),
                       noise = 100, method = "additive")

hist(sdcNoise@manipNumVars$Gehalt_in_EUR, 
     col = 4,
     main = "Häufigkeit Gehalt",
     xlab = "Gehalt in Euro",
     ylab = "Anzahl")
qqnorm(sdcNoise@manipNumVars$Gehalt_in_EUR)
qqline(sdcNoise@origData$Gehalt_in_EUR)
sdcNoise@manipNumVars$Gehalt_in_EUR - sdcNoise@origData$Gehalt_in_EUR

ggplot() + 
  stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
  stat_qq(aes(sample = sdcNoise@manipNumVars$Gehalt_in_EUR), colour = "blue") +
  geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

# Korreliert
# Die Methode “correlated” nimmt an, dass die Variablen normalverteilt sind.
# "correlated2" ist eine robuste Version der Methode " is a version of the method "correlated"
sdcNoiseKorr <- addNoise(obj = sdcInitial,
                     variables = c('Gehalt_in_EUR'),
                     noise = 100, method = "correlated2")

ggplot() + 
  stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
  stat_qq(aes(sample = sdcNoiseKorr@manipNumVars$Gehalt_in_EUR), colour = "blue") +
  geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

# Ausreiser (Funktioniert nicht für das Beispiel)
# Methode zum besseren Schutz von Ausreißern (Verwendung des Mahalanobis Abstands berechnet mit dem MCD Schätzer)
#sdcNoiseAus <- addNoise(obj = sdcInitial,
#                         variables = c('Gehalt_in_EUR'),
#                         noise = 100, method = "outdect")

#ggplot() + 
#  stat_qq(aes(sample = data$Gehalt_in_EUR), colour = "red") + 
#  stat_qq(aes(sample = sdcNoiseAus@manipNumVars$Gehalt_in_EUR), colour = "blue") +
#  geom_abline(aes(slope = 1, intercept = 0), linetype = 2)

## Rank Swapping

# Überprüfung der Korrelationen
cor(data$Gehalt_in_EUR, data$Kaufkraft_pro_Einwohner_in_EUR_5st)
cor(df[,data])

set.seed(123)

KeyVars <- c( 'Geschlecht', 'Ethnizitaet', 'Krankheit_KZ', 'Postleitzahl_2st')
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Gehalt_in_EUR', 'Alter')
IdVars <- c('Identifikationsnummer')
SensVars <- c('Gehalt_in_EUR')

sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, 
                           pramVars = PramVars,
                           numVar= NumVars, 
                           hhId = IdVars, 
                           sensibleVar = SensVars, 
                           weightVar = 'Gehalt_in_EUR')


# Rank Swapping
rankSwap(sdcInitial, variables = c("Alter", "Gehalt_in_EUR"), missing = NA)

## Shuffling (ToDo)
# Lineares Model anpassen und R^2 auswerten
df<-data
summary(lm(Gehalt_in_EUR ~Geschlecht + Alter + Beruf, data = df))

KeyVars <- c( 'Geschlecht', 'Ethnizitaet', 'Krankheit_KZ', 'Postleitzahl_2st')
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Gehalt_in_EUR', 'Alter')
IdVars <- c('Identifikationsnummer')
SensVars <- c('Gehalt_in_EUR')

sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, 
                           pramVars = PramVars,
                           numVar= NumVars, 
                           hhId = IdVars, 
                           sensibleVar = SensVars, 
                           weightVar = 'Gehalt_in_EUR')

# Shuffling using the specified regression equation
#sdcShuffle <- shuffle(sdcInitial, method='ds',
                      #form = Gehalt_in_EUR ~Geschlecht + Alter + Beruf, data = df
#                      Geschlecht + Alter + Beruf ~ EXP + Gehalt_in_EUR)
