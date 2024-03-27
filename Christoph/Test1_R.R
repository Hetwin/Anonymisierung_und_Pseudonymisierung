library(sdcMicro)
library(readxl)
library(tidyverse)
set.seed(17)

#Einlesen der Daten
setwd("F:/DAV")
dat <- read.csv2("Datenbasis_V4.csv",sep=",")

#Erste ansicht der Daten
summary(dat)
#Sieht alles ok aus 


#Kurzer sdcMicro Versuch

#Wähle Schlüsselvariablen
kv <- c("Einwohner_5st", "Gehalt_in_EUR","Religion_allgemein")
sdc_O <- createSdcObj(dat,keyVars = kv)

sdc_O
# Dies liefert eine erste Auswertung
# Wir haben 50 Zeilen / Beobachtungen und 34 Variablen
# Es wurden die Schlüsselvariablen wie oben angegeben gewählt
# Key Variable                  Number of categories      Mean size     Size of smallest (>0)       
#Einwohner_5st                   49 (49)                1.020  (1.020)   1(1)
#Gehalt_in_EUR                   26 (26)               1.923  (1.923)   1(1)
#Religion_allgemein              4  (4)                12.500 (12.500)    2(2)
#
# Number of observations violating
#- 2-anonymity: 50 (100.000%)
#- 3-anonymity: 50 (100.000%)
#- 5-anonymity: 50 (100.000%)
# Es ist keine Art von k-Anonymität bis 5 gegeben

# Nun lokal Supression
sdc_LS <- localSuppression(sdc_O)

sdc_LS
#Key Variable Number of categories      Mean size         
#Einwohner_5st                    7 (49)     1.000  (1.020)
#Gehalt_in_EUR                   25 (26)     1.958  (1.923)
#Religion_allgemein                    4  (4)    11.750 (12.500)
#Size of smallest (>0)    
#1 (1)
#1 (1)
#9 (2)
#---------------------------------------------------------------------

#  Infos on 2/3-Anonymity:
#  
#  Number of observations violating
#- 2-anonymity: 0 (0.000%) | in original data: 50 (100.000%)
#- 3-anonymity: 18 (36.000%) | in original data: 50 (100.000%)
#- 5-anonymity: 42 (84.000%) | in original data: 50 (100.000%)

#----------------------------------------------------------------------


#  Local suppression:
#  
#  KeyVar | Suppressions (#) | Suppressions (%)
#    Einwohner_5st |               44 |           88.000
#    Gehalt_in_EUR |                3 |            6.000
#    Religion_allgemein |                3 |            6.000
#   ----------------------------------------------------------------------

# Mit unterdrückung von 44 Datensätzen für Einwohner, 3 für Gehalt und 3 für Religion
# erhalten wir zumindest 2-Anonymität

#FRAGE: Wie kommt man an den untertrückten Datensatz
print(sdc_LS,'ls')
sdc_LS@origData
sdc_LS@localSuppression
sdc_LS@manipKeyVars


# Damit kann man den Datenasatz bauen
index <- seq (1,50,1)
ind_dat_adj <- na.omit(index[sdc_LS@manipKeyVars != "NA"])
dat_adj<-dat[ind_dat_adj,]
dim(dat)
dim(dat_adj)

#Nun mit mehr Daten
names(dat)
kv1<-names(dat)[c(1,2,3,4,7,8,9,10,11,12,14,16,18,20,22,23,24,31)]
sdc_1 <- createSdcObj(dat,keyVars = kv1)
sdc_1
sdc_LS1 <- localSuppression(sdc_1)
index <- seq (1,50,1)
ind_dat_adj1 <- na.omit(index[sdc_LS1@manipKeyVars != "NA"])
ind_dat_adj1 <- na.omit(index[sdc_LS1@manipKeyVars != "<NA>"])
length(ind_dat_adj1)
dat_adj1<-dat[ind_dat_adj1,]
dim(dat)
dim(dat_adj1)

##########################
#Sandkasten Beispiel #####
##########################

setwd("F:/DAV")
data <- read.csv2("Datenbasis_V4.csv",sep=",")

data$Geschlecht
data$Vorname
data$Nachname
data$Geburtsjahr
data$Ethnizitaet
KeyVars <- c( 'Geschlecht', 'Ethnizitaet', 'Krankheit_KZ', 'Postleitzahl_2st')
NumVars <- c('Kaufkraft_pro_Einwohner_in_EUR_5st', 'Gehalt_in_EUR', 'Alter')
IdVars <- c('Identifikationsnummer')
SensVars <- c('Gehalt_in_EUR')

sdcInitial <- createSdcObj(dat= data, keyVars = KeyVars, numVar= NumVars,hhId = IdVars, sensibleVar = SensVars, weightVar = 'Gehalt_in_EUR')


# 48 Beobachtungen verletzen 2-Anonymität

# Wir reduzieren den Datensatz auf nur wenige Attribute

dat1 <- data[,c(7,8,9,13,24)]
names(dat1)
KeyVars1 <- c('Krankheit_KZ','Geschlecht','Ethnizitaet','Postleitzahl_2st')
SensVars1 <- c('Gehalt_in_EUR')

sdc_dat1 <- createSdcObj(dat= dat1, keyVars = KeyVars1, sensibleVar = SensVars1)

## Das Problem ist die 2-stellige PLZ. Diese hat fast nur individuelle ausprägungen


dat2 <- data[,c(7,8,9,24)]
names(dat2)
KeyVars2 <- c('Krankheit_KZ','Geschlecht','Ethnizitaet')
SensVars2 <- c('Gehalt_in_EUR')

sdc_dat2 <- createSdcObj(dat= dat2, keyVars = KeyVars2, sensibleVar = SensVars2)


sdc_ld <- ldiversity(sdc_dat2)

### Eleganter
cols = c('Krankheit_KZ','Geschlecht','Ethnizitaet')
dat_x <- data
dat_x[,cols] <- lapply(dat_x[,cols],factor)
KeyVars_x <- c('Krankheit_KZ','Geschlecht','Ethnizitaet')
SensVars_x <- c('Gehalt_in_EUR')

sub_Vars <- c(KeyVars_x,SensVars_x)
subData <- dat_x[,sub_Vars]

sdc_sub <- createSdcObj(dat= subData, keyVars = KeyVars_x, sensibleVar = SensVars_x)

#Risikoauswertung
ir <- sdc_sub@risk$individual
ir_table <- cbind(subData, ir)
View(ir_table)

print(sdc_sub, type="kAnon")
# Die Zeilen können über die fk Werte ermittelt werden
sum(sdc_sub@risk$individual[,2] < 2)
sum(sdc_sub@risk$individual[,2] < 3)

# Um einen 2-Anonymen datensatz zu erzeigen könnte man folgendes tun

index_sub <- seq(1,dim(dat_x)[1],1)
leave_2<-na.omit(index_sub[sdc_sub@risk$individual[,2] > 1])
subData_adj<-subData[leave_2,]
dim(subData_adj)

sdc_sub_adj <- createSdcObj(dat= subData_adj, keyVars = KeyVars_x, sensibleVar = SensVars_x)
print(sdc_sub_adj, type="kAnon")

### Somit haben wir einen 2-Anonymen Subdatensatz angelegt
#Vergleich l-Diveristät der Ergebnisse

sdc_ldiv <- ldiversity(obj = sdc_sub, ldiv_index = c("Gehalt_in_EUR"), l_recurs_c = 2, missing = NA)
sdc_ldiv@risk$ldiversity
sdc_ldiv@risk$ldiversity[,'Gehalt_in_EUR_Distinct_Ldiversity']
print(sdc_sub,"risk")

sdc_ldiv <- ldiversity(obj = sdc_sub_adj, ldiv_index = c("Gehalt_in_EUR"), l_recurs_c = 2, missing = NA)
sdc_ldiv@risk$ldiversity
sdc_ldiv@risk$ldiversity[,'Gehalt_in_EUR_Distinct_Ldiversity']
print(sdc_sub_adj,"risk")


### Nun andere Methoden zur Anonymisierung
## Local Supression

sdc_LS1 <- localSuppression(sdc_sub, k=3)
index <- seq (1,50,1)
ind_dat_adj1 <- which(is.na(sdc_LS1@manipKeyVars),arr.ind = TRUE)[,1]
length(ind_dat_adj1)
dat_adj1<-subData[-ind_dat_adj1, ]
dim(dat)
dim(dat_adj1)

#Global Recording
#Hierfür sollten wir die PLZ wieder reinnehmen
### Eleganter
cols = c('Krankheit_KZ','Geschlecht','Ethnizitaet')
dat_x2 <- data
dat_x2[,cols] <- lapply(dat_x2[,cols],factor)
KeyVars_x2 <- c('Krankheit_KZ','Geschlecht','Ethnizitaet','Postleitzahl_2st')
SensVars_x2 <- c('Gehalt_in_EUR')

sub_Vars2 <- c(KeyVars_x2,SensVars_x2)
subData2 <- dat_x2[,sub_Vars2]

sdc_sub2 <- createSdcObj(dat= subData2, keyVars = KeyVars_x2, sensibleVar = SensVars_x2)

sdc_GR1 <- globalRecode(sdc_sub2,column=c("Postleitzahl_2st"),breaks = seq(1,121,30))
sdc_GR1
table(sdc_GR1@manipKeyVars$Postleitzahl_2st)

# Damit 2 Anonymer Datensatz
dat_x2$Postleitzahl_2st <- sdc_GR1@manipKeyVars$Postleitzahl_2st
subData2$Postleitzahl_2st <- sdc_GR1@manipKeyVars$Postleitzahl_2st
index_sub <- seq(1,dim(dat_x2)[1],1)
leave_22<-na.omit(index_sub[sdc_GR1@risk$individual[,2] > 1])
subData2_adj<-subData2[leave_22,]
dim(subData2_adj)


sdc_sub_adj2 <- createSdcObj(dat= subData2_adj, keyVars = KeyVars_x, sensibleVar = SensVars_x)
print(sdc_sub_adj2, type="kAnon")