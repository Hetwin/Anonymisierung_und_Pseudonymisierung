library(sdcMicro)
set.seed(17)

#Einlesen der Daten
dat <- read.csv2("/home/christoph/Schreibtisch/DAV/AG Anonymisierung und Pseudonymisierung/GIT/Datenbasis_V4.csv",sep=",")

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


