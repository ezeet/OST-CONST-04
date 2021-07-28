# Herzlich willkommen zur R-Auffrischung!
# Dieser Überblick ist selbstverständlich nicht vollständig, sondern soll lediglich einige Anhaltspunkte liefern,
# wie man mit Daten umgehen kann; von der Dateneinlesung bis hin zu den Analysen.
# Das Datenfile, welches wir hier verwenden wurde aus der Studie 'R-Auffrischung_2019' entnommen.
# Die Daten wurden mithilfe der Funktion 'Testdaten erzeugen' innerhalb von Unipark 'fabriziert'. -> Keine echten Daten!

# Wichtig: Es gibt meistens (oder immer) mehrere Wege wie man zum Ziel kommt. Es gibt unterschiedliche Funktionen, welche das
# Gleiche bewerkstelligen, oder unterschiedliche Packages, mit eingebauten Funktionen, welche für das Gleiche zuständig sind.
# R ist open source, was auch heisst, dass auch immer neue Packages geschrieben werden, welche dann verwendet werden können.



# Falls ihr Hilfe benötigt
# R hat eine eingabute Hilfefunktion, welche euch (sofern ihr die Befehle kennt) direkt weithelfen können:
?mean
?read.csv
# Falls ihr die Funktion oder den Befehl nicht kennt ist Google euer Freund!
# R ist open source und daher werdet ihr sehr viele hilfreiche Seiten dazu im Internet finden
# Falls ihr zu irgendetwas eine Frage habt ist die Wahrscheinlichkeit sehr gross, dass jemand anders
# die gleiche Frage auch schon hatte, und sie in einem Forum diskutiert wurde.

# Kurse zu R:
# https://www.coursera.org
# Forum in welchem sehr viel zu R diskutiert wird:
# https://stackoverflow.com
# ... und viele weitere hilfreiche Seiten

# Fehlermeldungen können auch einfach gegoogelt werden. Vielleicht (oder besser: sehr wahrscheinlich) hatte schon
# jemand mal das gleiche Problem wie ihr.

# Workflow: Wenn ihr mit R und nicht mit R-Studio -> verwendet Shortcuts
# cmd + 1 -> Konsole | cmd + 2 -> Skript | cmd + 3 -> Graphical Interface (Plots)
# (ich selbst arbeite allerdings mit RStudio)

# Shortcuts (Mac):
# Alt + 3 -> Hashtag (#)
# Alt + n -> Tilde (~)
# Alt + 7 -> Pipe (|)
# Alt + 5 respektive + 6 -> Eckige Klammern ([])
# Alt + 8 respektive + 9 -> Geschwungene Klammern ({})

########################################################################################################
################################# Arbeitsfläche vorbereiten ############################################
########################################################################################################

# Arbeitsbereich aufräumen --> löscht alle Inhalte, die noch auf der Konsole gespeichert sind.
# Verhindert, dass allfällige bereits gespeicherte Variablen ausversehen verwendet werden.
# Achtung, falls ihr mehrere Projekt habt oder mit verschiedenen Datensätzen arbeitet!
rm(list = ls())

### benötigte Packages laden und installieren (Wenn sie noch nicht installiert sind, müsst ihr sie einmal
# mit 'install.packages' installieren)

# install.packages('psych') # muss jeweils nur einmal installiert werden. Danach reicht der Aufruf über 'library'
# install.packages('psy')
# install.packages('foreign', dependencies = TRUE)
# install.packages('heplots')
# install.packages('gplots')
# install.packages('ggplot2')
# install.packages('reshape2')
# install.packages('nlme')
# install.packages('lmerTest')
# install.packages('lsr')

library(psych)
library(psy)
library(foreign)
library(heplots)
library(gplots)
library(ggplot2)
library(reshape2)
library(nlme)
library(lmerTest)
library(lsr)
library(ggpubr)
library(Matrix)
library(languageR)
library(lsr)
library(reshape2)
library(rcompanion)
library(magrittr)

# R ist open source! Es werden immer neue Packages geschrieben, und alte geupdated!
# Google ist euer Freund (und Stackoverflow, Quora, cookbook-r, ...)

### Arbeitsverzeichnis festlegen
getwd() # --> zeigt einem das Arbeitsverzeichnis an, in welchem man sich gerade befindet
setwd('/Users/XYZ/Desktop/R-Auffrischung_2019') # --> den gewünschten Arbeitsordner angeben; XYZ ersetzen mit eurem User-Namen
# setwd('/Users/mkeller/Documents/Lehre/R-Auffrischung_2019/R-Auffrischung_2019') #(mein spezifischer Ordner)
setwd('~/Desktop/R-Auffrischung_2019') # führt zum Gleichen; Shortcut
# Achtung: Windows User -> setwd("C:/Users/XYZ/Documents/...")

### Daten einlesen
d = read.csv2('Testdaten_R_Auffrischung.csv') # Variante für csv Files, wie man sie bspw. von Unipark erhält
#Datensatz mit 92 Variablen, 200 Vpn

head(d)

# Falls Fehler auftauchen, kann es sein, dass der separator ein anderer ist (evt. ',')

# Weitere Befehle um Daten einzulesen:
# read.csv() | read.table() Unterschiede sind die Defaulteinstellungen (separator, decimal, header, ...)
d2 = read.csv('Testdaten_R_Auffrischung.csv')
head(d2) # sieht komisch aus -> separator ist nicht korrekt, da read.csv eine andere Defaulteinstellung hat
d2 = read.csv('Testdaten_R_Auffrischung.csv', sep = ';')
head(d2) # jetzt sieht es korrekt aus
# Alternative: File direkt aufrufen ohne Verzeichnis zu setzen (führt zumindest bei mir jedoch öfters zum Absturz)
#d3 <- read.table(file.choose(), header = TRUE, sep = ";")


### Erster Blick auf das Datenfile
d 			# um die Daten anzuzeigen (oft wenig überblicklich)
head(d) 		# für einen besseren Überblick --> zeigt euch die ersten sechs Fälle an
tail(d)		# zeigt euch die letzten 6 Fälle an
str(d) 		# Variablen und deren Eigenschaft anzeigen
names(d)		# Listet alle Variablen in dem Datensatz auf


########################################################################################################
#################################### Datensatz aufbereiten #############################################
#######################################################################################################

### Abbrecher raus
nrow(d) # -> 200 (soviele Testdaten wurden insgesamt erzeugt)
# nur diejenigen, welche bis zur letzten Seite gekommen sind
d %<>% dplyr::filter(lastpage == 5996094)
nrow(d) # -> 102 (98 Fälle 'klickten' bei der EVE 'nein' an und wurden rausgefiltert)

### Ausschlüsse
# evt. hat man bei AsPredicted angegeben, dass man alle VPs ausschliesst, welche einen Wert tiefer als 3 bei Sorgfalt angegeben haben:
# nur diejenigen VPs abspeichern, welche Werte höher als 3 bei Sorgfalt haben
d.clean <- dplyr::filter(d, careful > 3)
# 64 VPN bleiben im Datensatz enthalten --> Plant beim Erheben auf jeden Fall Puffer ein

### Datenfile aufbereiten (nicht alle Schritte notwendig)

# --> alle unnötigen Variablen wie 'hflip' oder 'rts' usw. rausnehmen, damit man ein File erhält, in welchem nur die
# zu verwenden Daten enthalten sind. Nicht notwendig, gibt aber bessere Übersicht.

# letzte von uns selbst gesetzte Variable: 'comments'
head(d)
which(colnames(d) == 'comments') 			# ist an 42. Stelle
d.clean %<>% dplyr::select(1:"careful")
# Variablen von 1 bis 41 zusammenfügen (alle Fälle)
d.clean <- dplyr::select(d, 1:41)
head(d.clean)
# die Variablen 2 bis 6 noch rausnehmen
d.clean %<>% dplyr::select(-c(2:6))
# Variable, die vergessen ging wieder anfügen (aus dem unangepassten d)
d.clean %<>% tibble::add_column(comments = d$comments)
head(d.clean)

# nun sind aber wieder 102 Personen im Datensatz -- wenn Sorgfalt-Kriterium gilt, müsste dieser Befehl erneut ausgeführt werden.

# Labels geben und Variablen umbenennen (am besten bereits in Unipark) und Faktoren zu Faktoren machen.
# # Übersicht behalten
str(d.clean)
d.clean %<>% dplyr::mutate(gender = dplyr::case_when(gender == 1 ~ "male",
                                                     gender == 2 ~ "female",
                                                     gender == 3 ~ "not answered"))
d.clean %<>% dplyr::mutate(c_0001 = dplyr::case_when(c_0001 == 1 ~ "einschl_pos",
                                                     c_0001 == 2 ~ "einschl_neg",
                                                     c_0001 == 3 ~ "ausschl_pos",
                                                     c_0001 == 4 ~ "ausschl_neg"))
d.clean %<>% dplyr::rename(VPnr = lfdn)
names(d.clean)[names(d.clean) == 'd[, "comments"]'] = 'comments'
head(d.clean)

# Wenn ihr Labels setzt, können danach aber nicht mehr die Werte verwendet werden zum indexieren:
d.clean[d.clean[ ,'c_0001'] == 1 ,] # gibt leeren Datensatz, da 1 in 'einschl_pos' umbenannt wurde
d[d[ ,'c_0001'] == 1 ,] # hier wären die VPs noch drin, da im 'd' Datensatz nicht umgewandelt wurde
d.clean[d.clean[ ,'c_0001'] == 'einschl_pos' ,] # zeigt alle Fälle an, welche in der einschluss positiv Bedingung waren


# Umgang mit Missings
# !!! ACHTUNG !!! Nächste Zeile verändert den Datensatz!
d.clean = na.omit(d.clean) 	# !!! ACHTUNG !!! löscht alle Zeilen, in welchen NA's vorkommen; evt. hat das File in jeder Zeile
							# irgendwo ein NA (bspw. bei Files in welchen verschiedene Bedingungen enthalten sind)
							# Damit felhende Werte als NA bezeichnet werden kann man entweder direkt auf Unipark beim
							# Datenexport Missings als 'NA' deklarieren, oder man kodiert die Missings in R um.
							# bspw. wenn Missings als -77 kodiert sind:
d.clean %<>% dplyr::na_if(-77)
# d.clean = na.omit(d.clean) -> nochmal diesen Befehl laufen lassen führt dazu, dass alle Fälle weg sind (R deleted case-wise)
d.clean2 = na.omit(d.clean)
d.clean2
d.clean

# um für die weiteren Punkte besseren Überblick zu haben:
d %<>% dplyr::select(1:42)

#######################################################################################################
################################# Weiteres zu Indexieren ##############################################
#######################################################################################################

# Daten einer spezifische Person oder einer spezifischen Variable anschauen?
# -> am einfachsten über Indexieren: d[Fall/VP, Variable]
# wird auch für Loops und für viele weitere Operationen benötigt.

### Person:
d[1, ] 						# den ersten Fall (die erste VP) anzeigen
d[c(1:4), ]					# die ersten 4 Fälle (VPs)
d[d[ ,'lfdn'] == 209, ]		# die VP mit der lfdn 209 anzeigen
# -> aufgesplitet:
d[ ,'lfdn']					# alle 'lfdn's aller VPs
d[ ,'lfdn'] == 209			# logische Indizierung -> welche VP hat bei dieser Variable 209?
d[d[ ,'lfdn'] == 209, ]		# d[diese spezifische VP, alle Variablen]

d[d[ ,'careful'] <= 1, ]	# alle VPs, welche einen Wert kleiner oder gleich 1 bei Carefulness angegeben haben


### Variable
d[ ,'age']					# Die Variable 'age' anzeigen (für alle VPs)
d$age						# das Gleich ohne Indexierung
which(colnames(d) == 'age') # -> 37 (an welcher Stelle befindet sich die Variable 'age' im Datensatz?)
d[ ,37]						# Indexierung des Alters über die Position im Datensatz
d[37]						# ohne erste Stelle anzugeben -> Default ist Variable (und nicht Fall)


### Kombiniert:
d[1,'age']
d[d[ ,'careful'] <= 1, 'comments']


#######################################################################################################
########################################### Demographie ###############################################
#######################################################################################################

# Die folgenden Kennwerte werdet ihr dann auch beim Schreiben eurer Arbeit bei den Methoden/Versuchspersonen benötigen:
# Anzahl VPs, Geschlecht der VPs, Alter der VPs (Mittelwert und Standardabweichung)
# Plus je nachdem was bei eurer Arbeit relevant ist noch Bildung, Deutschkenntnisse, ....

head(d)

length(d[,1]) 		# N = 102
mean(d$age)     # Mittelwert Alter: 47.03 (Achtung, falls ihr noch -77 drin haben solltet)
sd(d$age)
d$age           # sieht gut aus!

# Exkurs: Falls NAs im Datensatz
d[1,'age'] = NA # Erster Wert wird als NA gesetzt
d$age
mean(d$age) # Resultat ist NA, da R nicht weiss wie damit umgehen -> wir müssen sagen wie damit umgegangen wird
mean(d$age, na.rm = T)
d[1,'age'] = 52 # wieder zurück setzen
# Exkurs ende

sd(d[ ,'age'])		# Standardabweichung Alter: 29.85
table(d$gender)		# Verteilung Geschlecht -> 32 male | 31 female | 39 not answered
table(d.clean$gender) # das Gleiche aber mit d.clean -> direkte Labels angegeben

table(d$german)		# Verteilung Deutschkenntnisse

mean(d$careful)
table(d$careful)
hist(d$careful) 		# Einfache Veranschaulichung der Verteilung


table(d$c_0001)		# Verteilung auf die verschiedenen Bedingungen
table(d.clean$c_0001)

d$comments
d$data_use_not

# ihr könnt euch auch anschauen, wie die Geschlechterverteilung innerhalb der Bedingungen ist -> list
table(d$gender, d$c_0001)
table(d.clean$gender, d.clean$c_0001)


#######################################################################################################
################################# Umkodierung und Aufbereitung ########################################
#######################################################################################################


############ Skalen bilden ############

###### Umkodierung

# Wäre eine Variable negativ kodiert kann man dies beispielsweise mit folgendem Befehl ändern (Skala geht von 1 bis 7 und NS_2 ist negativ kodiert)
# Variable_umkodiert = (maximale Ausprägung der Variable + 1) - (Wert auf der Variable)
# vorher 7 -> 8 - 7 = 1 | vorher 5 -> 8 - 5 = 3 | vorher 1 -> 8 - 1 = 7

d.clean$NS_2_umkodiert = 8 - d.clean$NS_2 # --> eine neue Variable wird erstellt und die alte bleibt erhalten
d.clean$NS_2_umkodiert
cbind(d.clean$NS_2, d.clean$NS_2_umkodiert) # test

# Exkurs:
# cbind() bindet zwei Variablen nebeneinander (column-bind), rbind() würde die gleichen Variablen untereinander binden (row-bind)
cbind(d.clean$NS_2, d.clean$NS_2_umkodiert)
rbind(d.clean$NS_2, d.clean$NS_2_umkodiert)


d.clean$NS_2 = 8 - d.clean$NS_2 # --> NS_2 wird direkt mit den neuen Werten überschrieben
cbind(d.clean$NS_2, d.clean$NS_2_umkodiert) # test
# !!Achtung!! Wenn ihr Variablen direkt in die gleiche Variable umkodiert aufpassen, dass ihr diesen Befehl nicht mehrfach laufen
# lasst. Sonst wird die Variable wieder zurück-umkodiert! Markiert euch das am besten in eurem Skript.


###### Skalen zusammenführen

# mit dem 'apply' Befehl: apply ( , , ) | apply(welche Daten, über Spalte = 2 oder über Reihe = 1?, welche Funktion?)
apply(d.clean[ ,c('NS_1', 'NS_2', 'NS_3', 'NS_4','NS_5')], 1, mean) # Mittelwerte der Skala für jede VP ( 1 -> über Reihe)
apply(d.clean[ ,c('NS_1', 'NS_2', 'NS_3', 'NS_4','NS_5')], 2, mean) # Mittelwerte für die einzelnen Variablen ( 2 -> über Spalte)

# Mittelwert in neue Variable abspeichern und im Anschluss an das Datenfile anfügen (im ersten Schritt wird erst ein Vektor gebildet)
NS_mean = apply(d.clean[ ,c('NS_1', 'NS_2', 'NS_3', 'NS_4','NS_5')], 1, mean)
NS_mean

# Funktioniert auch mit jeder anderen Funktion (bspw. für die Standardabweichung):
NS_sd = apply(d.clean[ ,c('NS_1', 'NS_2', 'NS_3', 'NS_4','NS_5')], 1, sd)
NS_sd

# Mittelwert und Standardabweichung am Ende des Datenframes anfügen (den vorher gebildeten Vektor im DataFrame anfügen)
d.clean = cbind(d.clean, NS_mean, NS_sd)
head(d.clean)

# Alternative Möglichkeit: (andere Berechnungsart und neue Variable wird direkt angefügt)
d.clean$NS_mean_neu = (d.clean$NS_1 + d.clean$NS_2 + d.clean$NS_3 + d.clean$NS_4 + d.clean$NS_5) / 5
# Q.E.D.
head(d.clean)
d.clean$NS_mean - d.clean$NS_mean_neu


# Frage bleibt noch, ob man die Skala überhaupt zusammenführen dürfte->
# bei zweit Items über Korrelation, ab 3 Items über Cronbachs-Alpha:
cronbach(d.clean[ ,c('NS_1', 'NS_2', 'NS_3', 'NS_4','NS_5')]) # bei Random Daten natürlich nicht aussagekräftig... (Faustregel: ab .7 akzeptabel)


###### Verschiedene Bedingungen zusammenbringen (siehe Word Dokument 'Datenstruktur')
# Hat man verschiedene Bedingungen auf Unipark und dadurch systematische Missings bei einer Variable muss man diese Variablen
# in eine neue Variable umkodieren -> mood_1_1 bis mood_4_5
head(d.clean)
d.clean[d.clean[ ,'c_0001'] == 'einschl_pos' ,] # mood_1_1 bis mood_1_5
d.clean[d.clean[ ,'c_0001'] == 'einschl_neg' ,] # mood_2_1 bis mood_2_5
d.clean[d.clean[ ,'c_0001'] == 'ausschl_pos' ,] # mood_3_1 bis mood_3_5
d.clean[d.clean[ ,'c_0001'] == 'ausschl_neg' ,] # mood_4_1 bis mood_4_5


# For Loop für die Umkodierung (mit dem 'd' File, da wir da die -77 nicht als NA umkodiert haben)
d %<>% tibble::add_column(mood_1 = "mood_\\d_1" != -77)


d$mood1 = d$mood_1_1 							# Neue Variable kreieren, in welche die Werte gefasst werden sollen und dieser Variable die Werte von mood_1_1 zuweisen
for (i in 1:length(d[,1])){						# von 1 bis Anzahl Versuchsteilnehmer
	if (d[i, 'mood_2_1'] != -77){				# Nur wenn KEIN '-77' in der Variable mood_2_1 steht, dann mache Folgendes:
		d[i, 'mood1'] = d[i, 'mood_2_1']		# ersetzte den Wert an der bestimmten Stelle durch den Wert von Var_1_2
	} else if (d[i, 'mood_3_1'] != -77){		# same für mood_3_1
		d[i, 'mood1'] = d[i, 'mood_3_1']
	} else if (d[i, 'mood_4_1'] != -77){		# und für mood_4_1
		d[i, 'mood1'] = d[i, 'mood_4_1']
	}
}
head(d)
cbind(d$mood1, d$mood_1_1, d$mood_2_1, d$mood_3_1, d$mood_4_1)

# mit dem d.clean funktioniert es nicht gleich, da NA ein spezieller 'Wert' in R ist. (i kann auch papperlapap sein)
d.clean$mood1 = d.clean$mood_1_1
for (paparlapap in 1:length(d.clean[,1])){
	if (!is.na(d.clean[paparlapap,'mood_2_1'])){
		d.clean[paparlapap, 'mood1'] = d.clean[paparlapap, 'mood_2_1']
	} else if (!is.na(d.clean[paparlapap,'mood_3_1'])){
		d.clean[paparlapap, 'mood1'] = d.clean[paparlapap, 'mood_3_1']
	} else if (!is.na(d.clean[paparlapap,'mood_4_1'])){
		d.clean[paparlapap, 'mood1'] = d.clean[paparlapap, 'mood_4_1']
	}
}
# check:
cbind(d$mood1, d.clean$mood1)

# für die weiteren 4 Variablen:
d.clean$mood2 = d.clean$mood_1_2
for (i in 1:length(d.clean[,1])){
	if (!is.na(d.clean[i,'mood_2_2'])){
		d.clean[i, 'mood2'] = d.clean[i, 'mood_2_2']
	} else if (!is.na(d.clean[i,'mood_3_2'])){
		d.clean[i, 'mood2'] = d.clean[i, 'mood_3_2']
	} else if (!is.na(d.clean[i,'mood_4_2'])){
		d.clean[i, 'mood2'] = d.clean[i, 'mood_4_2']
	}
}
d.clean$mood3 = d.clean$mood_1_3
for (i in 1:length(d.clean[,1])){
	if (!is.na(d.clean[i,'mood_2_3'])){
		d.clean[i, 'mood3'] = d.clean[i, 'mood_2_3']
	} else if (!is.na(d.clean[i,'mood_3_2'])){
		d.clean[i, 'mood3'] = d.clean[i, 'mood_3_3']
	} else if (!is.na(d.clean[i,'mood_4_2'])){
		d.clean[i, 'mood3'] = d.clean[i, 'mood_4_3']
	}
}
d.clean$mood4 = d.clean$mood_1_4
for (i in 1:length(d.clean[,1])){
	if (!is.na(d.clean[i,'mood_2_4'])){
		d.clean[i, 'mood4'] = d.clean[i, 'mood_2_4']
	} else if (!is.na(d.clean[i,'mood_3_4'])){
		d.clean[i, 'mood4'] = d.clean[i, 'mood_3_4']
	} else if (!is.na(d.clean[i,'mood_4_4'])){
		d.clean[i, 'mood4'] = d.clean[i, 'mood_4_4']
	}
}
d.clean$mood5 = d.clean$mood_1_5
for (i in 1:length(d.clean[,1])){
	if (!is.na(d.clean[i,'mood_2_5'])){
		d.clean[i, 'mood5'] = d.clean[i, 'mood_2_5']
	} else if (!is.na(d.clean[i,'mood_3_5'])){
		d.clean[i, 'mood5'] = d.clean[i, 'mood_3_5']
	} else if (!is.na(d.clean[i,'mood_4_5'])){
		d.clean[i, 'mood5'] = d.clean[i, 'mood_4_5']
	}
}

# nachschauen, ob es funktioniert hat:
head(d.clean)


# Wenn NAs verwendet werden geht das ganze auch ein bisschen einfacher;-) (funktioniert aber nicht wenn Characters mit drin sind)
d.clean$mood1.test = apply(d.clean[ ,c('mood_1_1', 'mood_2_1', 'mood_3_1', 'mood_4_1')], 1, mean, na.rm = T)
# check:
rbind(d.clean$mood1.test, d.clean$mood1)


# Skalenmittelwert bilden
cronbach(d.clean[ ,c('mood1', 'mood2', 'mood3', 'mood4','mood5')])
d.clean$mood.mean = apply(d.clean[ ,c('mood1', 'mood2', 'mood3', 'mood4','mood5')], 1, mean)


# Gruppierungsvariable c_0001 in die verschiedenen UVs umkodieren, da wir ein 2x2 (einschl vs ausschl x pos vs neg) haben
# 1 -> einschluss positiv | 2 -> einschluss negativ | 3 -> ausschluss positiv | 4 -> ausschluss negativ
# Wenn ihr so eine ANOVA rechnen würdet, dann würde R es wie eine einfaktorielle ANOVA mit 4 Level handhaben
# Was wir aber möchten ist eine 2-Faktorielle ANOVA rechnen zu können
# Ziel: zwei Variablen kreieren, welche jeweils für die entsprechende UV kodieren.
# c_0001		einschl_ausschl		pos_neg
# 1 			1					1
# 2			1					2
# 3			2					1
# 4			2					2
# (siehe auch Word Dokument 'Datenstruktur')

# ifelse Befehel -> ifelse(welche Variable auf was getestet wird, was abgespeichert wird wenn TRUE, was wenn FALSE )
einschl_ausschl_d.char = ifelse(d$c_0001 <3, 'einschl', 'ausschl')
einschl_ausschl_d.num = ifelse(d$c_0001 <3, 1, 2)
pos_neg_d.char = ifelse(d$c_0001 %%2, 'negativ', 'positiv')
pos_neg_d.num = ifelse(d$c_0001 %%2, 1, 2) # %%x testet ob eine Zahl ohne Rest durch x teilbar ist
# check:
head(cbind(d$c_0001, einschl_ausschl_d.num, einschl_ausschl_d.char, pos_neg_d.num, pos_neg_d.char))

# bei unserem d.clean haben wir jedoch die Variablen bereits als Faktoren. Daher funktioniert folgendes nicht mehr:
einschl_ausschl = ifelse(d.clean$c_0001 <3, 'Auspraegung_1', 'Auspraegung_2') # not meaningful for vectors
# daher:
d.clean$einschl_ausschl = ifelse(d.clean$c_0001 == 'einschl_pos' | d.clean$c_0001 == 'einschl_neg', 'einschluss', 'ausschluss') # noch nicht als Faktor deklariert
d.clean$pos_neg = as.factor(ifelse(d.clean$c_0001 == 'einschl_pos' | d.clean$c_0001 == 'ausschl_pos', 'positiv', 'negativ')) # direkt als Faktor deklariert

str(d.clean)

d.clean$einschl_ausschl = as.factor(d.clean$einschl_ausschl)
str(d.clean)



# würde natürlich auch über Loop gehen mit if else innerhalb des Loops
d.clean$ea = c()
d.clean$pn = c()
for (i in 1:length(d.clean[,1])){
	if (d.clean[i,'c_0001'] == 'einschl_pos'){
		d.clean[i, 'ea'] = 1
		d.clean[i, 'pn'] = 1
	} else if (d.clean[i,'c_0001'] == 'einschl_neg'){
		d.clean[i, 'ea'] = 1
		d.clean[i, 'pn'] = 2
	} else if (d.clean[i,'c_0001'] == 'ausschl_pos'){
		d.clean[i, 'ea'] = 2
		d.clean[i, 'pn'] = 1
	} else if (d.clean[i,'c_0001'] == 'ausschl_neg'){
		d.clean[i, 'ea'] = 2
		d.clean[i, 'pn'] = 2
	}
}
#check
head(cbind(einschl_ausschl_d.num, d.clean$ea, pos_neg_d.num, d.clean$pn))




# Subsets bilden und abspeichern
# Bspw. wenn man Männer und Frauen separat betrachten möchte
data.m = subset(d.clean, d.clean$gender == 'male')
data.f = subset(d.clean, d.clean$gender == 'female')
data.m
data.f


# Je nach Analyse müsste das Datenfile noch ins Longformat überführt werden (wenn ihr eine Repeated measure habt)
# so wie es jetzt ist, ist es im sogenannten Wide-Format -> Jede VP hat genau eine Zeile
# zur Veranschaulichung bringen wir unser Datenset in ein Long-Format und tun so, als wäre mood.mean und NS_mean
# zwei Variablen, welche repeated gemessen wurden. Und wir tun so, als ob die pos_neg Variable die Between UV sei.
# (siehe auch Word Dokument 'Datenstruktur')

d.clean$mood.mean
d.clean$NS_mean

d.clean.long = melt(d.clean, id.vars = c('VPnr', 'pos_neg'), measure.vars = c('mood.mean', 'NS_mean'),
                    variable.name = 'DV', value.name = 'dependent_variable')
head(d.clean.long)
d.clean.long = d.clean.long[order(d.clean.long$VPnr), ]
head(d.clean.long)


#######################################################################################################
#################################### Deskriptive Analsyse #############################################
#######################################################################################################

# Erste Überblicke über die Daten

# Häufigkeitstabellen können über die 'table' Funktion erhalten werden
table(d.clean$gender) # Die table Funktion schliesst standardmässig NA's aus
table(d.clean$c_0001)
table(d.clean$einschl_ausschl, d.clean$pos_neg)

# Grafische Darstellung von Häufigkeiten über die Funktion 'barplot' oder 'hist'
barplot(table(d.clean$NS_mean))
hist(d.clean$NS_mean)

# Normalverteilung der Daten
# --> erster Überblick ebenfalls durch den barplot-Befehl
barplot(table(d.clean$mood.mean))
# Statistischer Test über:
shapiro.test(d.clean$mood.mean)	 	# Der Shapiro-Test prüft die Nullhypothese, dass die Werte normalverteilt sind;
									# ist der Test signifikant (p<.05) muss angenommen werden, dass die Werte NICHT
									# normalverteilt sind.


# wichtige Kennzahlen
mean(d.clean$age)	# Mittelwert
sd(d.clean$age)		# Standardabweichung
min(d.clean$age)		# kleinster Wert
max(d.clean$age)		# grösster Wert
range(d.clean$age)	# Range
# ....
mean(d.clean$mood.mean)
sd(d.clean$mood.mean)

#Übersicht
describe(d.clean$age)

# Mittelwerte der einzelnen Variablen in Abhängigkeit der Bedingung --> mit tapply (wendet eine Funktion auf x an in Abhängigkeit von y):
tapply(d.clean[ ,'mood.mean'], d.clean[ ,'c_0001'], mean) # oder:
tapply(d.clean$mood.mean, d.clean$c_0001, mean)
tapply(d.clean$mood.mean, d.clean$c_0001, sd)

# oder mit Indexieren:
mean(d.clean$mood.mean[d.clean$einschl_ausschl == 'einschluss'])
mean(d.clean$mood.mean[d.clean$einschl_ausschl == 'ausschluss'])

mean(d.clean$mood.mean[d.clean$einschl_ausschl == 'einschluss' & d.clean$pos_neg == 'positiv'])
mean(d.clean$mood.mean[d.clean$einschl_ausschl == 'einschluss' & d.clean$pos_neg == 'negativ'])

# Mittelwerte von Mood in Abhängigkeit des Geschlechts und der Bedingung
tapply(d.clean$mood.mean, list(d.clean$c_0001, d.clean$gender), mean)

#######################################################################################################
############################################### Plots #################################################
#######################################################################################################

# simple plotting:
plot(d.clean$NS_mean, d.clean$mood.mean)
abline(lm(d.clean$NS_mean ~ d.clean$mood.mean), col = 'red')

# histograms
hist(d.clean$mood.mean)

# Mittelwerte plotten für Mittelwerte
plotmeans(mood.mean ~ c_0001, data = d.clean)
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1)
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating')
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating', xlab = 'Bedingung')
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating', xlab = 'Bedingung', connect = F)
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating', xlab = 'Bedingung', connect = F, legend = c('einschl pos', 'einschl neg', 'ausschl pos', 'ausschl neg'))
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating', xlab = 'Bedingung', connect = F, legend = c('einschl pos', 'einschl neg', 'ausschl pos', 'ausschl neg'), n.label = F)
plotmeans(mood.mean ~ c_0001, data = d.clean, las = 1, ylab = 'rating', xlab = 'Bedingung', connect = F, legend = c('einschl pos', 'einschl neg', 'ausschl pos', 'ausschl neg'), n.label = F, barcol = 'darkgrey')


?plotmeans

# super powerful zum Plotten ist natürlich ggplot und ggplot2! -> google
# Aber achtung! Damit kann man Stunden um Stunden verbringen! Macht zwar Spass, es sollte aber am Ende auch zweckdienlich sein.

#######################################################################################################
######################################### Analsyse ####################################################
#######################################################################################################

########## Reliabilität der Skalen
cronbach(d.clean[25:29])
cronbach(subset(d.clean, einschl_ausschl == 'ausschluss')[25:29])



########## t-Test
# one-sample -> t.test(AV, mu = x)
t.test(d.clean$mood.mean, mu = 4) # gegen Mitte getestet
cohensD(d.clean$mood.mean, mu = 4)
# two-sample -> t.test(AV ~ Gruppe)
t.test(d.clean$mood.mean ~ d.clean$pos_neg)
cohensD(d.clean$mood.mean ~ d.clean$pos_neg)
# paired t-test -> t.test(AV1, AV2, paired = TRUE)
t.test(d.clean$NS_mean, d.clean$mood.mean, paired = T) # macht natürlich inhaltlich überhaupt keinen Sinn...
cohensD(d.clean$NS_mean, d.clean$mood.mean, method = 'paired')

########## Correlation
# cor(X, Y, method = '')
cor(d.clean$mood.mean, d.clean$NS_mean, method = 'spearman')


########## Regression (linear)
# summary(lm(X ~ Y, data = datenset))
model = lm(mood.mean ~ NS_mean, data = d.clean)
summary(model)


########## ANOVA
# one-way -> summary(aov(AV ~ Gruppe, data = datenset))
model2 = aov(mood.mean ~ c_0001, data = d.clean)
summary(model2)
anova(lm(mood.mean ~ c_0001, data = d.clean)) # Alternative, welches auf das gleiche Resultat kommt
etasq(model2) # Effektstärke berechnen lassen (partielles Eta-Quadrat); benötigt Package 'heplots'

# two-way -> summary(aov(AV ~ Bed1 * Bed2, data = datenset))
model3 = aov(mood.mean ~ einschl_ausschl*pos_neg, data = d.clean)
summary(model3)
etasq(model3)

# one-way repeated measure -> Daten müssen im long format sein

# summary(lme(AV ~ Treatment, random = ~1 | subject, data = datenset))
model4 = lme(dependent_variable ~ DV, random = ~1 | VPnr, data = d.clean.long)
summary(model4)

# two-way repeated measure
# model5 = lme(AV ~ Treatment*Bedingung, random = ~1 | subject, data = datenset)
model5 = lme(dependent_variable ~ DV*pos_neg, random = ~1 | VPnr, data = d.clean.long)
summary(model5)
  #ACHTUNG: DUMMY-KODIERUNG! Bei Interpretation der Haupteffekte berücksichtigen! Hier können sonst Fehler passieren.






