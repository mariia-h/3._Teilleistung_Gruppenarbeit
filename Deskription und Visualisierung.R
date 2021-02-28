library(vcd)
source("Skript-1.R")
source("Skript-2.R")
Datensatz <- read_csv("Datensatz.csv")



lagemasse(Datensatz)
#Das Ergebnis : 
#$arithmetisches.Mittel
#[1] 24.88

#$Modus
#[1] 5

#$geometrisches.Mittel
#[1] 2.733854e+137

#arithmetisches Mittel vom Alter zeigt uns ,dass die Studierende im Durchschnitt 25 jahre alt sind
# Außerdem das Modus vom Interesse an Mathematik betregt 5,das heißt ,dass die Studenten ein überdurchschnittliches Interesse an Mathematik haben
# und durch die geometrisches Mittel vom Interesse an Prog sieht man ,dass es ein durchschnittliche Veränderungsrate von 4.311268e+137 gibt


bfunktion(Datensatz)

#Das Ergebnis: 
#$`Modalwert von Studienfach sowie des entsprechenden Fach:`
#[1] "Data Science" "Data Science"

#$`Für die Interesse an Mathematik:`
#Modalwert    Median 
#5         4 

#$`Für die Interesse an Programmieren:`
#Modalwert    Median 
#4.0       4.5 


#hier sieht man deutlich ,dass das am haufigsten vorkommenden Studienfach "Data Science" ist 
# die Mehrheit der Studenten weisen ein durschnittliche Interesse an Mathe und Programierung auf, das kann man in die Modalwerte "5" ablesen.
# und durch Der Median sieht man, dass "4" das Wert ist, das die Datenreihe  bei der Interesse an Mathematik halbiert , das gleiche gilt auch Für die Interesse an Programmieren
#sodass eine Hälfte der Daten unterhalb und die andere Hälfte oberhalb des Medians in der geordneten Reihe liegt.


zusammenhangsmasse(Datensatz)
#Der Kontingenzkoeffizient kann Werte zwischen 0 und 1 annehmen.
#Dabei bedeutet 0, dass es keinen Zusammenhang zwischen den beiden Merkmalen gibt, 
#und 1, dass es einen vollständigen Zusammenhang gibt.
# deswegen kann mann hier ablesen, dass zwischen Studienfach und die Tatsache, dass Person Mathe-LK hatte kein Zusammenhang besteht
#und es ganz eine schwach bis mittlerer Zusammenhangzwischen Interesse an Mathematik und Studienfach besteht.
# Für die Zusammengang zwischen Interesse an Programmieren und Studienfach weisen Kontingenzindexen aud unterschidliche stärke von die Zussemmenhänge, 
#also von ein ganze schwacher bis ein ganz starker Zusammenhang. deshalb kann mann hier keinen klaren Aussage abschließen .
# dazu kann man hier auch ablesen , dass es keine Korellation zwischen Interesse an Mathematik und Interesse an Programmieren gibt


dfunktion(Datensatz)

#Das Ergebnis: 
#[[1]]

#20 21 22 23 24 25 26 27 28 29 30
#Ja    0  2  5  6 14  9  6  6  7  0  1
#Nein  1  0  2  6 11 10  7  4  1  2  0

#[[2]]

#Pearson's product-moment correlation

#data:  Alter and Mathe_LK2
#t = 0.2354, df = 98, p-value = 0.8144
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1734553  0.2191675
#sample estimates:
#       cor 
#0.02377276

#Der Korrelationskoeffizient kann Werte zwischen -1 und 1 annehmen, wobei ein Korrelationskoeffizient von 0 bedeutet, 
#dass kein Zusammenhang zwischen beiden Variablen existiert.Ein Korrelationskoeffizient von +1 beschreibt einen perfekten positiven Zusammenhang
#zwischen beiden Variablen, während eine Korrelation von -1 einen perfekten negativen (inversen) Zusammenhang (Antikorrelation) beschreibt.

# da hier 0.02377276 rauskommt, heißt das , dass hier fast gar kein linearen Zusammenhang zwischen Alter und die Interesse an Mathe gibt.




kategorien(Datensatz)

#[[1]]
#niedrig mittel hoch
#x       0      2    2
#y       0     50   50

#[[2]]
#niedrig mittel   hoch
#x     4.0 13.000 15.000
#y    12.5 40.625 46.875



#in erste Tabelee sieht mann ,dass für Mathe Studierend :  2 Studenten (50% der studierende) zeigt ein mittelere Interese an Mathematik,
#während 2 studierende (50% der studierende) zeigen hoch Interesse an Mathe. Und keiner hat niedrige Interesse an Mathe.

#in zweite Tabelee sieht mann ,dass für Informatik Studierend : nur 4 Student (12.5% der studierende) zeigt ein niedrige Interese an Programmieren,
#während 13 studierende (40.625% der studierende) zeigen mittel Interesse an Programmieren. Und 15 (46.875% der studierende) studierende haben hoch Interesse an Programmieren.


visualisation(Studienfach,Interesse_an_Mathematik,Mathe_LK)

#bei Studienfach Informatik kann man bemerken, dass die Leute, die hoch Interesse an Mathe haben,
#hatten auch in der Schule Mathe-LK

#und Für Data Science und Statistik spielt Mathe_Lk fast keine rolle 
#wenn es um Inreresse _an_Mathematik angeht
