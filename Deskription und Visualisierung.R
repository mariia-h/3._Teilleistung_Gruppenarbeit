library(vcd)
source("Skript-1.R")
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


#hier sieht man deutlich ,dass das am haufigsten vorkommenden Studienfach "Statistik" ist 
# die Mehrheit der Studenten weisen ein durschnittliche Interesse an Mathe und Programierung auf, das kann man in die Modalwerte "5" ablesen.
# und durch Der Median sieht man, dass "5" das Wert ist, das die Datenreihe sowhol bei der Interesse an Mathematik als auch bei der Interesse an Programmieren halbiert ,
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
#Der Korrelationskoeffizient kann Werte zwischen -1 und 1 annehmen, wobei ein Korrelationskoeffizient von 0 bedeutet, 
#dass kein Zusammenhang zwischen beiden Variablen existiert.Ein Korrelationskoeffizient von +1 beschreibt einen perfekten positiven Zusammenhang
#zwischen beiden Variablen, während eine Korrelation von -1 einen perfekten negativen (inversen) Zusammenhang (Antikorrelation) beschreibt.

# da hier 0,1544032 rauskommt, heißt das , dass hier fast gar kein linearen Zusammenhang zwischen Alter und die Interesse an Mathe gibt.

# Basic scatterplot
plot(Datensatz=Datensatz$Mathe_LK, y=Datensatz$Alter)


kategorien(Datensatz)
#Das Ergebnis:
#[[1]]
#niedrig mittel hoch
#x       0      2    2
#y       0     50   50

#[[2]]
#niedrig mittel   hoch
#x     4.0 13.000 15.000
#y    12.5 40.625 46.875

#in erste Tabelee sieht mann ,dass für Mathe Studierend : nur 1 Student (20% der studierende) zeigt ein mittelere Interese an Mathematik,
#während 4 studierende (80% der studierende) zeigen hoch Interesse an Mathe. Und keiner hat niedrige Interesse an Mathe.

#in zweite Tabelee sieht mann ,dass für Informatik Studierend : nur 1 Student (5.263158% der studierende) zeigt ein mittelere Interese an Programmieren,
#während 18 studierende (94.73684% der studierende) zeigen hoch Interesse an Programmieren. Und keiner hat niedrige Interesse an Programmieren.


visualisation(Studienfach,Interesse_an_Mathematik,Mathe_LK)
