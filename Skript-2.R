#b) 

#Hilfsfunktion fuer Berechnung von Modus 

#Diese Funktion nimmt den Vektor "v" als Eingabe und gibt den Moduswert als Ausgabe.

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}


#c)

#Hilfsfunktion fuer Berechnung von Erwartungshaefinkeiten bei 
#chi-quadrat-Koeffizient

#Eingabe: numerc Matrix, Kotingenztafel mit absolute Haeufigkeiten

#Ausgabe: numeric Matrix, enthaelt Erwartungshaefinkeiten fuer jede 
#Beobachtungspaare 

#Funktion berechner Tabelle mit Erwartungshaefinkeiten, dabei nutzt Formel 
#(Spaltensumme * Zeilensumme) / n, mit n Summe von allen Eintaege der gegebenen 
#Matrix  

expect<- function(x){
  
  result<- x
  n <- sum(x)
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      result[i,j] <- n * (sum(x[,j]) / n) * (sum(x[i,]) / n)
    }
  }
  return(result)
}



#e)
#Hilfsfunktion fuer die Umwandlung von daten

#Eingabe : zwei numeric vektoren

#Ausgabe : Data frame (besser für weitere Bearbeitungen)

conversion <- function(x,y){
  names(x) = c("niedrig","mittel","hoch")
  names(y) = c("niedrig","mittel","hoch")
  mydf = data.frame(x,y)
  mydf_t = as.data.frame(t(as.matrix(mydf)))
  return(mydf_t)
}


#f)

#Hilfsfunktion fuer extrahieren passenden Vektor 

#Eingabe: x Data Frame 
#Ausgabe: vektor mit Beobachtungen fuer Studienfach aus gegebene Data Frame 

Studienfach <- function(x){
  return(x$Studienfach)
}

#Eingabe: x Data Frame 
#Ausgabe: vektor mit Beobachtungen fuer Interesse_an_Mathematik aus gegebene Data Frame

Interesse_an_Mathematik <- function(x){
  return(x$Interesse_an_Mathematik)
}

#Eingabe: x Data Frame 
#Ausgabe: vektor mit Beobachtungen fuer Mathe_LK aus gegebene Data Frame

Mathe_LK <- function(x){
  return(x$Mathe_LK)
}
