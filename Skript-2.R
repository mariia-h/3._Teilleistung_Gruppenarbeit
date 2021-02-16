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
