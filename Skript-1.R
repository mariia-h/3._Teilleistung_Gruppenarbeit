library(readr)
#install.packages("vcd")
library(MASS)
library(vcd)
source("Skript-2.R")

# a) Die Funktion lagemasse berechnet ein paar Lagemassen fuer metrische Merkmale. 
# Input:  dataframe, default:  unseres Datensatzes
#Output:  liste enthaelt: - arithmetisches Mittel vom Alter
#                         - Modus vom Interesse an Mathematik 
#                         - geometrisches Mittel vom Interesse an Prog. 

lagemasse = function(x = datensatz){
  
  arith.mittel = 1/length(which(!is.na(x$Alter))) * 
    sum(x$Alter,rm.na = TRUE)
  
  modus = getmode(x$Interesse_an_Mathematik)
  
  geom.mittel =    prod(x$Alter,rm.na = TRUE) ^ 
                   1/length(which(!is.na(x$Alter)))  
  
  
  return(list(arithmetisches.Mittel = arith.mittel,
              Modus = modus , 
              geometrisches.Mittel = geom.mittel))
}


# b) Die Funktion bfunktion berechnet ein paar Lagemassen fuer für kategoriale Variablen. 
# Input:  dataframe, default:  unseres Datensatzes
#Output:  liste enthaelt: - 
#                         - Modus vom Studienfach und "das zugehörige Stuienfach"
#                         - Modus und Median des Interesse an Mathematik 
#                         - Modus und Median des Interesse an Programmieren.

#Für Häufigkeitsverteilungen, die aus den Werten einer nominalen Variable gebildet sind, ist der Modalwert das einzige Lagemaß, 
#das berechnet werden kann,denn die Variablenwerte weisen weder numerische Intervalle oder Verhältnisse noch eine Rangordnung auf

bfunktion<- function(x){
  
  Fach.Modus <- getmode(x$Studienfach)
  IM.Modus <-  getmode(x$Interesse_an_Mathematik)
  IP.Modus <-  getmode(x$Interesse_an_Programmieren)
  
  IM.Median <-   median(x$Interesse_an_Mathematik)
  IP.Median<-  median(x$Interesse_an_Programmieren)
  
  tt <- table(x$Studienfach)
  modus_name<- names(tt[which.max(tt)]) # um zu wissen welche Level bzw. welche Studienfach am häufigsten vorkommt
  


   list_data <- list(c(Fach.Modus,modus_name),c(Modalwert = IM.Modus,Median = IM.Median),c(Modalwert = IP.Modus, Median = IP.Median )) 
   names(list_data)<- c("Modalwert von Studienfach sowie des entsprechenden Fach:","Für die Interesse an Mathematik:","Für die Interesse an Programmieren:")
   
   return( list_data)     
}


#c)
#Funktion fuer die Beschreibung von Zusammenhangsmasse 

#Eingabe: Data Frame, von interesse sind Spalten Mathe_LK(character), Studienfach(character), Interesse_an_Mathematik(numeric), Interesse_an_Programmieren(numeric)

#Ausgabe: Vektor von characters, der Beschreibt Zusammenhang zwischen 4 Beobachtungspaare
#         1. Zusammengang zwischen Studienfach und die Tatsache, dass Person Mathe-LK hatte 
#         2. Zusammengang zwischen Interesse an Mathematik und Studienfach
#         3. Zusammengang zwischen Interesse an Programmieren und Studienfach
#         4. Korrelation zwischen Interesse an Mathematik und Interesse an Programmieren
#         Fuer jede zusammengangspaare gibt es Liste mit drei Koeffizienten und Interpretation dazu 

#Funktion Beschreibt 4 Zusammenhang zwischen 4 Beobachtungspaare:
#Fuer 1-3. Zusammenhangspaare erstellt die Funktion fuer jede Paare eine Liste mit
# Pearson Kontingenzindex, Korrigierte Pearson Kontingenzindex und Crammers Kontingenzindex,
#fuer jeden Index gibt es Interpretation 
#Fuer 4. Zusammenhangspaare erstellt die Funktion fuer jede Paare eine Liste mit
#Spearman Rangkorellationskoeffizient, Kendall`sche` Rangkorellationskoeffizient,
#Pearson Korellationskoeffizient; fuer jeden Index gibt es Interpretation. 

zusammenhangsmasse <- function(x){
  ausgabe <- list()
  for(k in 1:3){
    if(k==1)  table <- table(x$Mathe_LK, x$Studienfach) #Zusammenhang zwischen Studienfach und die Tatsache, dass Person Mathe-LK hatte
    else if(k==2) table <- table(x$Studienfach, x$Interesse_an_Mathematik)#Zusammenhang zwischen Studienfach und Interesse an Mathematik 
    else table <- table(x$Studienfach, x$Interesse_an_Programmieren)#Zusammenhang zwischen Studienfach und Interesse an Programmieren 
  expected <-expect(table)  #Berechnen Erwartunghaeufigkeit
  table <- as.matrix(table)
  expected<- as.matrix(expected)
  n <- sum(table)
  chi.sq <- 0
  
  # Berechnen chi-quadrat
  for(i in 1:nrow(table)){
    for(j in 1:ncol(table)){
      chi.sq <- chi.sq + ((table[i,j] - expected[i,j])^2 / expected[i,j])
    }
  }
  
  pearson  <- sqrt(chi.sq/(chi.sq+n)) #Pearson Kontingenzindex
  kor_pearson <- sqrt(min(ncol(table), nrow(table))/(
                        min(ncol(table), nrow(table)) - 1)) * pearson #Korrigierte Pearson Kontingenzindex
  
  cramer <- chi.sq/((min(ncol(table), nrow(table)) - 1) * n) #Crammers Kontingenzindex
  ausgabe[[k]]<- c(pearson, kor_pearson, cramer) 
  }
  interpret <- list(c(),c(),c())
  test <- c("Studienfach und die Tatsache, dass Person Mathe-LK hatte", 
            "Interesse an Mathematik und Studienfach",
            "Interesse an Programmieren und Studienfach")
  #Erstellen Vektor mit Interpretation fuer jeder Index
  for(l in 1:3){
  for(i in 1:3){
    if(ausgabe[[l]][i]<=0.1) interpret[[l]][i] <-paste( "Es besteht kein Zusammenhang zwischen ", test[l])
    else if(ausgabe[[l]][i]>0.1 && ausgabe[[l]][i]<0.4)interpret[[l]][i] <- paste("Es besteht sehr niedriger Zusammenhang zwischen ", test[l])
    else if(ausgabe[[l]][i]>=0.4 && ausgabe[[l]][i]<=0.6)interpret[[l]][i] <- paste("Es besteht mittlerer Zusammenhang ",test[l])
    else if(ausgabe[[l]][i]>0.6 && ausgabe[[l]][i]<0.9)interpret[[l]][i] <- paste("Es besteht ganz starke Zusammenhang", test[l])
    else if(ausgabe[[l]][i]>=0.9)interpret[[l]][i] <- paste(" Es besteht absoluten Zusammenhang zwischen ",test[l])
  }
  }
  rangkor <- c()
  rangkor[1] <- cor(x$Interesse_an_Mathematik, x$Interesse_an_Programmieren, method = "spearman") #Spearman Rangkorellationskoeffizient
  rangkor[2] <- cor(x$Interesse_an_Mathematik, x$Interesse_an_Programmieren, method = "kendal") #Kendall`sche Rangkorellationskoeffizient
  rangkor[3] <- cor(x$Interesse_an_Mathematik, x$Interesse_an_Programmieren, method = "pearson") #Pearson Korellationskoeffizient
  interpret_rangcor <- c()
  vorzeichen <- "positive "
  #Erstellen Vektor mit Interpretation fuer jeder Index
  for(i in 1:3){
    if (rangkor[i] < 0) vorzeichen <- "negative " 
    if(rangkor[i]<=0.1) interpret_rangcor[i] <-paste( "Es gibt keine Korellation zwischen Interesse an Mathematik und Interesse an Programmieren")
    else if(rangkor[i]>0.1 && rangkor[i]<0.4)interpret_rangcor[i] <- paste("Es gibt sehr niedrige", vorzeichen, "Korellation zwischen Interesse an Mathematik und Interesse an Programmieren")
    else if(rangkor[i]>=0.4 && rangkor[i]<=0.6)interpret_rangcor[i] <- paste("Es gibt mittlere", vorzeichen, "Korellation zwischen Interesse an Mathematik und Interesse an Programmieren")
    else if(rangkor[i]>0.6 && rangkor[i]<0.9)interpret_rangcor[i] <- paste("Es gibt ganz starke", vorzeichen, "Korellation zwischen Interesse an Mathematik und Interesse an Programmieren")
    else if(rangkor[i]>=0.9)interpret_rangcor[i] <- paste("Es gibt absolute", vorzeichen, "Korellation zwischen Interesse an Mathematik und Interesse an Programmieren")
    vorzeichen <- "positive "
    }
 return(c("Zusammengang zwischen Studienfach und die Tatsache, dass Person Mathe-LK hatte",  
          list("Pearson Kontingenzindex" = (paste(ausgabe[[1]][1], " => ", interpret[[1]][1])),
               "Korrigierte Pearson Kontingenzindex"  = (paste(ausgabe[[1]][2], " => ", interpret[[1]][2])),
               "Crammers Kontingenzindex" = (paste(ausgabe[[1]][3], " => ", interpret[[1]][3])) ),
          "Zusammengang zwischen Interesse an Mathematik und Studienfach",  
          list("Pearson Kontingenzindex" = (paste(ausgabe[[2]][1], " => ", interpret[[2]][1])),
               "Korrigierte Pearson Kontingenzindex"  = (paste(ausgabe[[2]][2], " => ", interpret[[2]][2])),
               "Crammers Kontingenzindex" = (paste(ausgabe[[2]][3], " => ", interpret[[2]][3])) ),
          "Zusammengang zwischen Interesse an Programmieren und Studienfach",  
          list("Pearson Kontingenzindex" = (paste(ausgabe[[3]][1], " => ", interpret[[3]][1])),
               "Korrigierte Pearson Kontingenzindex"  = (paste(ausgabe[[3]][2], " => ", interpret[[3]][2])),
               "Crammers Kontingenzindex" = (paste(ausgabe[[3]][3], " => ", interpret[[3]][3])) ),
          "Korrelation zwischen Interesse an Mathematik und Interesse an Programmieren",
          list("Spearman Rangkorellationskoeffizient" = paste(rangkor[1], " => ", interpret_rangcor[1]),
               "Kendall`sche Rangkorellationskoeffizient" = paste(rangkor[2], " => ", interpret_rangcor[2]),
               "Pearson Korellationskoeffizient" = paste(rangkor[3], " => ", interpret_rangcor[3]))
          ))
}


 
#d)
#Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang 
#zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt

#Eingabe: Data Frame, von Interesse sind Spalten Alter(numeric), Mathe_LK(factor) 

#Ausgabe: eine Liste mit einer Tabelle zur Veranschaulichung der Daten und die zugehörigen Punktbiseriale Korrelation  

#die einzige Zusammenhangsmaß. die die Zusammenhang zwischen einer metrischen und einer
#dichotomen Variablen berechnet und ausgibt, ist die Punktbiseriale Korrelation


dfunktion <- function(x){

#die Zusammenhang als Table zwischen Alter und ob man in der Schule Mathe-LK hatte oder nicht
  table <- table(x$Mathe_LK,x$Alter) 

  Alter<-x$Alter

  #Da die zweite vektor numeric  sein muss
  Mathe_LK<-as.numeric(x$Mathe_LK)  

  # diese R funktion lifert uns die punktbiseriale Korrelationskoeffizient 
  Punktbiseriale_Korrelation <- cor.test(Alter, Mathe_LK) 
  
  return(list(table,Punktbiseriale_Korrelation))
}

    
    
    

#e) 
#Eine Funktion fuer die Beschreibung verschiedene ordinale Variablen
#Eingabe: Data Frame, von interesse sind Spalten Interesse_an_Mathematik(numeric), 
#Interesse_an_Programmieren(numeric)

#ausgabe: eine Liste mit 2 verschiedene Verteilungen der Daten als Kategorien:
#1: Verteilung der Studenten, die mathe studieren, in Uebereinstimmung mit wie viel sie es magen
#2: Verteilung der Studenten, die Informatik studieren,  in Uebereinstimmung mit viel sie es magen

#Die  Funtion liefert uns in prozentanteil, ob die Studenten jung oder alt sind und wieviel Studenten, die 
#mathe/Informatik studieren, eigentlich ihre Studienfach magen.


#Erstellung der Funktion
kategorien <- function(x){
  
  #Wie viele Studenten informatik studieren
  anzahlinfo = length(x$Studienfach[x$Studienfach == "Informatik"])
  #Wie Viele Studenten Mathe studieren
  anzahlmathe= length(x$Studienfach[x$Studienfach == "Mathe"])
  
  #Die Studenten, die Mathe studieren in 3 Kategorien verteilen in Ueberstimmung mit wie viel sie Mathe magen.
  mathekategorisiert = c(length(x$Interesse_an_Mathematik[x$Interesse_an_Mathematik <= 2 & x$Studienfach=="Mathe"]),
                         length(x$Interesse_an_Mathematik[(x$Interesse_an_Mathematik ==3 | x$Interesse_an_Mathematik==4) & x$Studienfach=="Mathe"] ),
                         length(x$Interesse_an_Mathematik[x$Interesse_an_Mathematik>=5 & x$Studienfach=="Mathe"]))
  
  
  #Dieselbe Verteilung aber in Prozenanteil
  mathekategorisiert_pro = c(((length(x$Interesse_an_Mathematik[x$Interesse_an_Mathematik <= 2 & x$Studienfach=="Mathe" ])*100)/anzahlmathe),
                             ((length(x$Interesse_an_Mathematik[(x$Interesse_an_Mathematik ==3 | x$Interesse_an_Mathematik==4)& x$Studienfach=="Mathe"])*100)/anzahlmathe),
                             ((length(x$Interesse_an_Mathematik[x$Interesse_an_Mathematik>=5& x$Studienfach=="Mathe"])*100)/anzahlmathe))
  mathedf = conversion(mathekategorisiert, mathekategorisiert_pro)
  
  #Informatik is Analog zu mathe
  infokategorisiert = c(length(x$Interesse_an_Programmieren[x$Interesse_an_Programmieren <= 2 & x$Studienfach=="Informatik"]),
                        length(x$Interesse_an_Programmieren[(x$Interesse_an_Programmieren==3 | x$Interesse_an_Programmieren==4) & x$Studienfach=="Informatik"] ),
                        length(x$Interesse_an_Programmieren[x$Interesse_an_Programmieren>=5 & x$Studienfach=="Informatik"]))
  
  
  
  infokategorisiert_pro = c(((length(x$Interesse_an_Programmieren[x$Interesse_an_Programmieren <= 2 & x$Studienfach=="Informatik" ])*100)/anzahlinfo),
                            ((length(x$Interesse_an_Programmieren[(x$Interesse_an_Programmieren ==3 | x$Interesse_an_Programmieren==4)& x$Studienfach=="Informatik"])*100)/anzahlinfo),
                            ((length(x$Interesse_an_Programmieren[x$Interesse_an_Programmieren>=5& x$Studienfach=="Informatik"])*100)/anzahlinfo))
  
  
  info_df = conversion(infokategorisiert,infokategorisiert_pro) 
  
  
  return(list(mathedf,infodf))
}



#f)

#Funktion fuer Visualisation von kategorielen Variablen 

#Datensatz ablesen 
Data <- data.frame(read.csv("Datensatz.csv"))

#Passende Vektoren extrahieren 
Studienfach <- Studienfach(Data)
Interesse_an_Mathematik <- Interesse_an_Mathematik(Data)
Mathe_LK <- as.vector(Mathe_LK(Data))

#Eingabe: s, i, m Vektoren die visualisiert werden 

#Ausgabe: Mosaikplot

#Funktion stellt Variable Mathe_LK, Interesse_an_Mathematik und Studienfach zusammen

visualisation <- function(s,i,m){
  
  mosaic(~ Studienfach + Interesse_an_Mathematik+Mathe_LK,main = "Beziehung zwischen Studienfach, Interesse an Mathematok und Vorhandensein von Mathe LK", highlighting = "Mathe_LK", highlighting_fill = c("lightblue", "pink"),
         direction = c("v","h","v"))
  
}

visualisation(Studienfach,Interesse_an_Mathematik,Mathe_LK)


