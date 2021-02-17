
#c)
#Funktion fuer Beschreibung von Zusammenhangsmasse 

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
  expected <-expect(table) 
  table <- as.matrix(table)
  expected<- as.matrix(expected)
  n <- sum(table)
  chi.sq <- 0
  for(i in 1:nrow(table)){
    for(j in 1:ncol(table)){
      chi.sq <- chi.sq + ((table[i,j] - expected[i,j])^2 / expected[i,j])
    }
  }
  pearson  <- sqrt(chi.sq/(chi.sq+n))
  kor_pearson <- sqrt(min(ncol(table), nrow(table))/(
                        min(ncol(table), nrow(table)) - 1)) * pearson
  phi <- sqrt(chi.sq/(n))
  cramer <- chi.sq/((min(ncol(table), nrow(table)) - 1) * n)
  ausgabe[[k]]<- c(pearson, kor_pearson, cramer)
  }
  interpret <- list(c(),c(),c())
  test <- c("Studienfach und die Tatsache, dass Person Mathe-LK hatte", 
            "Interesse an Mathematik und Studienfach",
            "Interesse an Programmieren und Studienfach")
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
  rangkor[1] <- cor(x$Interesse_an_Mathematik, Interesse_an_Programmieren, method = "spearman")
  rangkor[2] <- cor(x$Interesse_an_Mathematik, Interesse_an_Programmieren, method = "kendal")
  rangkor[3] <- cor(x$Interesse_an_Mathematik, Interesse_an_Programmieren, method = "pearson")
  interpret_rangcor <- c()
  vorzeichen <- "positive "
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




#test2