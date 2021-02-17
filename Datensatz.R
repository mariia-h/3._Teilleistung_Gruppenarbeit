### Der Datensatz ist in Form einer Dataframe darzustellen. 



## Alter: 

    #wir haben einfach einen Vektor erstellet mit den Alter, sodass sie normal verteilt sind.
alter = round(rnorm(100,sd= 2,mean = 25))


##Studienfach: 
   #Analoge zum Alter aber wir haben statistik und Data Science mehrmals geschrieben um "haufiger"
#zu simulieren
fach = c(rep("Statistik",5),rep("Data Science",5),"Mathe",
                rep("Informatik",3))
studienfach = sample(fach,100,replace = TRUE)


##Interesse_an_mathematik: 
# Zusammenhang zwischen den Fachstudiengaengen und dem Interesse an Mathematik: 
# absteigende Sortierung: - Mathematik  
#                         - Statistik
#                         - Data Science
#                         - Informatik 


#Normalerweiser magen die Studenten, derjenigen mathe studieren, deswegen hörere Anzahlen haufiger
#vorkommen. Dann nutzen wir dies neu Vektor um das Interesse an Mathe auf die Leute verteilen, die 
#mathe  studieren 
mathe_mathe = c(rep(1,1),rep(1,2),rep(3,3),rep(4,6),rep(5,7),rep(6,7),rep(7,6))
mathe_mathe.final = sample(mathe_mathe,size = length(which(studienfach =="Mathe")),
               replace = TRUE)

#Idr magen die Informatiker*innen Mathe veniger als derjenigen, die Mathe studieren.
mathe_info = c(rep(7,1), rep(6,1), rep(5,3), rep(4,6), rep(3,7), rep(2,7),rep(1,6))
mathe_info.final = sample(mathe_info,size = length(which(studienfach =="Informatik")),
               replace = TRUE)

# Statistik ist analog verteilt
mathe_stat = c(rep(1,2),rep(1,3),rep(3,4),rep(4,7),rep(5,6),rep(6,6),rep(7,5))
mathe_stat.final = sample(mathe_stat,size = length(which(studienfach =="Statistik")),
               replace = TRUE)

# Data sience ist auch analog verteilt
mathe_ds = c(rep(1,2),rep(1,3),rep(3,4),rep(4,7),rep(5,6),rep(6,6),rep(7,5))
mathe_ds.final = sample(mathe_stat,size = length(which(studienfach =="Data Science")),
                          replace = TRUE)

#Am Ende bekommen wir dies Vektor.
Interesse_an_Mathematik = rep(0,100)
Interesse_an_Mathematik[which(studienfach=="Mathe")] = mathe_mathe.final
Interesse_an_Mathematik[which(studienfach=="Informatik")] = mathe_info.final
Interesse_an_Mathematik[which(studienfach=="Data Science")] = mathe_ds.final
Interesse_an_Mathematik[which(studienfach=="Statistik")] = mathe_stat.final


## Interesse_an_programmieren

#Analog zum Inderesse_an_mathematik
prog_mathe = c(rep(7,1), rep(6,1), rep(5,3), rep(4,6), rep(3,7), rep(2,7),rep(1,6))
prog_mathe.final = sample(prog_mathe,size = length(which(studienfach =="Mathe")),
                           replace = TRUE)


prog_info = c(rep(1,1),rep(1,2),rep(3,3),rep(4,6),rep(5,7),rep(6,7),rep(7,6))
prog_info.final = sample(prog_info,size = length(which(studienfach =="Informatik")),
                          replace = TRUE)

prog_stat = c(rep(1,2),rep(1,3),rep(3,4),rep(4,7),rep(5,6),rep(6,6),rep(7,5))
prog_stat.final = sample(prog_stat,size = length(which(studienfach =="Statistik")),
                          replace = TRUE)

prog_ds = c(rep(1,2),rep(1,3),rep(3,4),rep(4,7),rep(5,6),rep(6,6),rep(7,5))
prog_ds.final = sample(prog_stat,size = length(which(studienfach =="Data Science")),
                        replace = TRUE)

#final: 
Interesse_an_Programmieren = rep(0,100)
Interesse_an_Programmieren[which(studienfach=="Mathe")] = prog_mathe.final
Interesse_an_Programmieren[which(studienfach=="Informatik")] = prog_info.final
Interesse_an_Programmieren[which(studienfach=="Data Science")] = prog_ds.final
Interesse_an_Programmieren[which(studienfach=="Statistik")] = prog_stat.final


##Mathe_LK: 
mathe_LK = sample(c("Ja","Nein"),100,TRUE)

#_______________________________________________________________________________

#Dataframe: 

datensatz=data.frame(Alter = alter ,Studienfach = studienfach ,
                     Interesse_an_Mathematik = Interesse_an_Mathematik,
                     Interesse_an_Programmieren = Interesse_an_Programmieren,
                     Mathe_LK = mathe_LK)

write.csv(datensatz,'Datensatz.csv')