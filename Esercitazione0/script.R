##1
# Assegno la working directory - Ho creato un progetto, quindi la workdir è già corretta. Aggiungo pseudocodice esplicativo sotto
setwd("C:/Files/Personali/Unige/Code/SD/GruppoStat/Esercitazione0")
# Prendi il file adult.txt dalla workdir attuale e portalo in un data frame - I valori sotto rispettano il file importato
dfAdult = read.table(
    "adult.txt", 
    sep = ",",
    na.strings="?",
    # La prima riga non va letta
    header = F,
    skip = 1,
##2
    col.names = c("age", "workclass", "fnlwgt", "education","education-num","marital-status", "occupation", "relationship", "race", "sex", "capital-gain","capital-loss", "hours-per-week","native-country","income")
    )

##3
# Studio la dimensione del dataframe
dim = dim(dfAdult) # RxC
#La stampo a schermo
cat(dim, "(Righe Colonne)")

##4
#Studio la struttura del dataframe prima di convertirle in factor
str(dfAdult)
#Trasformo tutte le variabili in chr a variabili in factor ( workclass, education, marital-status...)
#Cambio tutte le variabili chr in factor con un iterazione

#Itero attraverso i nomi di colonna
for (i in colnames(dfAdult)) {
# Se la classe del vettore colonna (uso [[]] per generare un vettore anzi che un dataframe da una singola colonna) è character, cambia quella colonna in fattore  
  # print(class(dfAdult[[i]])) //DEBUG
  if(class(dfAdult[[i]]) == "character"){
    #Cat concatena i valori e li stampa, così posso mettere sia i che la stringa nella stessa linea e ridurre la lunghezza 
    cat("Cambio la classe di ", i ," a fattore\n")
    # (Chiamo la colonna come vettore con [[]] anzi che dataframe$nomecolonna - che fa la stessa cosa - e cambio questo vettore in un factor) 
    dfAdult[[i]] = as.factor(dfAdult[[i]])
  }
}


##5
#Stampo le prime 10 righe
head(dfAdult,10)
#Visualizzo il dataframe
View(dfAdult)


##6
#Visualizzo i livelli (gli elementi distinti) di education
levels(dfAdult[["education"]])
# analogo: levels(dfAdult$education)
#L'ordine è alfabetico


##7
#Creo un fattore ordinato per l'educazione
education_rec <- ordered(x = dfAdult$education, levels =c("Preschool","1st-4th","5th-6th","7th-8th","9th","10th","11th","12th","HS-grad","Prof-school","Assoc-acdm","Assoc-voc","Some-college","Bachelors","Masters","Doctorate"))
#Non ho capito se serva sostituirlo ad education, ma in caso ecco quì:
# dfAdult$education = education_rec


##8
#Costruisco il nuovo dataframe con solo variabili qualitative usando il ciclo usato in precedenza
# Lo faccio partendo da quello iniziale
qDfAdult = na.omit(dfAdult)
#Itero e rimuovo le colonne che hanno classe int

keepVect <- c()

for (i in colnames(qDfAdult)) {
  if(class(qDfAdult[[i]]) != "integer"){
    #Aggiungo tutte queste colonne a un vettore
     keepVect <- append(keepVect,i)
  }
}
#Ora creo un subset del dataframe qualitativo senza le colonne quantitative
#Devo usare questo metodo e non subset perchè colnames mi da una stringa, quindi il vettore è composto di stringhe - mentre subset vuole il valore "non in stringa"
qDfAdult <- qDfAdult[keepVect]
#Lo rinomino fattori e visualizzo le prime righe
fattori <- qDfAdult
head(fattori, 10)

##9
#(TABELLE)


#Definisco una funzione per "pulizia"

#FUNZIONE:
#ARGUMENTS: vettore - un vettore, nomeX - nome da dare alla coordinata x, il nome della colonna del dataframe va bene
#RETURNS: void
#UTILIZZO: Prende un vettore variabile (factor, a più livelli), se stampa una tabella di contingenza per la freq assoluta, quella relativa (e quella rel percentuale), e stampa un diagramma a barre (non cambia nulla tra i tipi di dati in quanto separati da una costante coumne)
printAbsRelHist <-  function (vettore, nomeX){
  aWcTable <- table(vettore)
  #Relativo come "proportionate to the rest of the table": prop.table
  rWcTable <- prop.table(aWcTable)
  #Affianco le tabelle per colonna (prendo la freq relativa e quella rel perc con il round e il *100)
  totalTable <- cbind(aWcTable,round(rWcTable,4),round(rWcTable*100,2))
  #Nomi colonne
  colnames(totalTable) <- c("Freq Assoluta", "Relativa", "Pecentuale")
  #Stampa un "divisore". Il \n appended alla fine serve a mandare il cursore a nuova linea in cat. Con writelines non ci sarebbe il problema, con print non si potrebbe stampare su più righe
  cat("\n\n######## \n", nomeX,"\n########")
  totalTable
  #Stampo l'istogramma del dataset dato
  barplot(aWcTable, xlab = nomeX, ylab = "Numero di rilevazioni",las=2)
  }


#In questo caso in particolare seguo un processo più tedioso per workclass, mentre itero per stampare le altre
#Osservo la variabile workclass (dalla tabella completa, in quanto na-omittendo rimuovo quelli senza capital gainis - ovvero chi non lavora, eliminando un importante dato dalla frequenza: i disoccupati)
levels(dfAdult$workclass)
#Tabella con frequenza assoluta (PER OTTENERE UNA PERCENTUALE CHE ARRIVA A 100 DEVO OMETTERE I VALORI NON PRESENTI)
naOmittedWc <-  na.omit(dfAdult$workclass)
#Caso workclass
printAbsRelHist(naOmittedWc, "workclass")

#Restanti casi
for(i in colnames(qDfAdult)){
  #Se non è workclass, che abbiamo già plottato
  if(i != "workclass"){
    printAbsRelHist(qDfAdult[[i]], i)
  }
}

##10
#Tabella a due vie
#Prendo il workclass originario affinchè abbia la stessa lunghezza di education rec
vcErTable <- table(dfAdult$workclass,education_rec)
cat("\n\n######## \n Workclass e Education\n########", "\n")
print(vcErTable)

##11
#Anzi che ordinare secondo workclass che non è ordinata, ordino secondo education_rec
orderedAdult <- cbind(dfAdult, education_rec)
orderedAdult <- with(orderedAdult, orderedAdult[order(education_rec),])
#Stampo
cat("\n\n######## \n Primi 5 risultati\n########", "\n")
head(orderedAdult, 5)
cat("\n\n######## \n Ultimi 5 risultati\n########", "\n")
tail(orderedAdult, 5)


##12
#Filtro il dataframe per male e female
mOrderedAdult <- orderedAdult[orderedAdult$sex == "Male",]  
fOrderedAdult <- orderedAdult[orderedAdult$sex == "Female",]  
#Li concateno per riga (Hanno lo stesso numero di colonne)

mfOrderedAdult <- rbind(mOrderedAdult,fOrderedAdult)
View(mfOrderedAdult)
print("Dimensioni del dataframe Male")
dim(mOrderedAdult)
print("Dimensioni del dataframe Female")
dim(fOrderedAdult)
   