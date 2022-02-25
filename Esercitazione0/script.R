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
education_rec <- ordered(x = dfAdult$education, levels =c("Preschool","1st-4th","5th-6th","9th","10th","11th","12th","HS-grad","Prof-school","Assoc-acdm","Assoc-voc","Some-college","Bachelors","Masters","Doctorate"))
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


##9
#(TABELLE)


#Definisco una funzione per "pulizia"

#FUNZIONE:
#ARGUMENTS: vettore - un vettore
#RETURNS: void
#UTILIZZO: Prende un vettore variabile (factor, a più livelli), se stampa una tabella di contingenza per la freq assoluta, quella relativa (e quella rel percentuale), e stampa un diagramma a barre (non cambia nulla tra i tipi di dati in quanto separati da una costante coumne)
printAbsRelHist <-  function (vettore){
  aWcTable <- table(vettore)
  print(vettore)
  #per contare la popolazione, osservo quale sia la lunghezza del vettore (freq rel = freq ass / popolazione)
  rWcTable <- aWcTable / length(vettore) 
  print(rWcTable)
  # Per sfizio la stampo anche in percentuale
  percWcTable <-  rWcTable * 100
  print(percWcTable)
  
  #Stampo l'istogramma del dataset dato
  barplot(aWcTable, xlab = "Classe lavorativa", ylab = "Numero di rilevazioni",las=2)
  }

#In questo caso in particolare seguo un processo più tedioso per workclass, mentre itero per stampare le altre
#Osservo la variabile workclass (dalla tabella completa, in quanto na-omittendo rimuovo quelli senza capital gainis - ovvero chi non lavora, eliminando un importante dato dalla frequenza: i disoccupati)
levels(dfAdult$workclass)
#Tabella con frequenza assoluta (PER OTTENERE UNA PERCENTUALE CHE ARRIVA A 100 DEVO OMETTERE I VALORI NON PRESENTI)
naOmittedWc <-  na.omit(dfAdult$workclass)
#Caso workclass
printAbsRelHist(naOmittedWc)

#Restanti casi
for(i in colnames(qDfAdult)){
  #Se non è workclass, che abbiamo già plottato
  if(i != "workclass"){
    printAbsRelHist(qDfAdult[[i]])
  }
}
