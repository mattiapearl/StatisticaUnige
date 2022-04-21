## 1
# Assegno la working directory
# Ho creato un progetto, quindi la workdir è già corretta.
setwd('C:/Files/Personali/Unige/Code/SD/StatGit/Esercitazione3/')
# Prendi il file adult.txt dalla workdir attuale e portalo
# in un data frame - I valori sotto rispettano il file importato

df_adult <- read.table(
  "../dataset/adult.txt",
  sep = ",",
  na.strings = "?",
  # La prima riga non va letta
  header = F,
  skip = 1,
  ## 2
  col.names = c(
    "age", "workclass", "fnlwgt", "education", "education-num",
    "marital-status", "occupation", "relationship", "race", "sex",
    "capital-gain", "capital-loss", "hours-per-week", "native-country", "income"
  )
)
# Trasformo tutte le variabili in chr a variabili
# in factor ( workclass, education, marital-status...)
# Cambio tutte le variabili chr in factor con un iterazione

# Itero attraverso i nomi di colonna
for (i in colnames(df_adult)) {
  # Se la classe del vettore colonna (uso [[]] per generare un vettore
  # anzi che un dataframe da una singola colonna) è character,
  # cambia quella colonna in fattore
  # print(class(df_adult[[i]])) //DEBUG
  if (class(df_adult[[i]]) == "character") {
    # Cat concatena i valori e li stampa, così posso mettere sia
    # i che la stringa nella stessa linea e ridurre la lunghezza
    cat("Cambio la classe di ", i, " a fattore\n")
    # (Chiamo la colonna come vettore con [[]] anzi che dataframe$nomecolonna
    # - che fa la stessa cosa - e cambio questo vettore in un factor)
    df_adult[[i]] <- as.factor(df_adult[[i]])
  }
}

df_adult_11 <- na.omit(df_adult)

for (i in colnames(df_adult)) {
  cat("\n\n#####\n TDC di ", i, "\n#####\n")
  # Conto le osservazioni per livello
  freq_colonna <- table(df_adult_11[[i]])
  print(freq_colonna)
  # Utilizzo la frequenza per rimuovere le righe con certi attributi
  for (l in levels(df_adult[[i]])) {
    # Prendo ogni livello del fattore
    # Se ce ne sono meno di 10, togli le righe con quelle osservazioni
    # da df_adult_11
    if (freq_colonna[l] < 11) {
      df_adult_11 <- df_adult_11[df_adult_11[[i]] != l, ]
    }
  }
}

##Es2

#a: Visualizza i 5 numeri di Tukey con setting della mediana di Rstudio

tukey <- fivenum(df_adult_11$hours.per.week)
cat("Minimo, Q1, Mediana/Q2, Q3, Massimo:\n", tukey)

#b: costruisci il box-plot della variabile hours.per.week
boxplot(df_adult_11$hours.per.week)