# Dove vett n componenti è il vettore contenenti le componenti da combinare. Tipo 1:3 o 2:5 o c(2,3,6,9)
# Dove matrice_corr è la matrice della correlazione tra le variabili e la loro proiezione sulle componenti principali
cerchio_correlazione <- function(matrice_corr, st_dataset, vett_n_componenti){
  posizione <- 1
  for(i in vett_n_componenti){
    # Itero negli elementi che non sono stati ancora combinati
    for(j in vett_n_componenti[-(1:posizione)]){
      plot(matrice_corr[,i], matrice_corr[,j], xlim=c(-1,1), ylim=c(-1,1),asp=1,xlab=paste(i,"° asse", sep = ""),ylab=paste(j,"° asse", sep = ""), main = paste("Correlazione tra variabili e componenti principali",i,"e",j))
      abline(h=0, v=0)
      symbols(0,0, circles = 1, inches =F, add=T)
      text(matrice_corr[,i], matrice_corr[,j], pos=3, labels=colnames(st_dataset),cex=0.8)
      symbols(0, 0, circles=0.8, inches = F, fg= "blue", add=T)
    }
    posizione <- posizione+1
  }
}
# dove matr_proiezione è la matrice in cui ogni vettore ha ora coordinate sulle componenti principali
# dove vett gruppi è un vettore di lunghezza = altezza del dataset; contenente le divisioni in n gruppi tramite cluster o assegnazione manuale. I gruppi iniziano da 1 e vanno fino a n
# Dove vett n componenti è il vettore contenenti le componenti da combinare. Tipo 1:3 o 2:5 o c(2,3,6,9)
# dove titolo è il titolo degli scatter plot
# dove sottotitolo è il sottotitolo
# dove vett_legend è un vettore in cui ci sono i nomi da assegnare ai gruppi nella legenda ordinati
scatter_plot <- function(matr_proiezione, vett_gruppi, vett_n_componenti, dataset,titolo = "", sottotitolo = "", vett_legend = paste("Gruppo", 1:length(table(vett_gruppi)))){
  numero_divisioni = length(table(vett_gruppi))
    posizione <- 1
    for(i in vett_n_componenti){
      # Itero negli elementi che non sono stati ancora combinati
      for(j in vett_n_componenti[-(1:posizione)]){
        # Creo un vettore in cui al posto dell'indice del gruppo ho un numero maggiore di 16 (forme per pch)
        vett_livelli_pch = c(16:(16+numero_divisioni))[vett_gruppi]
        vett_livelli_col = hcl.colors(numero_divisioni, "viridis")[vett_gruppi]
        plot(
          matr_proiezione[,i],
          matr_proiezione[,j],
          xlab=paste(i,"° componente", sep = ""),
          ylab=paste(j,"° componente", sep = ""), 
          pch= vett_livelli_pch, 
          col = vett_livelli_col,
          cex=1.5, 
          main= titolo, 
          sub = sottotitolo
          )
        abline(h=0, v=0)
        legend("bottomright", 
               legend=vett_legend, 
               pch=c(16:(16+numero_divisioni)), 
               col=hcl.colors(numero_divisioni, "viridis")
               )
      }
      posizione <- posizione+1
    }
}


# Imprto il dataframe
olives <- classifly::olives
# Per utilità, ordino il dataset da sud verso nord e poi alla fine aggiungo la sardegna.
# Il processo è tedioso, ma risulta nell'utilizzare la funzione order su un manualmente "ri" numerato factor dell'Area
ordering_olives <- olives$Area
levels(ordering_olives) = c(2,8,7,9,4,1,3,5,6)
olives <- olives[order(as.numeric(as.character(ordering_olives))),]




# Eseguo cluster analysis delle variabili quantitative (Non AREA)
q_olives <- olives
# Rimuovo le colonne non quantitativa
q_olives$Area <- NULL
q_olives$Region <- NULL



# Osservo che la somma per riga della composizione è circa 10000.Per "normalizzare" i dati, poichè si riferiscono alla percentuale di acidi grassi (https://search.r-project.org/CRAN/refmans/pgmm/html/olive.html) in un ordine di grandezza 100 volte superiore (rowSums ~= 10k), abbiamo preso la percentuale di grassi per olio e poi abbiamo ri-moltiplicato 10000 per questo numero. Ottenendo perc_olives.
perc_olives <- 10000*q_olives/rowSums(q_olives)
perc_olives <- cbind(perc_olives, Area =olives$Area)
perc_olives <- cbind(perc_olives, Region =olives$Region)
# Riassegno q_olives
q_olives <- perc_olives
q_olives$Area <- NULL
q_olives$Region <- NULL



#standardizzo le variabili
st_q_olives = scale(q_olives)



##boxplot delle variabili del dataframe
boxplot(st_q_olives, vertical = T, cex.axis=1.4, main ="Box-plot delle valiabili del dataframe")



##matrice della distanza tra le variabili
distanza_var =as.dist(round(cor(st_q_olives),3))
View(as.matrix(distanza_var))
pairs(st_q_olives)



##Analisi in componenti principali
pca_olives = princomp(st_q_olives, cor=T)
pca_olives



##informazioni sulla qualità della rappresentazione
summary(pca_olives)
varCP = pca_olives$sdev^2
percVarCP= varCP/sum(varCP)
qual_rappr=round(rbind (pca_olives$sdev,varCP,percVarCP,cumsum(percVarCP)), 3)
rownames(qual_rappr)= c("Standard deviation","Variance","Proportion of variance", "Cumulative Proportion")



##barplot delle varianze delle componenti principali
plot(pca_olives)



##matrice contenente le correlazioni tra variabili originali e componenti principali
corr_var_comp = round(pca_olives$loadings%*%diag(pca_olives$sdev),3)
#Cambio i colnames
colnames(corr_var_comp) = paste("C",1:8,sep="")
View(corr_var_comp)


##disegno il grafico delle comp 1 e 2
plot(corr_var_comp[,1],corr_var_comp[,2],ylim=c(-1,1), xlim=c(-1,1),asp=1,xlab="primo asse",ylab="secondo asse", main = "Grafico delle variabili")
abline(h=0, v=0)
symbols(0,0, circles = 1, inches =F, add=T)
text(corr_var_comp[,1], corr_var_comp[,2], pos=3, labels=colnames(st_q_olives),cex=0.8)
symbols(0, 0, circles=0.8, inches = F, fg= "blue", add=T)

cerchio_correlazione(corr_var_comp,st_q_olives,1:3)

##fedeltà dela rappresentazione delle singoe variabili sul primo piano fattoriale
qual_12 = corr_var_comp[,1]^2+corr_var_comp[,2]^2
View(qual_12)


##disegno il grafico delle componenti 1 e 3
plot(corr_var_comp[,1],corr_var_comp[,3], xlim=c(-1,1), ylim=c(-1,1),asp=1,xlab="primo asse",ylab="terzo asse", main = "Grafico delle variabili")
abline(h=0, v=0)
symbols(0,0, circles = 1, inches =F, add=T)
text(corr_var_comp[,1], corr_var_comp[,3], pos=3, labels=colnames(st_q_olives),cex=0.8)
symbols(0, 0, circles=0.8, inches = F, fg= "blue", add=T)

##disegno il grafico delle componenti 1 e 3 
plot(corr_var_comp[,2],corr_var_comp[,3], xlim=c(-1,1), ylim=c(-1,1),asp=1,xlab="secondo asse",ylab="terzo asse", main = "Grafico delle variabili")
abline(h=0, v=0)
symbols(0,0, circles = 1, inches =F, add=T)
text(corr_var_comp[,2], corr_var_comp[,3], pos=3, labels=colnames(st_q_olives),cex=0.8)
symbols(0, 0, circles=0.8, inches = F, fg= "blue", add=T)

##grafico delle unità sperimentali per la prima e la seconda componente
plot(pca_olives$scores[,1],pca_olives$scores[,2], xlab="prima componente", ylab="seconda componente", pch=c(16,17,15)[olives$Region], col = c("red", "blue", "darkgreen")[olives$Region], cex=1.5, main= "Grafico gli oli per zona geografica")
abline(h=0, v=0)
legend("bottomright", legend=c("nord","centro","sud"), pch=c(16,17,15), col=c("red","blue", "darkgreen"))

##grafico delle unità sperimentali per la prima e la terza componente
plot(pca_olives$scores[,1],pca_olives$scores[,3], xlab="prima componente", ylab="terza componente", pch=c(16,17,15)[olives$Region], col = c("red", "blue", "darkgreen")[olives$Region], cex=1.5, main= "Grafico gli oli per zona geografica")
abline(h=0, v=0)
legend("bottomright", legend=c("nord","centro","sud"), pch=c(16,17,15), col=c("red","blue", "darkgreen"))

##grafico delle unità sperimentali per la seconda e la terza componente
plot(pca_olives$scores[,2],pca_olives$scores[,3], xlab="seconda componente", ylab="terza componente", pch=c(16,17,15)[olives$Region], col = c("red", "blue", "darkgreen")[olives$Region], cex=1.5, main= "Grafico gli oli per zona geografica")
abline(h=0, v=0)
legend("bottomright", legend=c("nord","centro","sud"), pch=c(16,17,15), col=c("red","blue", "darkgreen"))




