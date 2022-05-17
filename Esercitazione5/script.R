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
        vett_livelli_pch = c(15:(15+numero_divisioni))[vett_gruppi]
        vett_livelli_col = hcl.colors(numero_divisioni, "viridis")[vett_gruppi]
        plot(
          matr_proiezione[,i],
          matr_proiezione[,j],
          xlab=paste(i,"° componente", sep = ""),
          ylab=paste(j,"° componente", sep = ""), 
          pch= vett_livelli_pch, 
          col = vett_livelli_col,
          bg = vett_livelli_col, 
          main= titolo, 
          sub = sottotitolo,
          )
        abline(h=0, v=0)
        legend("bottomright", 
               legend=vett_legend, 
               pch=c(15:(15+numero_divisioni)), 
              pt.bg = hcl.colors(numero_divisioni, "viridis"),
               col=hcl.colors(numero_divisioni, "viridis")
               )
      }
      posizione <- posizione+1
    }
}

best_rappr <-  function(matrice_corr){
  posizione <- 1
  posizione_in_matr <- 1
  tot_colonne <- sum(1:(ncol(matrice_corr)-1))+2
  matrice_qual_rappr <- matrix(0, 
                               nrow(matrice_corr),
                               tot_colonne
                               )
  
  rownames(matrice_qual_rappr) = rownames(matrice_corr) 
  colnames(matrice_qual_rappr) = 1:tot_colonne
  colnames(matrice_qual_rappr)[tot_colonne-1] = "Top rappr."
  colnames(matrice_qual_rappr)[tot_colonne] = "Valore Top"
  for(i in 1:ncol(matrice_corr)){
    # Itero negli elementi che non sono stati ancora 

      for(j in (1:ncol(matrice_corr))[-(1:posizione)]){
        qual = round(sqrt(matrice_corr[,i]^2+matrice_corr[,j]^2),3)
        matrice_qual_rappr[,posizione_in_matr] = qual
        nome_colonna <- paste("Comp",i,"e",j)
        #Trova quale sia il migliore
        for(k in 1:length(qual)){
          qual_riga = qual[k]
          max_riga = matrice_qual_rappr[k,tot_colonne]
          if(qual_riga>max_riga){
            matrice_qual_rappr[k,tot_colonne] = qual[k]
            matrice_qual_rappr[k,tot_colonne-1] = nome_colonna
          }
        }
        
        colnames(matrice_qual_rappr)[posizione_in_matr] = nome_colonna
        posizione_in_matr <- posizione_in_matr+1
      }

    posizione <- posizione+1
  }
  return(matrice_qual_rappr)
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


##disegno il grafico delle componenti da 1 a 3
cerchio_correlazione(corr_var_comp,st_q_olives,1:3)

##fedeltà dela rappresentazione delle singoe variabili sul primo piano fattoriale
qual_12 = sqrt(corr_var_comp[,1]^2+corr_var_comp[,2]^2)
View(qual_12)
qual_13 = corr_var_comp[,1]^2+corr_var_comp[,3]^2
View(qual_13)
qual_23 = corr_var_comp[,2]^2+corr_var_comp[,3]^2
View(qual_23)



##grafico delle unità sperimentali per le componenti secondo il raggruppamento in zone
vett_gruppi = c(1,2,3)[olives$Region]
scatter_plot(pca_olives$scores, vett_gruppi, 1:3, st_q_olives, vett_legend = c("Sud","Sardegna","Nord"))


##grafico delle unità sperimentali per le componenti secondo il raggruppamento in regioni
vett_gruppi = c(2,8,7,9,4,1,3,5,6)[olives$Area]
scatter_plot(pca_olives$scores, vett_gruppi, 1:3, st_q_olives, vett_legend = c("Sicilia", "Calabria","Sud Puglia", "Nord Puglia", "Umbria","Liguria Ovest", "Liguria Est", "Costa Sarda","Entroterra Sardo"))

             