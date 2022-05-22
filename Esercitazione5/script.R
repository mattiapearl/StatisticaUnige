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
      
      par(xpd=FALSE)
      # Creo un vettore in cui al posto dell'indice del gruppo ho un numero maggiore di 16 (forme per pch)
      vett_livelli_pch = c(15:(15+numero_divisioni))[vett_gruppi]
      vett_livelli_col = hcl.colors(numero_divisioni, "viridis")[vett_gruppi]
      plot(
        matr_proiezione[,i],
        matr_proiezione[,j],
        xlab=paste(i,"° componente", sep = ""),
        ylab=paste(j,"° componente", sep = ""),
        xlim = c(-5,5),
        ylim = c(-5,5),
        pch= vett_livelli_pch,
        col = vett_livelli_col,
        bg = vett_livelli_col,
        main= titolo,
        sub = sottotitolo,
        cex = 1.1
      )
      abline(h=0, v=0)
      par(xpd=TRUE)
      legend("bottomright",
             inset = c(-0.05,0),
             cex = 0.7,
             legend=vett_legend,
             pch=c(15:(15+numero_divisioni)),
             pt.bg = hcl.colors(numero_divisioni, "viridis"),
             col=hcl.colors(numero_divisioni, "viridis"),
             legend.sta )
    }
    posizione <- posizione+1
  }
}



# Funzione che fornisce informazioni su Area e Region di una serie di gruppi data una cluster di separazione
grup_reg_area <-function(dataset, gruppo, matrice_sep, clear= FALSE){
  
  
  count_dataset <- cbind(dataset,Area = dataset$Area ,Region = dataset$Region )
  # Ho bisogno di assegnare i livelli a questa variabile per avere una tabella con 0 osservazioni e poterla dividere in gruppo_plot_reg_area
  count_dataset$Region <- as.factor(count_dataset$Region)
  levels(count_dataset$Region) <- c("Sud-Italia","Sardegna","Nord-Italia")
  count_dataset <- count_dataset[matrice_sep == gruppo,]
  #Rimuovo i livelli inutilizzati
  if(clear){
    count_dataset$Area <- droplevels(count_dataset$Area)
    count_dataset$Region <- droplevels(as.factor(count_dataset$Region))
  }
  
  return(list(table(count_dataset$Area),table(count_dataset$Region), 100*table(count_dataset$Area)/sum(table(count_dataset$Area)), 100*table(count_dataset$Region)/sum(table(count_dataset$Region))))
}



#Funzione che permette di graficare visivamente la "Quantità" di Area e Region contenute nei vari gruppi di una cluster
gruppo_plot_reg_area <- function(dataset, matrice_sep, subtitle){
  par(mfrow= c(2,1), bg = "#ffffff", mar = c(6,2.5,2,2.5), cex.lab =0.5)
  
  num_classi <- length(table(matrice_sep))
  
  for(i in 1:num_classi){
    
    tabelle_reg_zona <- grup_reg_area(dataset,i,matrice_sep)
    table_reg_gruppo <- tabelle_reg_zona[[1]]
    table_zona_gruppo <- tabelle_reg_zona[[2]]
    
    
    # Base barplot "Area"/Regione
    barplot(table(dataset$Area), axes = FALSE, ylim=c(0,250) , xaxt='n', ann=FALSE)
    abline(h = 0, lty = 2, col = "black")
    abline(h = 50, lty = 2, col = "grey")
    abline(h = 100, lty = 2, col = "grey")
    abline(h = 150, lty = 2, col = "grey")
    abline(h = 200, lty = 2, col = "grey")
    abline(h = 250, lty = 2, col = "grey")
    barplot(table(dataset$Area), las = 2, col ="#09057265",ylim = c(0,250), add = TRUE )
    
    final_plot_r <- barplot(
      table_reg_gruppo,
      col =hcl.colors(9,palette ="ag_sunset"),
      ylim = c(0,250),
      add = TRUE,
      xaxt='n',
      yaxt="n",
      ann=FALSE,
      ylab="n° elementi",
    )
    text(final_plot_r, table(dataset$Area)+20, labels = paste(round((table_reg_gruppo/table(dataset$Area))*100, 0),"%"))
    
    title(main = paste("Gruppo ",i))
    
    
    # Base barplot "Region"/Zona
    barplot(table(dataset$Region), axes = FALSE, ylim=c(0,572), xaxt='n', ann=FALSE )
    abline(h = 0, lty = 2, col = "black")
    abline(h = 100, lty = 2, col = "grey")
    abline(h = 200, lty = 2, col = "grey")
    abline(h = 300, lty = 2, col = "grey")
    abline(h = 400, lty = 2, col = "grey")
    abline(h = 500, lty = 2, col = "grey")
    tablR <- table(dataset$Region)
    rownames(tablR) = c("Sud-Italia", "Sardegna","Nord-Italia")
    barplot(tablR, col ="#09057265",ylim = c(0,572), add = TRUE )
    
    final_plot_z <- barplot(
      table_zona_gruppo,
      col =hcl.colors(4,palette ="Sunset"),
      ylim = c(0,572),
      add = TRUE,
      xaxt='n',
      yaxt="n",
      ann=FALSE,
      ylab="n° elementi",
    )
    text(final_plot_z, table(dataset$Region)+50, labels = paste(round((table_zona_gruppo/table(dataset$Region))*100, 1),"%"))
    title( sub = paste(
      subtitle,
      " - n° Gruppi: ",
      num_classi,
      " - numerosità gruppo corrente: ",
      sum(table_reg_gruppo),
      " su ", nrow(dataset))
    )
  }
}



best_rappr <- function(matrice_corr){
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
View(distanza_var)
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
plot(pca_olives, col= hcl.colors(8, "plasma"),title(main=NULL))
title(main="Varianza per componenti principali")



##matrice contenente le correlazioni tra variabili originali e componenti principali
corr_var_comp = round(pca_olives$loadings%*%diag(pca_olives$sdev),3)
##disegno il grafico delle componenti da 1 a 3
cerchio_correlazione(corr_var_comp,st_q_olives,1:3)



## matrice fedeltà rappresentazione
View(best_rappr(corr_var_comp))




##grafico delle unità sperimentali per le componenti secondo il raggruppamento in zone
vett_gruppi = c(1,2,3)[olives$Region]
scatter_plot(pca_olives$scores, vett_gruppi, 1:3, st_q_olives, titolo = "Scatter plot rispetto alle componenti per zona",vett_legend = c("Sud","Sardegna","Nord"))




##grafico delle unità sperimentali per le componenti secondo il raggruppamento in regioni
vett_gruppi = c(2,8,7,9,4,1,3,5,6)[olives$Area]
scatter_plot(pca_olives$scores, vett_gruppi, 1:3, st_q_olives, titolo = "Scatter plot rispetto alle componenti per regioni", vett_legend= c("Sicilia", "Calabria","Sud Puglia", "Nord Puglia", "Umbria","Liguria Ovest", "Liguria Est", "Costa Sarda","Entroterra Sardo"))



##aggregazione non gerarchica
aggreg_kmeans9= kmeans(st_q_olives, center= 9, nstart= 20)
# Riordino le variabili (della specifica cluster) rispetto alla cluster precedente
aggreg_kmeans9$cluster = c(4,2,7,9,8,6,3,1,5)[aggreg_kmeans9$cluster]
gruppo_plot_reg_area(perc_olives, aggreg_kmeans9$cluster,"Kmeans - 9")
par(mfrow = c(1,1),mar=c(4,4,2.5,2.5))
scatter_plot(pca_olives$scores, aggreg_kmeans9$cluster, 1:3, st_q_olives,titolo = "Scatter plot rispetto alle componenti per Kmeans9")




