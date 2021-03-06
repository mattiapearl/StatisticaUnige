# Includo la libreria
library(classifly)

# Funzione che stampa a schermo un immagine a partire dalla matrice delle distanze date
dist_image <- function(matr, title, subtitle){
  par(mfrow = c(1,1), mar= c(7,2.5,2.5,4) )
  # Specchio la matrice per visualizzarla
  image(matr[,c(572:1)], axes= FALSE)
  # Linee verticali (spesse al cambio area)
  segments(36/572,0,36/572,572/572, lwd = 1)
  segments(92/572,0,92/572,572/572, lwd = 1)
  segments(298/572,0,298/572,572/572, lwd = 1)
  segments(323/572,0,323/572,572/572, lwd = 2)
  segments(374/572,0,374/572,572/572, lwd = 1)
  segments(424/572,0,424/572,572/572, lwd = 1)
  segments(474/572,0,474/572,572/572, lwd = 2)
  segments(507/572,0,507/572,572/572, lwd = 1)
  # Linee orizzontali
  segments(0,1-36/572,1,1-36/572, lwd = 1)
  segments(0,1-92/572,1,1-92/572, lwd = 1)
  segments(0,1-298/572,1,1-298/572, lwd = 1)
  segments(0,1-323/572,1,1-323/572, lwd = 2)
  segments(0,1-374/572,1,1-374/572, lwd = 1)
  segments(0,1-424/572,1,1-424/572, lwd = 1)
  segments(0,1-474/572,1,1-474/572, lwd = 2)
  segments(0,1-507/572,1,1-507/572,lwd = 1)
  title(main= title, sub = subtitle)
}

# Funzione che prende un vettore e restituisce un vettore della stessa lunghezza in formato rgb che evidenzia con colori più scuri i valori più vicini al massimo fornito
yellow_rgber <- function(vettore, maxi){
  vettore <- (1-abs(vettore/maxi))^2
  result <- c()
  for(i in vettore){
    red <-  (1 )
    green <- (0.4 )
    blue <- (0.1 )
    result <- c(result, rgb(red,green,blue, alpha = 1 * (1-i)))
  }
  return(result)
}

# Funzione che data una cluster e il numero di classi a cui "tagliare" fornisce informazioni utili, come le distanze interne, le inerzie, e l'indice di bontà
info_cluster <-  function(dataset_std, cluster, numclassi){
  dataset_std <- as.data.frame(dataset_std)
  dist_int_medie = c(1:numclassi); dist_int_max= c(1:numclassi); inerzie_int_medie = c(1:numclassi)
  gruppi = cutree(cluster, k=numclassi)
  
  for(i in 1:numclassi){
    a <- rowSums(scale(dataset_std[gruppi==i,],scale = F)^2)
    dist_int_medie[i] = mean(sqrt(a))
    dist_int_max[i] = max(sqrt(a))
    inerzie_int_medie[i] = mean(a)
  }
  compattezza = cbind(table(gruppi),dist_int_medie, dist_int_max,inerzie_int_medie)
  colnames(compattezza)[1] = "n"
  inerzia_tot = (dim(dataset_std)[1]-1)*dim(dataset_std)[2]
  inerzia_int = sum(inerzie_int_medie*table(gruppi))
  percent_inerzia_fra= round((inerzia_tot - inerzia_int)/inerzia_tot*100,2)
  return(list(compattezza,inerzia_tot,inerzia_int,percent_inerzia_fra))
}

# Funzione che data una matrice di deviazione costruisce un grafico di bell'aspetto per ogni riga
plot_dev_baricentro <- function(matr_deviaz, subtitle = ""){
  par(mfrow = c(1,1), mar= c(10,2.5,5,2.5))
  
  n_blocchi <- nrow(matr_deviaz)
  
  for(i in 1:n_blocchi){
    barplot(matr_deviaz[i,], axes = FALSE, ylim=c(-2,2) , xaxt='n', ann=FALSE, col= rgb(0.9,0.4,0,0.3))
    abline(h = -2:2, lty = 2, col = "grey")
    abline(h = 0, lty = 2, col = "#333333")
    title(main= paste("Deviazione dal baricentro: ", rownames(matr_deviaz)[i]), sub= subtitle)
    barplot(matr_deviaz[i,], las = 2,ylim = c(-2,2) , col = yellow_rgber(matr_deviaz[i,],2), add = TRUE)
    
  }
  
}

# Funzione che mostra su un immagine analoga a quella delle distanze la posizone (con un colore scuro) dei ogni gruppo in questione
highlight_gruppo <- function(q_dataset,gruppo, matrice_sep, palette, metodo){
  par(mfrow = c(1,1), mar = c(5,2.5,2.5,2.5))
  
  dataset_col = q_dataset
  dataset_col[matrice_sep == gruppo,] = 0
  # Il colore scuro deve rappresentare la classe, quindi il valore massimo si deve avere dove non si ha la classe
  dataset_col[matrice_sep != gruppo,] = 1
  image(as.matrix(dataset_col), axes = FALSE, col = palette)
  title(main = paste("Elementi su immagine di: Gruppo ",gruppo), sub = paste("Metodo: ",metodo))
  return(dataset_col)
}

# Funzione che fornisce informazioni su Area e Region di una serie di gruppi data una cluster di separazione
grup_reg_area <-function(dataset, gruppo, matrice_sep, clear= FALSE){
  
  
  count_dataset <-  cbind(dataset,Area = dataset$Area  ,Region = dataset$Region )
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
    
    final_plot_r <-  barplot(
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

    final_plot_z <-  barplot(
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

# Funzione che grafica per ogni proprietà qualitativa disponibile, per ogni gruppo della separazione di una cluster, i boxplot riferiti a quelle variabili
gruppo_box_propriet <- function(q_dataset, divisione_gruppi, propriet,subtitle){
  par(mar=c(7,2.5,2,2.5))
  q_dataset = as.data.frame(q_dataset)
  num_classi = length(table(divisione_gruppi))
  # Colorazione
  colors_d = hcl.colors(num_classi+2,"Sunset")
  # Creo una lista vuota per attaccarci gli elementi
  lista_dati_plot = list()
  lista_dati_plot[[1]] = q_dataset[[propriet]]
  means <- c(mean(q_dataset[[propriet]]))
  names(lista_dati_plot)[1] = "Dataset"
  for(i in 1:num_classi){
    current_group <- q_dataset[divisione_gruppi==i,]
    print(current_group)
    lista_dati_plot[[i+1]] = current_group[[propriet]]
    # Cambio nome
    names(lista_dati_plot)[i+1] = paste("Gruppo ", i)
    # Aggiungo media al vettore per stamparlo tra poco
    means <- cbind(means, mean(current_group[[propriet]]))
  }
  boxplot(lista_dati_plot, horizontal = TRUE, col = colors_d, border = "#333333", outcol =colors_d, main = paste("Boxplot per gruppi di: ", propriet), sub= paste(subtitle," | n° Gruppi:",num_classi ), cex= 0.8 )
  # Stampa le linee di media
  for(i in 1:length(means)){
    # Punti
    points(y = i,                             
           x = means[i],
           col="white",
           pch = 16)
    # Contorni
    points(y = i,                             
           x = means[i],
           col="black",
           pch = 21)
  }
}


###########
# RUNTIME #
###########

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

# Calcolo il baricentro dei dati per interesse
baricentro <- round(rowSums(t(q_olives))/572,2)

## Cluster delle variabili
par(mfrow=c(1,1))
# Inizialmente creo una matrice della distanza utilizzando 1-rho^2
matr_dist_corr <- 1-cor(q_olives)^2
matr_corr_var <- cor(q_olives)
# Immagine delle distanze. Specchiata per fare in modo che rispecchi la matrice data, con colori abbastanza "Profondi" (100 unità) per vedere variazione di distanze standardizzate
image(1-matr_dist_corr[,c(8:1)], axes = FALSE, col=hcl.colors(100,"YlOrRd"))

#Per la cluster scelgo il metodo di ward
aggregazione_var <- hclust(as.dist(matr_dist_corr),method = "ward.D" )
par(mar = c(5,2.5,2.5,2)+0.1)
plot(aggregazione_var, hang= -0.1, frame.plot= TRUE, main="Aggregazione variabili",sub = "Distanza utilizzata: 1-rho^2 - Metodo di aggregazione: Ward", xlab ="", ylab="Indice di aggregazione")
num_classi = 3
rect.hclust(aggregazione_var, k=num_classi, border = "purple")

### Cluster unità sperimentali
matrice_osservazioni = as.matrix(q_olives)

## Parto da cluster gerarchiche
#Standardizzo (per righe)
matrice_osservazioni = scale (matrice_osservazioni)

# Calcolo le varie distanze (per righe) e le trasformo in matrici

ed1_o = as.matrix(dist(matrice_osservazioni, method = "euclidean") , nrow= 572)
edM_o = as.matrix(dist(matrice_osservazioni, method = "maximum") , nrow= 572)
edm_o = as.matrix(dist(matrice_osservazioni, method = "manhattan") , nrow= 572)

dist_image(ed1_o, "Immagine distanza", "Distanza: Euclidea")
dist_image(edM_o, "Immagine distanza", "Distanza: Massimo")
dist_image(edm_o, "Immagine distanza", "Distanza: Manhattan")

st_q_olives = scale(q_olives)
# Osservando le tre possibili distance, sembra che la distanza euclidea sia quella che prioritizza la differenziazione per regione. Questo vuol dire che c'è un elemento (una coordinata) degli oli che è più caratterizzata dalla regione di altre - infatti la distanza del massimo non la evidenzia mentre la manhattan si (che è una versione meno "geometrica" della distanza euclidea).
# Guardo quindi cosa devi per regione dal baricentro:
baric_reg1 <-  colMeans(st_q_olives[perc_olives$Region == 1,])# Sum somma i valori True, ovvero conta le righe prese in considerazione. Il resto è per i calcoli dei baricentri 
baric_reg2 <-   colMeans(st_q_olives[perc_olives$Region == 2,])
baric_reg3 <- colMeans(st_q_olives[perc_olives$Region == 3,])

dist_reg_non_cluster <-matrix(c( baric_reg1 , baric_reg2 , baric_reg3 ), ncol = 8, byrow = TRUE) # Sono standardizzate, quindi il baricentro sarebbe 0
colnames(dist_reg_non_cluster) <-  colnames(q_olives)
rownames(dist_reg_non_cluster) <-  c("Sud Italia", "Sardegna", "Nord Italia")
# TABELLA BELLA
View(dist_reg_non_cluster)

#Barplot di deviazione dal baricentro
plot_dev_baricentro(dist_reg_non_cluster)
# Sembra che in tutta italia sia l'acido oleico

#@ Studio la matrice ed1_o (i.e: euclidean distance ^ 1 _ olives)
# Già dalla matrice delle distanze si può osservare una forte similitudine tra oli della stessa regione. La puglia del nord sembra più uniformata all'Umbria e alla Liguria che all'Italia del Sud, quindi è supponibile che verrà aggregata durante la cluster.
# Si possono inoltre intersecare le coordinate per osservare quali siano le caselle più chiare, come nel caso di Umbria e ovest della liguria. La Sardegna ha un simile comportamento rendendo la divisione tra costa e entroterra più qualitativo che necessario. 
# Infine è interessante osservare che il nord della puglia abbia una distanza mediamente alta con il sud della puglia, mentre sia piò intersecabile con regioni come la sicilia o l'ovest ligure.

#La frazione più compatta (distanza interna) sembrano essere l'umbria e la Sardegna, invece Sicilia e Ovest della Liguria sono quelle con i punti più scuri

# L'analisi appena fatta è un tipo possibile di cluster, meno quantitativo e basato solo sulla supposizione che regioni affini abbiano alberi dello stesso tipo, quindi oli con grassi simili.

## Cluster quantitativo
##aggregazione gerarchica con distanza euclidea e metodo di ward, single linkage, complete linkage, average linkage 
aggreg_w2eucl = hclust(as.dist(ed1_o), method ="ward.D2")
aggreg_seucl= hclust(as.dist(ed1_o), method = "single")
aggreg_compleucl = hclust(as.dist(ed1_o), method ="complete")
aggreg_avereucl = hclust(as.dist(ed1_o), method ="average")

##aggregazione gerarchica con distanza del massimo e metodo di ward, single linkage, complete linkage, average linkage 
aggreg_w2max = hclust(as.dist(edM_o), method ="ward.D2")
aggreg_smax = hclust(as.dist(edM_o), method ="single")
aggreg_complmax = hclust(as.dist(edM_o), method ="complete")
aggreg_avermax = hclust(as.dist(edM_o), method ="average")

##aggregazione gerarchica con distanza del massimo e metodo di ward, single linkage, complete linkage, average linkage 
aggreg_w2man =hclust(as.dist(edm_o), method ="ward.D2")
aggreg_sman = hclust(as.dist(edm_o), method ="single")
aggreg_complman = hclust(as.dist(edm_o), method ="complete")
aggreg_averman = hclust(as.dist(edm_o), method ="average")

##creo i dendogrammi delle aggregazioni con il metodo del ward linkage
par(mfrow=c(3,1), mar = c(2.5,2.5,2.5,2.5))
plot(aggreg_w2eucl, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Euclidea - Ward linkage",info_cluster(st_q_olives,aggreg_w2eucl,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels =FALSE)
##dall'analisi degli indici di bontà la cluster migliore risulta essere l'aggregazione con la distanza euclidea e il metodo di ward
###divido le unità sperimentali in tre classi e le visualizzo sul dendogramma 
num_clust =3
gruppi_euclw = cutree(aggreg_w2eucl, k=num_clust)
rect.hclust(aggreg_w2eucl, k=num_clust, border= "purple")
plot(aggreg_w2max, hang= -0.1, frame.plot= TRUE, main=paste("Distanza del massimo - Ward linkage",info_cluster(st_q_olives,aggreg_w2max,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_w2man, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Manhattan - Ward linkage: ",info_cluster(st_q_olives,aggreg_w2man,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)

##creo i dendogrammi delle aggregazioni con il metodo single linkage  
plot(aggreg_seucl, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Euclidea - Single linkage",info_cluster(st_q_olives,aggreg_seucl,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_smax, hang= -0.1, frame.plot= TRUE, main=paste("Distanza del massimo - Single linkage",info_cluster(st_q_olives,aggreg_smax,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_sman, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Manhattan - Single linkage",info_cluster(st_q_olives,aggreg_sman,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)

##creo i dendogrammi delle aggregazioni con il metodo complete linkage
plot(aggreg_compleucl, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Euclidea - Complete linkage",info_cluster(st_q_olives,aggreg_compleucl,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_complmax, hang= -0.1, frame.plot= TRUE, main=paste("Distanza del massimo - Complete linkage",info_cluster(st_q_olives,aggreg_complmax,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_complman, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Manhattan - Complete linkage",info_cluster(st_q_olives,aggreg_complman,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)

##creo i dendogrammi delle aggregazioni con il metodo dell'average linkage
plot(aggreg_avereucl, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Euclidea - Average linkage",info_cluster(st_q_olives,aggreg_avereucl,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_avermax, hang= -0.1, frame.plot= TRUE, main=paste("Distanza del massimo - Average linkage",info_cluster(st_q_olives,aggreg_avermax,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)
plot(aggreg_averman, hang= -0.1, frame.plot= TRUE, main=paste("Distanza Manhattan - Average linkage",info_cluster(st_q_olives,aggreg_averman,3)[[4]]), xlab ="", ylab="Indice di aggregazione", labels=FALSE)

##aggregazione non gerarchica
aggreg_kmeans3= kmeans(st_q_olives, center= 3, nstart= 20)
aggreg_kmeans9= kmeans(st_q_olives, center= 9, nstart= 20)

# Coloro delle immagini 572x572 rispetto alle divisioni in classi interessanti per poi sovrapporle alle precedenti immagini delle distanze.
par(mfrow = c(1,1))
highlight_gruppo(q_olives,1, cutree(aggreg_w2eucl,k=3), hcl.colors(2, "RdPu"), "Ward, Euclidea (3 gruppi)")
highlight_gruppo(q_olives,2, cutree(aggreg_w2eucl,k=3), hcl.colors(2, "RdPu"), "Ward, Euclidea (3 gruppi)")
highlight_gruppo(q_olives,3, cutree(aggreg_w2eucl,k=3), hcl.colors(2, "RdPu"), "Ward, Euclidea (3 gruppi)")
# Divisioni in classi per kmeans3
highlight_gruppo(q_olives,1, aggreg_kmeans3$cluster, hcl.colors(2, "RdPu"), "K-Means (3 gruppi)")
highlight_gruppo(q_olives,2, aggreg_kmeans3$cluster, hcl.colors(2, "RdPu"),"K-Means (3 gruppi)")
highlight_gruppo(q_olives,3, aggreg_kmeans3$cluster, hcl.colors(2, "RdPu"),"K-Means (3 gruppi)")
# Divisioni in classi per kmeans9
par(mfrow=c(3,3))
highlight_gruppo(q_olives,1, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,2, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,3, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,4, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,5, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,6, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,7, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,8, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")
highlight_gruppo(q_olives,9, aggreg_kmeans9$cluster, hcl.colors(2, "RdPu"), "K-Means (9 gruppi)")


## Studio della struttura dei gruppi dati (method: w2 con euclidea ; 3 gruppi)
# Serie di barplot rappresentanti la presenza dei dati elementi nei vari gruppi rispetto al gruppo principale



## Boxplot per variabile per gruppo con media in punto bianco
par(mfrow=c(1,1))
for(i in colnames(q_olives)){
  gruppo_box_propriet(q_olives, gruppi_euclw, i, "Euclidea di Ward")
}
for(i in colnames(q_olives)){
  gruppo_box_propriet(q_olives, aggreg_kmeans3$cluster, i, "Kmeans - 3")
}
for(i in colnames(q_olives)){
  gruppo_box_propriet(q_olives, aggreg_kmeans9$cluster, i, "Kmeans - 9")
}


## Informazioni sulla presenze di aree e regioni in ogni gruppo
gruppo_plot_reg_area(perc_olives, gruppi_euclw, "Euclidea di Ward")
gruppo_plot_reg_area(perc_olives, aggreg_kmeans3$cluster,"Kmeans - 3")
gruppo_plot_reg_area(perc_olives, aggreg_kmeans9$cluster,"Kmeans - 9")



## Distribuzioni marginali
par(mfrow= c(1,1), bg = "#ffffff", mar = c(8,2.5,3,2.5), cex.lab =0.5)

barplot(table(perc_olives$Area), axes = FALSE, ylim=c(0,250) , xaxt='n', ann=FALSE)
abline(h = 0, lty = 2, col = "black")
abline(h = 50, lty = 2, col = "grey")
abline(h = 100, lty = 2, col = "grey")
abline(h = 150, lty = 2, col = "grey")
abline(h = 200, lty = 2, col = "grey")
abline(h = 250, lty = 2, col = "grey")
marg_regioni <- barplot(table(perc_olives$Area), las = 2, col = hcl.colors(9,palette = "ag_sunset"),ylim = c(0,250), add = TRUE )

text(marg_regioni, table(perc_olives$Area)+20, labels = table(perc_olives$Area))

title(main = "Distribuzioni Marginali Regioni ")


# Base barplot "Region"/Zona
barplot(table(perc_olives$Region), axes = FALSE, ylim=c(0,572), xaxt='n', ann=FALSE )
abline(h = 0, lty = 2, col = "black")
abline(h = 100, lty = 2, col = "grey")
abline(h = 200, lty = 2, col = "grey")
abline(h = 300, lty = 2, col = "grey")
abline(h = 400, lty = 2, col = "grey")
abline(h = 500, lty = 2, col = "grey")
tablR <- table(perc_olives$Region)
rownames(tablR) = c("Sud-Italia", "Sardegna","Nord-Italia")
marg_zone <-  barplot(tablR, col =hcl.colors(4,palette ="Sunset"),ylim = c(0,572), add = TRUE )
text(marg_zone, table(perc_olives$Region)+50, labels = table(perc_olives$Region))
title(main = "Distribuzioni Marginali Zone")

# Composizione percentuale dei gruppi

