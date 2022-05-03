# Cluster generica + colorazione sulla matrice delle distanze mostrata delle variabili che si trovano nella stessa classe dato il metodo
# Commento generico sulla parte analizzata.


# Includo la libreria
library(classifly)

dist_image <- function(matr){
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
# Inizialmente creo una matrice della distanza utilizzando 1-rho^2
matr_dist_corr <- 1-cor(q_olives)^2
matr_corr_var <- cor(q_olives)
#COMMENTARE etc. etc.
image(matr_corr_var[,c(8:1)], axes = FALSE,)

#Per la cluster scelgo il metodo di ward
aggregazione_var <- hclust(as.dist(matr_dist_corr),method = "ward.D" )
par(mar = c(2.5,2.5,2.5,2)+0.1)
plot(aggregazione_var, hang= -0.1, frame.plot= TRUE, main="Aggregazione variabili",sub = "Distanza utilizzata: 1-rho^2 ; Metodo di aggregazione: Ward", xlab ="", ylab="Indice di aggregazione")
num_classi = 2
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

par(mfrow=c(2,2))
dist_image(ed1_o)
dist_image(edM_o)
dist_image(edm_o)

st_q_olives = scale(q_olives)
# Osservando le tre possibili distance, sembra che la distanza euclidea sia quella che prioritizza la differenziazione per regione. Questo vuol dire che c'è un elemento (una coordinata) degli oli che è più caratterizzata dalla regione di altre - infatti la distanza del massimo non la evidenzia mentre la manhattan si (che è una versione meno "geometrica" della distanza euclidea).
# Guardo quindi cosa devi per regione dal baricentro:
baric_reg1 <-  colMeans(st_q_olives[perc_olives$Region == 1,])# Sum somma i valori True, ovvero conta le righe prese in considerazione. Il resto è per i calcoli dei baricentri 
baric_reg2 <-   colMeans(st_q_olives[perc_olives$Region == 2,])
baric_reg3 <- colMeans(st_q_olives[perc_olives$Region == 3,])

dist_reg_non_cluster <-matrix(c( baric_reg1 , baric_reg2 , baric_reg3 ), ncol = 8, byrow = TRUE) # Sono standardizzate, quindi il baricentro sarebbe 0
colnames(dist_reg_non_cluster) <-  colnames(q_olives)
rownames(dist_reg_non_cluster) <-  c("Sud Italia", "Nord Italia", "Sardegna")
# TABELLA BELLA
View(dist_reg_non_cluster)
par(mfrow = c(1,1))
barplot(dist_reg_non_cluster[1,], las = 2,ylim = c(-2,2), main = "Deviazione baricentro Sud Italia", col = yellow_rgber(dist_reg_non_cluster[1,],2))
barplot(dist_reg_non_cluster[2,], las = 2, ylim = c(-2,2), main = "Deviazione baricentro Nord Italia", col = yellow_rgber(dist_reg_non_cluster[2,],2))
barplot(dist_reg_non_cluster[3,], las = 2, ylim = c(-2,2), main = "Deviazione baricentro Sardegna", col = yellow_rgber(dist_reg_non_cluster[3,],2))
# Sembra che in tutta italia sia l'acido oleico

#@ Studio la matrice ed1_o (i.e: euclidean distance ^ 1 _ olives)
# Già dalla matrice delle distanze si può osservare una forte similitudine tra oli della stessa regione. La puglia del nord sembra più uniformata all'Umbria e alla Liguria che all'Italia del Sud, quindi è supponibile che verrà aggregata durante la cluster.
# Si possono inoltre intersecare le coordinate per osservare quali siano le caselle più chiare, come nel caso di Umbria e ovest della liguria. La Sardegna ha un simile comportamento rendendo la divisione tra costa e entroterra più qualitativo che necessario. 
# Infine è interessante osservare che il nord della puglia abbia una distanza mediamente alta con il sud della puglia, mentre sia piò intersecabile con regioni come la sicilia o l'ovest ligure.

#La frazione più compatta (distanza interna) sembrano essere l'umbria e la Sardegna, invece Sicilia e Ovest della Liguria sono quelle con i punti più scuri

# L'analisi appena fatta è un tipo possibile di cluster, meno quantitativo e basato solo sulla supposizione che regioni affini abbiano alberi dello stesso tipo, quindi oli con grassi simili.

## Cluster quantitativo

