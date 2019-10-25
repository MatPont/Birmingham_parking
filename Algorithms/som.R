setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(kohonen)
library(mclust)

source("util.R")
source("inertia.R")


week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)

norm_week_data <- norm_dataset(week_data)

chi_week_data <- norm_chi_2(norm_week_data)

# withins <- c()
# for(i in 2:10){
#   print(i)
#   res_som = som(chi_week_data, grid = somgrid(i, i, "hexagonal"), rlen=500) 
#   withins <- c(withins, mean(res_som$distances))
# }
# plot(withins, type="b")

#res_som = som(chi_week_data, grid = somgrid(10, 10, "rectangular"), rlen=500) 
#res_som = som(norm_week_data, grid = somgrid(10, 10, "hexagonal"), rlen=500) 



size <- 10
k <- 4
seed <- 11

set.seed(seed)
res_som = som(chi_week_data, grid = somgrid(size, size, "hexagonal"), rlen=1000)

res_kmeans <- kmeans(getCodes(res_som), k, nstart=50)
label <- res_kmeans$cluster

plot(res_som, shape="straight", bgcol=MYCOLOR[label])
add.cluster.boundaries(res_som, label, lwd = 3) 

convert_cluster <- function(som_cluster, cluster){
  for(i in 1:length(som_cluster)){
    for(y in unique(cluster)){
      if(som_cluster[i] %in% which(cluster == y)){
        som_cluster[i] <- y
        break
      }
    }
  }
  return(som_cluster)
}

layout(matrix(1:4, nrow=2))
plot_charge_week(norm_week_data, convert_cluster(res_som$unit.classif, res_kmeans$cluster), "Occupation normalisée")

# Basic plot
# withins <- c()
# for(i in 2:10){
#   print(i)
#   res_kmeans = kmeans(getCodes(res_som), i, nstart=50) 
#   withins <- c(withins, res_kmeans$tot.withinss)
# }
# dev.off()
# plot(withins, type="b")











# U-matrix (matrice de voisinage)
plot(res_som, type="dist.neighbours", palette.name = coolBlueHotRed)

# Count plot
plot(res_som, type="count", palette.name=degrade.bleu)


res_som$codes # coordonnees des centres (espace d’entrée)
res_som$distances # distances entre chaque donnée et son centre
res_som$changes # ecart moyen entre les donnees et les centres

plot(res_som,type="codes")
plot(res_som,type="changes")
plot(res_som,type="counts") # qualité de représentation
plot(res_som,type="dist.neighbours")
plot(res_som,type="mapping")
plot(res_som,type="property")
plot(res_som,type="quality")  # qualité de représentation

# Component planes
par(mfrow=c(2,2))
for (i in 1:4){
  plot(res_som, type="property", property=res_som$codes[,i], main=colnames(res_som$codes)[i])
}
