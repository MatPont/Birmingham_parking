setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(kohonen)
library(mclust)

source("util.R")
source("inertia.R")





########################################################
# Util Functions
########################################################

# Combine som clustering and a clustering on som to produce final clustering
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





########################################################
# Week Analysis
########################################################

week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)

norm_week_data <- norm_dataset(week_data)

chi_week_data <- norm_chi_2(norm_week_data)

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

plot(res_som,type="counts", palette.name = degrade.bleu, shape="straight") 
add.cluster.boundaries(res_som, label, lwd = 4) 

dev.off()
layout(matrix(c(1,4,2,3), nrow=2))
label <- convert_cluster(res_som$unit.classif, res_kmeans$cluster)
plot_charge_week(norm_week_data, label, "Occupation normalisée")

for(y in unique(label)){
  print("======================")
  print(unique(week_label[label == y,]))
  print(length(unique(week_label[label == y,])))
  print(length(week_label[label == y,]))
}





########################################################
# Parking Analysis
########################################################

park_data <- read.csv("../Datasets/parking_dataset.csv", row.names = 1)
park_label <- read.csv("../Datasets/parking_dataset_label.csv", row.names = 1)

norm_park_data <- norm_dataset(park_data)

chi_park_data <- norm_chi_2(norm_park_data)

size <- 5
k <- 3
seed <- 7

set.seed(seed)
res_som = som(chi_park_data, grid = somgrid(size, size, "hexagonal"), rlen=1000)

res_kmeans <- kmeans(getCodes(res_som), k, nstart=50)
label <- res_kmeans$cluster

plot(res_som, shape="straight", bgcol=MYCOLOR[label])
add.cluster.boundaries(res_som, label, lwd = 3) 

dev.off()
layout(matrix(c(1,4,2,3), nrow=2))
label <- convert_cluster(res_som$unit.classif, res_kmeans$cluster)
plot_charge_week(norm_week_data, label, "Occupation normalisée")

for(y in unique(label)){
  print("======================")
  print(unique(week_label[label == y,]))
  print(length(unique(week_label[label == y,])))
  print(length(week_label[label == y,]))
}





########################################################
# Other
########################################################
# withins <- c()
# for(i in 2:10){
#   print(i)
#   res_som = som(chi_week_data, grid = somgrid(i, i, "hexagonal"), rlen=500) 
#   withins <- c(withins, mean(res_som$distances))
# }
# plot(withins, type="b")

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
plot(res_som,type="counts", palette.name = degrade.bleu, shape="straight") 
plot(res_som,type="dist.neighbours")
plot(res_som,type="mapping")
plot(res_som,type="property")
plot(res_som,type="quality")  # qualité de représentation

# Component planes
par(mfrow=c(2,2))
for (i in 1:4){
  plot(res_som, type="property", property=res_som$codes[,i], main=colnames(res_som$codes)[i])
}
