setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(kohonen)

source("util.R")
source("inertia.R")


week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)

norm_week_data <- norm_dataset(week_data)

chi_week_data <- norm_chi_2(norm_week_data)

withins <- c()
for(i in 2:10){
  print(i)
  res_som = som(chi_week_data, grid = somgrid(i, i, "hexagonal"), rlen=500) 
  withins <- c(withins, mean(res_som$distances))
}
plot(withins, type="b")

#res_som = som(chi_week_data, grid = somgrid(10, 10, "rectangular"), rlen=500) 
res_som = som(norm_week_data, grid = somgrid(10, 10, "hexagonal"), rlen=500) 

res_som = som(chi_week_data, grid = somgrid(10, 10, "hexagonal"), rlen=1000)

# Basic plot
withins <- c()
for(i in 2:10){
  print(i)
  res_kmeans = kmeans(getCodes(res_som), i, nstart=50) 
  withins <- c(withins, res_kmeans$tot.withinss)
}
dev.off()
plot(withins, type="b")

res_kmeans <- kmeans(getCodes(res_som), 2)
plot(res_som, shape="straight", bgcol=c("steelblue1","sandybrown","yellowgreen", "gold", "plum1")[res_kmeans$cluster])
add.cluster.boundaries(res_som, res_kmeans$cluster, lwd = 3)

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
