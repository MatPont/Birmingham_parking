setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(TSdist)
library(TSclust)
library(cluster)
library(NbClust)

source("util.R")
source("data_cleaning.R")
source("inertia.R")

#data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
#data <- data_cleaning(data)

week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)

norm_week_data <- norm_dataset(week_data)

chi_week_data <- norm_chi_2(norm_week_data)

#res_nbclust <- NbClust(norm_week_data, method = "kmeans", min.nc=2, max.nc=6, index="alllong")
res_nbclust <- NbClust(norm_week_data, method = "kmeans", min.nc=2, max.nc=6)





withinss <- c()
for(i in 2:10){
  print(i)
  x <- chi_week_data
  
  # res_spec <- specc(norm_week_data, i, nstart=50)
  # within <- sum(withinss(res_spec))
  
  clus.dwt = KMedoids(data=x, k=i, "fourier")
  #clus.dwt = KMedoids(data=x, k=i, "wav")
  y <- clus.dwt
  
  #res_kmeans <- kmeans(x, i, nstart = 100)
  #y <- res_kmeans$cluster
  #within <- res_kmeans$tot.withinss
  
  res <- cluster.inertia(x, y)
  within <- res$tot.within
  
  withinss <- c(withinss, within)
}
plot(withinss, type='b')

res_kmeans <- kmeans(norm_week_data, 3, nstart = 50)
res_kmeans <- kmeans(chi_week_data, 2, nstart = 50)
plot_charge(norm_week_data, res_kmeans$cluster)

clus.dwt = KMedoids(data=chi_week_data, k=3, "fourier")
clus.dwt = KMedoids(data=chi_week_data, k=3, "wav")
plot_charge(norm_week_data, clus.dwt)



