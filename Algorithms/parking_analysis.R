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

park_data <- read.csv("../Datasets/parking_dataset.csv", row.names = 1)
park_label <- read.csv("../Datasets/parking_dataset_label.csv", row.names = 1)

norm_park_data <- norm_dataset(park_data)

chi_park_data <- norm_chi_2(norm_park_data)

#res_nbclust <- NbClust(norm_week_data, method = "kmeans", min.nc=2, max.nc=6, index="alllong")
res_nbclust <- NbClust(norm_park_data, method = "ward.D2", min.nc=2, max.nc=6)





withinss <- c()
x <- chi_park_data
for(i in 2:10){
  print(i)
  
  #clus.dwt = KMedoids(data=x, k=i, "fourier")
  #clus.dwt = KMedoids(data=x, k=i, "wav")
  #y <- clus.dwt
  
  res_kmeans <- kmeans(x, i, nstart = 100)
  y <- res_kmeans$cluster
  within <- res_kmeans$tot.withinss
  
  res <- cluster.inertia(x, y)
  #within <- res$tot.within
  
  withinss <- c(withinss, within)
}
plot(withinss, type='b')

k <- 4
res_kmeans <- kmeans(chi_park_data, k, nstart = 50)
layout(1:k)
plot_charge_separate(norm_park_data, res_kmeans$cluster)

k <- 3
clus.dwt = KMedoids(data=chi_park_data, k=k, "fourier")
clus.dwt = KMedoids(data=chi_park_data, k=k, "wav")
layout(1:k)
plot_charge_separate(norm_park_data, clus.dwt)








library(mclust)
res <- Mclust(chi_week_data, G = 3:3)
plot(res, what="BIC")
plot_charge(norm_week_data, res$classification)


