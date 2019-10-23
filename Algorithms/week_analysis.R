setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(TSdist)
library(TSclust)
library(cluster)
library(NbClust)

source("util.R")
source("data_cleaning.R")

#data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
#data <- data_cleaning(data)

week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)

norm_week_data <- norm_dataset(week_data)

chi_week_data <- norm_chi_2(norm_week_data)


NbClust(chi_week_data, method = "centroid")


withinss <- c()
for(i in 2:10){
  print(i)
  # res_spec <- specc(norm_week_data, i, nstart=50)
  # withinss <- c(withinss, sum(withinss(res_spec)))
  res_kmeans <- kmeans(chi_week_data, i, nstart = 100)
  withinss <- c(withinss, res_kmeans$tot.withinss)
}
plot(withinss, type='b')

res_kmeans <- kmeans(chi_week_data, 2, nstart = 50)
plot_charge(norm_week_data, res_kmeans$cluster)

clus.dwt = KMedoids(data=chi_week_data, k=3, "fourier")
clus.dwt = KMedoids(data=chi_week_data, k=3, "wav")
plot_charge(norm_week_data, clus.dwt)

