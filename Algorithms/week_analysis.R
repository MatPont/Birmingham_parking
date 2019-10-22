setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(TSdist)
library(TSclust)
library(cluster)

source("util.R")
source("data_cleaning.R")

plot_charge <- function(x, cluster){
  label <- unique(cluster)
  plot(colMeans(x[cluster == label[1],]), type='l', col=1, lty=1, ylim = c(0,1))
  for(i in 2:length(label)){
    lines(colMeans(x[cluster == label[i],]), type='l', col=i, lty=i, ylim = c(0,1)) 
  } 
  legend("topleft",legend=1:length(label), lty=length(label), col=1:length(label))
}

#data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
#data <- data_cleaning(data)

week_data <- make_week_dataset(data)
week_label <- make_week_label(data)
norm_week_data <- norm_dataset(week_data)


withinss <- c()
for(i in 2:10){
  print(i)
  # res_spec <- specc(norm_week_data, i, nstart=50)
  # withinss <- c(withinss, sum(withinss(res_spec)))
  res_kmeans <- kmeans(norm_week_data, i, nstart = 50)
  withinss <- c(withinss, res_kmeans$tot.withinss)
}
plot(withinss, type='b')

res_kmeans <- kmeans(norm_week_data, 4, nstart = 50)
plot_charge(norm_week_data, res_kmeans$cluster)

clus.dwt = KMedoids(data=week_data, k=4, "wav")
plot_charge(norm_week_data, clus.dwt)
