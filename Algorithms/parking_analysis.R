setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(TSdist)
library(TSclust)
library(cluster)
library(NbClust)
library(mclust)
library(blockcluster)
library(kernlab)

source("util.R")
source("data_cleaning.R")
source("inertia.R")

#data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
#data <- data_cleaning(data)

park_data <- read.csv("../Datasets/parking_dataset.csv", row.names = 1)
park_label <- read.csv("../Datasets/parking_dataset_label.csv", row.names = 1)

norm_park_data <- norm_dataset(park_data)

chi_park_data <- norm_chi_2(norm_park_data)



#################################################
# Number of cluster
#################################################

#res_nbclust <- NbClust(norm_week_data, method = "kmeans", min.nc=2, max.nc=6, index="alllong")
res_nbclust <- NbClust(norm_park_data, method = "ward.D2", min.nc=2, max.nc=6)

index <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
res <- rep(0, length(index))
for(i in 1:length(index)){
  idx <- index[i]
  print(idx)
  try(res_nbclust <- NbClust(norm_park_data, method = "kmeans", min.nc=2, max.nc=6, index=idx))
  try(res[i] <- res_nbclust$Best.nc[1])
  try(res_nbclust$Best.nc[1] <- 0)
  print(res)
}
sort(table(res),decreasing=TRUE)[1:5]

# ward.D2
# 0 2 3 5 6 
# 9 7 5 4 4 

# kMeans
# 0 3 6 2 4 
# 9 8 5 4 3 



#################################################
# Mixture Model
#################################################

k <- 3
res <- Mclust(chi_park_data, G = k)
res <- Mclust(chi_park_data)
plot(res, what="BIC")
layout(1:k)
plot_charge_separate(norm_park_data, res$classification)



#################################################
# Coclustering
#################################################

criterion <- c()
idx <- c()
for(i in 1:10){
  print(i)
  res <- coclusterContinuous(chi_park_data, nbcocluster = c(3,i)) 
  criterion <- c(criterion, res["likelihood"])
}
dev.off()
plot(criterion, type="b")


k <- 3
clus.dwt = KMedoids(data=chi_park_data, k=k, "wav")
#res <- coclusterContingency(norm_park_data, nbcocluster = c(3,18), rowlabels = as.factor(clus.dwt))
res <- coclusterContinuous(chi_park_data, nbcocluster=c(3,3), semisupervised=TRUE, rowlabels=as.integer(clus.dwt), collabels=as.integer(rep(-1, dim(chi_park_data)[2])))
res <- coclusterContinuous(chi_park_data, nbcocluster=c(3,3))
summary(as.factor(res["colclass"]))
plot(res)

for(yi in unique(res["colclass"])){
  cat("=======", yi, "\n")

  print("=== heures")
  print(which(res["colclass"] == yi) %% 18)
  print("=== jours")
  print(as.integer(which(res["colclass"] == 2) / 1386 * 77))
  print("=== semaines")
  print(as.integer(which(res["colclass"] == 2) / 1386 * 11))
}



layout(1:k)
plot_charge_separate(norm_park_data, res["rowclass"])
plot_charge_separate(norm_park_data, clus.dwt)



#################################################
# Spectral Clustering
#################################################

k <- 3
res_spec <- specc(chi_park_data, k, nstart=50)

layout(1:k)
plot_charge_separate(norm_park_data, res_spec)


#################################################
# Elbow criterion
#################################################

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
  print(dim(res_kmeans$centers))
  
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





#################################################
# Map
#################################################
# Load dataset
coord <- read.csv("../Datasets/full_dataset.csv")[,c(1,16)]
park_names <- read.csv("../Datasets/parking_dataset_label.csv")[,2]

# Get coordinates
temp <- c()
for(park in park_names){
  coordinates <- jsonlite::fromJSON(as.character(coord[coord[,1] == park, 2]))$coordinates
  if(coordinates[1] > -6){
    temp <- rbind(temp, c(coordinates[2], coordinates[1]))
    rownames(temp)[dim(temp)[1]] <- park 
  }
}

# Clustering
park_data <- read.csv("../Datasets/parking_dataset.csv", row.names = 1)
park_label <- read.csv("../Datasets/parking_dataset_label.csv", row.names = 1)
norm_park_data <- norm_dataset(park_data)
chi_park_data <- norm_chi_2(norm_park_data)
n <- 18
n_chi_park_data=t(apply(chi_park_data, 1, rollapply, n, mean, by = n))

k <- 3
clus.dwt = KMedoids(data=n_chi_park_data, k=k, "wav")

label <- c()
for(park_name in rownames(temp)){
  print(park_name)
  lab <- clus.dwt[park_names == park_name]
  print(lab)
  label <- c(label, lab)
}

# Plot
plot(temp[,2], temp[,1], pch = 19, col=label)

library("OpenStreetMap")
birmingham <- c(52.489471, -1.898575)
gap <- 0.02

layout(matrix(1:2, nrow=1))
map <-openmap(upperLeft = birmingham-gap, lowerRight = birmingham+gap, type="osm")
autoplot.OpenStreetMap(map)
plot(temp[,2], temp[,1], pch = 19, col=label, xlim = c(birmingham[2]-gap, birmingham[2]+gap), ylim = c(birmingham[1]-gap, birmingham[1]+gap))

