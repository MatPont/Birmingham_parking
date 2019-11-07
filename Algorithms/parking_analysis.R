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

n_chi_park_data <- cut_dataset(chi_park_data)
n_norm_park_data <- cut_dataset(norm_park_data)

data <- n_chi_park_data
data <- chi_park_data

k <- 3
clus.dwt = KMedoids(data=data, k=k, "wav")
#res <- coclusterContingency(norm_park_data, nbcocluster = c(3,18), rowlabels = as.factor(clus.dwt))
res <- coclusterContinuous(data, nbcocluster=c(3,3), semisupervised=TRUE, rowlabels=as.integer(clus.dwt), collabels=as.integer(rep(-1, dim(chi_park_data)[2])))
res <- coclusterContinuous(data, nbcocluster=c(3,3))
summary(as.factor(res["colclass"]))
plot(res)

for(yi in unique(res["colclass"])){
  cat("=======", yi, "\n")

  print("=== heures")
  temp <- which(res["colclass"] == yi) %% 18
  print(temp)
  print(mean(temp))
  print(max(temp))
  print("=== jours")
  temp <- as.integer(round(which(res["colclass"] == yi) / 1386 * 77))
  print(temp)
  print(mean(temp))
  print(max(temp))
  print("=== semaines")
  temp <- as.integer(round(which(res["colclass"] == yi) / 1386 * 11))
  print(temp)
  print(mean(temp))
  print(max(temp))
}



layout(1:k)
plot_charge_separate(n_norm_park_data, res["rowclass"])
plot_charge_separate(n_norm_park_data, clus.dwt)



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
n_norm_park_data=t(apply(norm_park_data, 1, rollapply, n, mean, by = n))

k <- 2
clus.dwt = KMedoids(data=n_chi_park_data, k=k, "wav")

label <- c()
for(park_name in rownames(temp)){
  print(park_name)
  lab <- clus.dwt[park_names == park_name]
  print(lab)
  label <- c(label, lab)
}

# Plot
birmingham <- c(52.489471, -1.898575)
gap <- 0.001
upperLeft <- c(max(temp[,1])+gap, min(temp[,2])-gap)
lowerRight <- c(min(temp[,1])-gap, max(temp[,2])+gap)

plot(temp[,2], temp[,1], pch = 19, col=label, xlim = c(min(temp[,2]), max(temp[,2])), ylim = c(min(temp[,1]), max(temp[,1])))

library("ggmap")
map <- get_map(c(left=upperLeft[2], bottom=lowerRight[1], right=lowerRight[2], top=upperLeft[1]), maptype = "satellite", source="google")
ggmap(map) +
  geom_point(data = as.data.frame(temp), aes(x = temp[,2], y = temp[,1], size = 2), colour = label, show.legend=F) +
  geom_point(data = as.data.frame(temp), aes(x = temp[,2], y = temp[,1], size = 1), colour = "white", show.legend=F) +
  theme_bw()


library("OpenStreetMap")
map <-openmap(upperLeft = upperLeft, lowerRight = lowerRight, type="osm")
autoplot.OpenStreetMap(map)

