library(TSdist)
library(TSclust)
library(cluster)
library(NbClust)

source('../Algorithms/inertia.R')
source('../Algorithms/util.R')

setwd("/Users/lucasrodriguespereira/Master/app-non-supervise/prj/Datasets")

weekdf=read.csv('week_dataset.csv', row.names = 1)

###############################
####### standard normalization
#weekdf=norm_dataset(weekdf)

###############################
###### Testing with Chi2 normalization
#weekdf=norm_chi_2(weekdf)



###### ELBOW CRITERION  ######
layout(1:2)
total_inertia_per_class_kmedoids=c()
for(k_classes in 2:10) {
  y = KMedoids(data=weekdf, k=k_classes, "wav")
  x = weekdf
  inertia=cluster.inertia(x,y)
  total_inertia_per_class_kmedoids=rbind(total_inertia_per_class_kmedoids,inertia$tot.within)
}

plot(total_inertia_per_class_kmedoids, type='b', ylab='Inertie within', xlab='Nombre de classes', main='Elbow criterion (Kmedoids)')

total_inertia_per_class_kmeans=c()
for(k_classes in 2:10) {
  y = kmeans(weekdf,k_classes)$cluster
  x = weekdf
  inertia=cluster.inertia(x,y)
  total_inertia_per_class_kmeans=rbind(total_inertia_per_class_kmeans,inertia$tot.within)
}

plot(total_inertia_per_class_kmeans, type='b', ylab='Inertie within', xlab='Nombre de classes', main='Elbow criterion (Kmeans)')



### PLOT RESULT FOR 3 CLASSES#####
dev.off()
layout(1:3)
y_norm=KMedoids(data=norm_dataset(read.csv('week_dataset.csv', row.names = 1)), k=3, "wav")
y_chi2=KMedoids(data=norm_chi_2(read.csv('week_dataset.csv', row.names = 1)), k=3, "wav")
plot_charge_week(norm_dataset(weekdf), y_chi2, "Occupation normalisée")


#################################
#### Parking analysis  ##########
#################################

## Finding the number of clusters
X=norm_dataset(read.csv('parking_dataset.csv', row.names = 1))
res.NbClust.kmeans=NbClust(t(X), method="kmeans", min.nc=2, max.nc=8)
res.NbClust.single=NbClust(X, distance="euclidean", method="single",min.nc=2, max.nc=8, index = "all")
res.NbClust.complete=NbClust(X, distance="euclidean", method="complete",min.nc=2, max.nc=5, index = "all")
res.NbClust.ward=NbClust(X, distance="euclidean", method="ward.D",min.nc=2, max.nc=8, index = "all")
res.NbClust.average=NbClust(X, distance="euclidean", method="average",min.nc=2, max.nc=8, index = "all")


layout(1:3)
y_norm=KMedoids(data=norm_dataset(read.csv('parking_dataset.csv', row.names = 1)), k=3, "wav")
plot_charge_week(norm_dataset(read.csv('parking_dataset.csv', row.names = 1)), y_norm, "Occupation normalisée")

y_chi2=KMedoids(data=norm_chi_2(read.csv('parking_dataset.csv', row.names = 1)), k=3, "wav")
plot_charge_week(norm_dataset(read.csv('parking_dataset.csv', row.names = 1)), y_chi2, "Occupation normalisée")



