library(TSdist)
library(TSclust)
library(cluster)

source('../Algorithms/inertia.R')
source('../Algorithms/util.R')

setwd("/Users/lucasrodriguespereira/Master/app-non-supervise/prj/Datasets")

weekdf=read.csv('week_dataset.csv', row.names = 1)

###############################
####### standard normalization
#weekdf=norm_dataset(weekdf)

###############################
###### Testing with Chi2 normalization
weekdf=norm_chi_2(weekdf)



###### ELBOW CRITERION  ######

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



### PLOT RESULT #####

layout(1:3)
plot_charge_week(weekdf, KMedoids(data=weekdf, k=3, "wav"), "Occupation normalisée")

weekdf_labels=read.csv('week_dataset_label.csv', row.names = 1)

