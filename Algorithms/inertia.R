#####################################
####### INERTIA #####################

inertia_class=function(x) {
  wk=0
  x_bar=colMeans(x)
  for(i in 1:dim(x)[1]) {
    wk = wk + t(as.matrix(x[i,]-x_bar))%*%as.matrix(x[i,]-x_bar)
  }
  return(list(centroid = x_bar, inertia = as.numeric(wk)))
}

cluster.inertia=function(x,y) {
  classes=unique(y)
  centroids=c()
  inertia=c()
  
  #Inertia within classes
  for(k in classes) {
    x_class=as.matrix(x[y == k,])
    a=inertia_class(x_class)
    centroids=rbind(centroids, a$centroid)
    inertia=rbind(inertia, a$inertia)
  }
  
  #Intertia between classes
  b=inertia_class(centroids)$inertia
  w=sum(inertia)
  total=sum(w,b)
  return(list(tot.within=w, within=inertia, centroids=centroids, between=b, total=total))
}

#### Test

setwd("/Users/lucasrodriguespereira/Master/app-non-supervise/prj/Datasets")
norm_dataset <- function(park_data, capacities=unlist(read.csv("capacities.csv", row.names = 1))){
 return(park_data / rep(capacities, rep(dim(park_data)[1]/length(capacities), length(capacities))))
}
weekdf=read.csv('week_dataset.csv', row.names = 1)
weekdf=norm_dataset(weekdf)
x=weekdf
result=kmeans(x,3)
y=result$cluster
inertie=cluster.inertia(x,y)
inertie
