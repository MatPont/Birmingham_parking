library(FactoMineR)

source('../Algorithms/util.R')

setwd("/Users/lucasrodriguespereira/Master/app-non-supervise/prj/Datasets")

data=read.csv('parking_dataset.csv', row.names = 1)

library(zoo)

data_norm<-norm_chi_2(norm_dataset(data))

n <- 18

test=apply(data_norm, 1, rollapply, n, mean, by = n)


y_chi2=KMedoids(data=data_norm, k=3, "wav")

res.PCA=PCA(data_norm)
plot.PCA(res.PCA, choix = "ind", col.ind = as.factor(y_chi2))

layout(1:3)
plot_charge_separate(norm_dataset(read.csv('parking_dataset.csv', row.names = 1)), y_chi2, "Occupation normalisée")
plot_charge_separate(t(apply(norm_dataset(data), 1, rollapply, n, mean, by = n)), y_chi2, "Occupation normalisée")


table(y_chi2)
