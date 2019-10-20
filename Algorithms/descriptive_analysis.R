setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(FactoMineR)
library(factoextra)

source("util.R")
source("data_cleaning.R")

# data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
# data <- data_cleaning(data)
# 
# week_data <- make_week_dataset(data)
# week_label <- make_week_label(data)
# day_data <- make_day_dataset(data)
# day_label <- make_day_label(data)
# park_data <- make_parking_dataset(data)
# park_label <- make_parking_label(data)

week_data <- read.csv("../Datasets/week_dataset.csv", row.names = 1)
week_label <- read.csv("../Datasets/week_dataset_label.csv", row.names = 1)
day_data <- read.csv("../Datasets/day_dataset.csv", row.names = 1)
day_label <- read.csv("../Datasets/day_dataset_label.csv", row.names = 1)
park_data <- read.csv("../Datasets/parking_dataset.csv", row.names = 1)
park_label <- read.csv("../Datasets/parking_dataset_label.csv", row.names = 1)

# Descriptive analysis
# boxplot(data[, 2:4])
# summary(data)
# pairs(data)

norm_day_data <- norm_mat(day_data)
norm_week_data <- norm_mat(week_data)
norm_park_data <- norm_mat(park_data)

boxplot(norm_day_data, ylab="Occupation normalisée", xaxt="n", xlab="Heure")
axis(1, at=seq(1,18,2), labels=8:16)
boxplot(norm_week_data, ylab="Occupation normalisée", xaxt="n", xlab="Jour")
axis(1, at=seq(1,126,18), labels=c("Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Lundi"), )
boxplot(t(norm_park_data), ylab = "Occupation")
boxplot(t(park_data), ylab = "Occupation", )


corrplot::corrplot(cor(t(park_data)), type= "upper")


resPCA <- PCA(week_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(week_label)), label = "none", addEllipses = TRUE)

resPCA <- PCA(day_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(day_label)), label = "none", addEllipses = TRUE)

resPCA <- PCA(park_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(park_label)), label = "none", addEllipses = TRUE)
