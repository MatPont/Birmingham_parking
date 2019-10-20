setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(FactoMineR)
library(factoextra)

source("util.R")
source("data_cleaning.R")

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
data <- data_cleaning(data)

week_data <- make_week_dataset(data)
week_label <- make_week_label(data)
day_data <- make_day_dataset(data)
day_label <- make_day_label(data)
park_data <- make_parking_dataset(data)
park_label <- make_parking_label(data)

# Descriptive analysis
# boxplot(data[, 2:4])
# summary(data)
# pairs(data)

boxplot(day_data, main = "Boxplot pour chaque mesure de chaque jour", ylab = "Occupation")
boxplot(week_data, main = "Boxplot pour chaque mesure de chaque semaine", ylab = "Occupation")
boxplot(scale(t(park_data)), ylab = "Occupation")





resPCA <- PCA(week_data)
fviz_pca_ind(resPCA, col.ind = as.factor(week_label), label = "none", addEllipses = TRUE)

resPCA <- PCA(day_data)
fviz_pca_ind(resPCA, col.ind = as.factor(day_label), label = "none", addEllipses = TRUE)

resPCA <- PCA(park_data)
fviz_pca_ind(resPCA, col.ind = as.factor(park_label), label = "none", addEllipses = TRUE)
