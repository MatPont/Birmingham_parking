setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(FactoMineR)
library(factoextra)

source("util.R")
source("data_cleaning.R")

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
data <- data_cleaning(data)

week_data <- make_week_dataset(data)
week_label <- make_week_label(data)

res_kmeans <- kmeans(week_data, 5, nstart = 50)

resPCA <- PCA(week_data)
fviz_pca_ind(resPCA, col.ind = as.factor(res_kmeans$cluster), label = "none", addEllipses = TRUE)
fviz_pca_ind(resPCA, col.ind = as.factor(week_label), label = "none", addEllipses = TRUE)
