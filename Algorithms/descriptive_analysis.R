setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(FactoMineR)
library(factoextra)

source("util.R")
source("data_cleaning.R")

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
data <- data_cleaning(data)
park_names <- unique(data[,1])
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



########################################################
# Normalization
########################################################
norm_day_data <- norm_mat(day_data)
norm_week_data <- norm_mat(week_data)
norm_park_data <- norm_mat(park_data)

capacities <- unlist(read.csv("../Datasets/capacities.csv", row.names = 1)) #as.integer(unique(data[,2]))
norm_day_data <- day_data / rep(capacities, rep(dim(day_data)[1]/length(capacities), length(capacities)))
norm_week_data <- week_data / rep(capacities, rep(dim(week_data)[1]/length(capacities), length(capacities)))
norm_park_data <- park_data / rep(capacities, rep(dim(park_data)[1]/length(capacities), length(capacities)))



########################################################
# Box plot
########################################################
boxplot(norm_day_data, ylab="Occupation normalisée", xaxt="n", xlab="Heure")
axis(1, at=seq(1,18,2), labels=8:16)
boxplot(norm_week_data, ylab="Occupation normalisée", xaxt="n", xlab="Jour")
axis(1, at=seq(1,126,18), labels=c("Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Lundi"), )
boxplot(t(norm_park_data), ylab = "Occupation")
boxplot(t(park_data), ylab = "Occupation", )


corrplot::corrplot(cor(t(park_data)), type= "upper")



########################################################
# Principal Component Analysis
########################################################
resPCA <- PCA(week_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(week_label)), label = "none", addEllipses = TRUE)

resPCA <- PCA(day_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(day_label)), label = "none", addEllipses = TRUE)

resPCA <- PCA(park_data)
fviz_pca_ind(resPCA, col.ind = as.factor(unlist(park_label)), label = "none", addEllipses = TRUE)



########################################################
# Correspondance Analysis
########################################################
res_CA <- CA(park_data)

#saveRDS(res_CA, file = "CA.rds")

#res_CA <- readRDS(file = "CA.rds")

#fviz_ca_row(res_CA, label = "none", col.row = labelK, addEllipses = TRUE, xlim=c(-0.5, 0.5), ylim=c(-1.5, 1.5))
fviz_ca_row(res_CA, label = "none", col.row = authors, addEllipses = TRUE, xlim=c(-0.5, 0.5), ylim=c(-1.5, 1.5))