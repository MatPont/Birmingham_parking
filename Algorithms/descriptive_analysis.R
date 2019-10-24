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
norm_week_data <- norm_dataset(week_data)
norm_day_data <- norm_dataset(day_data)
norm_park_data <- norm_dataset(park_data)

chi_week_data <- norm_chi_2(norm_week_data)
chi_day_data <- norm_chi_2(norm_day_data)
chi_park_data <- norm_chi_2(norm_park_data)


########################################################
# Box plot
########################################################
boxplot(norm_day_data, ylab="Occupation normalisée", xaxt="n", xlab="Heure")
axis(1, at=seq(1,18,2), labels=8:16)
boxplot(norm_week_data, ylab="Occupation normalisée", xaxt="n", xlab="Jour")
axis(1, at=seq(1,126,18), labels=c("Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche", "Lundi"), )
boxplot(t(norm_park_data), ylab = "Occupation")
boxplot(t(park_data), ylab = "Occupation", )
  
layout(1:2)

corrplot::corrplot(cor(t(park_data)), type= "upper", order = "hclust")
corrplot::corrplot(cor(t(norm_park_data)), type= "upper", order = "hclust")

t_norm_park_data <- t(norm_park_data)
median_order <- order(apply(t_norm_park_data, MARGIN=2, FUN=median))

par(mar=c(10,5,3,3)) # left margin space
boxplot(t_norm_park_data[, median_order], ylab="Occupation normalisée", names=park_names[median_order], las=2)

par(mar=c(2,5,2,2)) # left margin space
barplot(capacities, las=2, xaxt="n")



t_park_data <- t(park_data)
barplot(apply(t_park_data, MARGIN=2, FUN=var))


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
row.names(chi_park_data) <- park_names

res_CA <- CA(norm_park_data)
res_CA <- CA(chi_park_data)
res_CA <- CA(chi_day_data)
res_CA <- CA(chi_week_data)


#saveRDS(res_CA, file = "CA.rds")

#res_CA <- readRDS(file = "CA.rds")

#fviz_ca_row(res_CA, label = "none", col.row = labelK, addEllipses = TRUE, xlim=c(-0.5, 0.5), ylim=c(-1.5, 1.5))
day_park_names <- rep(park_names, rep(dim(day_data)[1]/length(park_names), length(park_names)))
week_park_names <- rep(park_names, rep(dim(week_data)[1]/length(park_names), length(park_names)))

fviz_ca_row(res_CA, col.row = as.factor(park_names))
fviz_ca_col(res_CA)

fviz_ca_row(res_CA, label = "none", col.row = as.factor(day_park_names), addEllipses = TRUE)
fviz_ca_row(res_CA, label = "none", col.row = as.factor(week_park_names), addEllipses = TRUE)
