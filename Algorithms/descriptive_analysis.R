setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("data_cleaning.R")

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)
data <- data_cleaning(data)

# Descriptive analysis
boxplot(data[, 2:4])
summary(data)
pairs(data)