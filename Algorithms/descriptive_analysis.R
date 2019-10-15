setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data <- read.table("../Datasets/dataset.csv", sep=",", header = TRUE)

# Descriptive analysis
boxplot(data[, 2:4])
summary(data)
pairs(data)